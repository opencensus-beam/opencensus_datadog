%% @author Łukasz Niemier <lukasz+opensource@niemier.pl>
%%
%% @doc Trace reporter for DataDog ([https://datadog.com]).
%%
%% == Configuration ==
%%
%% <ul>
%%  <li>`host' - address where DataDog Agent lives (default `"localhost"')</li>
%%  <li>`port' - port on which Agent listens for traces (default `8126')</li>
%%  <li>`service' - service name (default `opencensus-app')</li>
%% </ul>
-module(oc_reporter_datadog).

-behaviour(oc_reporter).

%% API exports
-export([init/1, report/2]).

-include_lib("opencensus/include/opencensus.hrl").

-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Format, Data), error_logger:error_msg(Format, Data)).
-endif.

-define(TRACER_VERSION, "OC/0.1.0").

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8126).
-define(DEFAULT_SERVICE, <<"opencensus-app">>).
-define(DEFAULT_TYPE, <<"custom">>).

-spec init(term()) -> oc_reporter:opts().
init(Options) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    Service = proplists:get_value(service, Options, ?DEFAULT_SERVICE),
    Type = proplists:get_value(type, Options, ?DEFAULT_TYPE),
    Client = proplists:get_value(http_client, Options, fun default_client/3),
    #{host => Host,
      port => Port,
      service => Service,
      type => Type,
      client => Client}.

-spec report(nonempty_list(opencensus:span()), oc_reporter:opts()) -> ok.
report(Spans, #{
         service := Service,
         host := Host,
         port := Port,
         type := Type,
         client := Client}) ->
    Sorted = lists:sort(fun(A, B) ->
                                A#span.trace_id =< B#span.trace_id
                        end, Spans),
    Grouped = group(Sorted),
    DSpans = [[build_span(S, Service, Type) || S <- Trace] || Trace <- Grouped],

    try jsx:encode(DSpans) of
        JSON ->
            Address = io_lib:format('http://~s:~B/v0.3/traces', [Host, Port]),
            Headers = [
                       {"Datadog-Meta-Lang", "erlang"},
                       {"Datadog-Meta-Lang-Version", lang_version()},
                       {"Datadog-Meta-Lang-Interpreter", interpreter_version()},
                       {"Datadog-Meta-Tracer-Version", ?TRACER_VERSION}
                      ],
            case Client(Address, Headers, JSON) of
                ok -> ok;
                {error, {http_error, Code, Message}} ->
                    ?LOG_ERROR("DD: Unable to send spans,"
                               " DD reported an error: ~p: ~p",
                              [Code, Message]);
                {error, Reason} ->
                    ?LOG_ERROR("DD: Unable to send spans, client error: ~p",
                               [Reason])
            end
    catch
        error:Error ->
            ?LOG_ERROR("DD: Can't spans encode to json: ~p", [Error])
    end.

default_client(Address, Headers, JSON) ->
    case httpc:request(
           put,
           {Address, Headers, "application/json", JSON},
           [],
           []
          ) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200, Code =< 299 ->
            ok;
        {ok, {{_, Code, _}, _, Body}} ->
            {error, {http_error, Code, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

lang_version() ->
    erlang:system_info(otp_release).

interpreter_version() ->
    io_lib:format('~s-~s', [erlang:system_info(version),
                            erlang:system_info(system_architecture)]).

group([]) -> [];
group([First | Spans]) -> group(Spans, [[First]]).

group([], Acc) -> Acc;
group([#span{trace_id = Id} = Span | Spans],
      [[#span{trace_id = Id} | _] = Curr | Acc]) ->
    group(Spans, [[Span | Curr] | Acc]);
group([Span | Spans], Acc) -> group(Spans, [[Span] | Acc]).

build_span(Span, Service, Type) ->
    (optional_fields(Span))#{
      <<"trace_id">> => Span#span.trace_id rem 16#ffffffff,
      <<"span_id">> => Span#span.span_id,
      <<"name">> => Span#span.name,
      <<"resource">> => Span#span.name,
      <<"service">> => to_tag(Service),
      <<"start">> => wts:to_absolute(Span#span.start_time) * 1000,
      <<"duration">> =>
        wts:duration(Span#span.start_time, Span#span.end_time) * 1000,
      <<"type">> => to_tag(Type),
      <<"meta">> => to_meta(Span#span.attributes)}.

to_meta(Attributes) -> maps:map(fun to_meta/2, Attributes).

to_meta(_Name, Value) when is_integer(Value) -> integer_to_binary(Value);
to_meta(_Name, Value) when is_float(Value) ->
    float_to_binary(Value, [compact, {decimals, 253}]);
to_meta(Name, Value) -> to_tag(Name, Value).

to_tag(Value) -> to_tag(nil, Value).

to_tag(_Name, Value) when is_function(Value) -> Value();
to_tag(_Name, Value) when is_list(Value) -> list_to_binary(Value);
to_tag(_Name, Value) -> Value.

optional_fields(Span) ->
    lists:foldl(fun(Field, Acc) ->
                       case span_field(Field, Span) of
                           undefined -> Acc;
                           Value -> maps:put(Field, Value, Acc)
                       end
               end, #{}, [<<"error">>, <<"parent_id">>]).

span_field(<<"error">>, #span{status = undefined}) -> undefined;
span_field(<<"error">>, #span{status = #status{code = 0}}) -> undefined;
span_field(<<"error">>, _) -> 1;

span_field(<<"parent_id">>, #span{parent_span_id = ParentId}) -> ParentId.
