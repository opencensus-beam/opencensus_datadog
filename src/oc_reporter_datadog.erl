-module(oc_reporter_datadog).

-behaviour(oc_reporter).

%% API exports
-export([init/1, report/2]).

-include_lib("opencensus/include/opencensus.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8126).
-define(DEFAULT_SERVICE, <<"opencensus-app">>).
-define(DEFAULT_TYPE, <<"custom">>).

init(Options) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    Service = proplists:get_value(service, Options, ?DEFAULT_SERVICE),
    Type = proplists:get_value(type, Options, ?DEFAULT_TYPE),
    #{host => Host, port => Port, service => Service, type => Type}.

report(Spans, #{service := Service, host := Host, port := Port, type := Type}) ->
    Sorted = lists:sort(fun(A, B) ->
                                A#span.trace_id =< B#span.trace_id
                        end, Spans),
    Grouped = group(Sorted),
    DSpans = [[build_span(S, Service, Type) || S <- Trace] || Trace <- Grouped],


    try jsx:encode(DSpans) of
        JSON ->
            Address = io_lib:format('http://~s:~B/v0.3/traces', [Host, Port]),
            case httpc:request(
                   put,
                   {Address, [], "application/json", JSON},
                   [],
                   []
                  ) of
                {ok, {{_, Code, _}, _, _}} when Code >= 200; Code =< 299 ->
                    ok;
                {ok, {{_, Code, _}, _, Message}} ->
                    ?LOG_ERROR("DD: Unable to send spans, DD reported an error: ~p : ~p",
                              [Code, Message]);
                {error, Reason} ->
                    ?LOG_ERROR("DD: Unable to send spans, client error: ~p", [Reason])
            end
    catch
        error:_ -> throw(datadog_json_encode_error)
    end.

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
      <<"duration">> => wts:duration(Span#span.start_time, Span#span.end_time) * 1000,
      <<"type">> => to_tag(Type),
      <<"meta">> => to_meta(Span#span.attributes)}.

to_meta(Attributes) -> maps:map(fun to_tag/2, Attributes).

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
