-module(oc_reporter_datadog_SUITE).

-include_lib("opencensus/include/opencensus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2]).
-export([test_eunit/1, test_reports_spans/1]).

all() -> [test_eunit, test_reports_spans].

init_per_testcase(_, Config) ->
    Span = #span{
              name = <<"foo">>,
              trace_id = 1,
              span_id = 1,
              start_time = wts:timestamp(),
              end_time = wts:timestamp(),
              attributes = #{foo => "bar"}
             },
    Options = oc_reporter_datadog:init([{http_client, fun mock_client/3}]),
    [{span, Span}, {options, Options} | Config].

mock_client(Address, Headers, JSON) ->
    self() ! {http, lists:flatten(Address), Headers, JSON},
    ok.

test_reports_spans(Config) ->
    Options = ?config(options, Config),
    Span = ?config(span, Config),
    oc_reporter_datadog:report([Span], Options),
    receive
        {http, "http://localhost:8126/v0.3/traces", _, JSON} ->
            Data = jsx:decode(JSON, [return_maps]),
            [[RSpan]] = Data,
            #{<<"name">> := <<"foo">>,
             <<"trace_id">> := 1,
             <<"span_id">> := 1,
             <<"type">> := <<"custom">>,
             <<"duration">> := _,
             <<"meta">> := #{<<"foo">> := <<"bar">>}} = RSpan,
            ok;
        Msg ->
            ct:fail("Unknown message: ~p", [Msg])
    after
        1000 -> ct:fail("Didn't received message in 1s")
    end.

%% EUNIT TESTS =================================================================

test_eunit(_Config) -> eunit:test(?MODULE, []).

init_return_value_contains_needed_keys_test_() ->
    Opts = oc_reporter_datadog:init([]),
    {"test that init value contains needed keys", inparallel,
     [?_assert(maps:is_key(service, Opts)),
      ?_assert(maps:is_key(host, Opts)),
      ?_assert(maps:is_key(port, Opts)),
      ?_assert(maps:is_key(type, Opts))]}.

init_sets_proper_fields_to_requested_values_test_() ->
    {"test that init sets proper fields in resulting map", inparallel,
     [init_sets(host, "foo"),
      init_sets(port, 6666),
      init_sets(service, "foo"),
      init_sets(type, "foo")]}.

init_sets(Key, Value) ->
    Opts = oc_reporter_datadog:init([{Key, Value}]),
    ?_assertEqual(Value, maps:get(Key, Opts)).
