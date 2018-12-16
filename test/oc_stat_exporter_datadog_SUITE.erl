-module(oc_stat_exporter_datadog_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_eunit/1, test_export/1, test_export_with_tags/1]).

all() -> [test_eunit, test_export, test_export_with_tags].

init_per_testcase(test_eunit, Config) -> Config;
init_per_testcase(_, Config) ->
    _ = application:ensure_all_started(oc_datadog),
    {ok, Socket} = gen_udp:open(8125, [{active, once}]),
    Measure = oc_stat_measure:new('datadog/test', "Test", foos),
    {ok, View} = oc_stat_view:subscribe(#{
                  name => "datadog.test",
                  measure => Measure,
                  description => "Test",
                  tags => [foo, bar],
                  aggregation => oc_stat_aggregation_count}),
    [{udp, Socket}, {measure, Measure}, {view, View} | Config].

end_per_testcase(test_eunit, Config) -> Config;
end_per_testcase(_, Config) ->
    Socket = ?config(udp, Config),
    View = ?config(view, Config),
    oc_stat_view:unsubscribe(View),
    oc_stat_view:deregister(View),
    gen_udp:close(Socket),
    Config.

test_eunit(_Config) -> eunit:test(?MODULE, []).

test_export(Config) ->
    _ = oc_stat:record(#{}, 'datadog/test', 1),
    View = ?config(view, Config),
    Data = oc_stat_view:export(View),
    oc_stat_exporter_datadog:export([Data], []),
    receive
        {udp, _, _, _, "datadog.test:1|g"} -> ok;
        Msg ->
            ct:fail("Unknown message: ~p", [Msg])
    after
        1000 -> ct:fail("Didn't received message in 1s")
    end.

test_export_with_tags(Config) ->
    _ = oc_stat:record(#{foo => "1", bar => "2"}, 'datadog/test', 1),
    View = ?config(view, Config),
    Data = oc_stat_view:export(View),
    oc_stat_exporter_datadog:export([Data], []),
    receive
        {udp, _, _, _, "datadog.test:1|g|#bar:2,foo:1"} -> ok;
        {udp, _, _, _, "datadog.test:1|g|#foo:1,bar:2"} -> ok;
        Msg ->
            ct:fail("Unknown message: ~p", [Msg])
    after
        1000 -> ct:fail("Didn't received message in 1s")
    end.
