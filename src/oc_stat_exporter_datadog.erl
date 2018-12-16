%% @author Łukasz Niemier <lukasz+opensource@niemier.pl>
%%
%% @doc Trace reporter for DataDog ([https://datadog.com]).
%%
%% == Configuration ==
%%
%% <ul>
%%  <li>`host' - address where DataDog Agent lives (default `"localhost"')</li>
%%  <li>
%%    `port' - port on which Agent listens for statsd metrics (default `8125')
%%  </li>
%% </ul>
-module(oc_stat_exporter_datadog).

-behaviour(oc_stat_exporter).

-export([export/2]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8125).

-include_lib("kernel/include/logger.hrl").

export(ViewData, Options) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    {ok, Socket} = gen_udp:open(0, [{active, false}]),
    Packet = build_packet(ViewData),
    ok = gen_udp:send(Socket, Host, Port, Packet),
    ok = gen_udp:close(Socket),
    ok.

build_packet(Data) when is_list(Data) ->
    List = [build_packet(Entry) || Entry <- Data],
    lists:join($\n, List);
build_packet(#{name := Name,
               ctags := CTags,
               tags := Tags,
               data := #{type := Type,
                         rows := Rows}}) ->
    Key = to_key(Name),
    List = [ build_rows(Key, Type, CTags, Tags, Row) || Row <- Rows ],
    lists:join($\n, List).

build_tags(Tags, TagsV, CTags) ->
    TagsMap = maps:merge(CTags, maps:from_list(lists:zip(Tags, TagsV))),
    TagsList = maps:to_list(TagsMap),
    Cleaned = [{Key, Value} || {Key, Value} <- TagsList, Value =/= undefined],
    build_tags(Cleaned).

build_tags([]) -> [];
build_tags(Tags) ->
    List = [[to_key(Key), $:, Value]
      || {Key, Value} <- Tags],
    ["|#", lists:join($,, List)].

to_key(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_key(Value) -> Value.

build_rows(Name, Type, CTags, Tags, #{tags := TagsV, value := Value}) ->
    TagList = build_tags(Tags, TagsV, CTags),
    Metrics = [[Name, Stat] || Stat <- build_row(Type, Value, TagList) ],
    lists:join($\n, Metrics).

build_row(sum, #{count := Count,
                 mean := Mean,
                 sum := Sum}, Tags) ->
    [
     [".count:", format_num(Count), "|g", Tags],
     [".mean:", format_num(Mean), "|g", Tags],
     [".sum:", format_num(Sum), "|g", Tags]
    ];
build_row(distribution, #{buckets := Buckets} = Data, Tags) ->
    BucketRows = [ bucket_row(Bucket, Tags) || Bucket <- Buckets ],
    build_row(sum, Data, Tags) ++ BucketRows;
build_row(_Type, Value, Tags) ->
    [[$:, format_num(Value), "|g", Tags]].

bucket_row({Bound, Count}, Tags) when is_integer(Count) ->
    [
     $:, format_num(Count),
     "|g",
     tags_append(Tags, ["le:", format_num(Bound)])
    ].

tags_append([], Value) -> ["|#", Value];
tags_append(List, Value) -> [List, $,, Value].

format_num(infinity) -> <<"infinity">>;
format_num(Integer) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);
format_num(Float) when is_float(Float) ->
    erlang:float_to_binary(Float, [{decimals, 5}, compact]).
