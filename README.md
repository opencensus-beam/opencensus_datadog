oc_datadog
=====

[![CircleCI](https://circleci.com/gh/opencensus-beam/opencensus_datadog.svg?style=svg)](https://circleci.com/gh/opencensus-beam/opencensus_datadog)
[![codecov](https://codecov.io/gh/opencensus-beam/opencensus_datadog/branch/master/graph/badge.svg)](https://codecov.io/gh/opencensus-beam/opencensus_datadog)

[Opencensus][oc] integration to [DataDog][dd] traces and metrics (via dogstatsd).

## Installation

Rebar3:

```erlang
{deps, [{oc_datadog, "~> 0.1.0"}]}.
```

Mix:

```elixir
def deps do
  [
    {:oc_datadog, "~> 0.1.0"}
  ]
end
```

## Configuration

`sys.conf`:

```erlang
[
    {opencensus, [
        {reporter, {oc_reporter_datadog, []}},
        {stat, [
            {exporters, [
                {oc_stat_exporter_datadog, []}
            ]}
        }]
    ]}
].
```

Elixir:

```elixir
config :opencensus, :reporter, {:oc_reporter_datadog, []}

config :opencensus, :stat,
  exporters: [{:oc_stat_exporter_datadog, []}]
```

## License

See [LICENSE](LICENSE) file.

[oc]: https://github.com/census-instrumentation/opencensus-erlang "Opencensus Erlang"
[dd]: https://datadoghq.com "DataDog"
