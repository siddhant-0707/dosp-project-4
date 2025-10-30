import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

/// Metric record for a single operation
pub type OpMetric {
  OpMetric(timestamp_ms: Int, operation: String, success: Bool, latency_ms: Int)
}

/// Aggregated metrics for a time window
pub type AggregatedMetrics {
  AggregatedMetrics(
    posts_per_sec: Float,
    comments_per_sec: Float,
    votes_per_sec: Float,
    dms_per_sec: Float,
    errors: Int,
    total_ops: Int,
  )
}

/// Collector state
pub type Collector {
  Collector(metrics: List(OpMetric), start_time_ms: Int)
}

/// Create a new metrics collector
pub fn new() -> Collector {
  Collector(metrics: [], start_time_ms: current_time_ms())
}

/// Merge multiple collectors into one
pub fn merge(collectors: List(Collector)) -> Collector {
  let all_metrics =
    collectors
    |> list.flat_map(fn(c) { c.metrics })

  let earliest_start =
    collectors
    |> list.map(fn(c) { c.start_time_ms })
    |> list.reduce(fn(a, b) {
      case a < b {
        True -> a
        False -> b
      }
    })
    |> result.unwrap(current_time_ms())

  Collector(metrics: all_metrics, start_time_ms: earliest_start)
}

/// Record a metric
pub fn record(
  collector: Collector,
  operation: String,
  success: Bool,
  latency_ms: Int,
) -> Collector {
  let metric =
    OpMetric(
      timestamp_ms: current_time_ms(),
      operation: operation,
      success: success,
      latency_ms: latency_ms,
    )
  Collector(..collector, metrics: [metric, ..collector.metrics])
}

/// Calculate aggregated metrics
pub fn aggregate(collector: Collector) -> AggregatedMetrics {
  let now = current_time_ms()
  let elapsed_secs = int.to_float(now - collector.start_time_ms) /. 1000.0

  let total_ops = list.length(collector.metrics)
  let errors =
    collector.metrics
    |> list.filter(fn(m) { !m.success })
    |> list.length

  let posts =
    collector.metrics
    |> list.filter(fn(m) { m.operation == "create_post" })
    |> list.length

  let comments =
    collector.metrics
    |> list.filter(fn(m) { m.operation == "create_comment" })
    |> list.length

  let votes =
    collector.metrics
    |> list.filter(fn(m) {
      m.operation == "vote_post" || m.operation == "vote_comment"
    })
    |> list.length

  let dms =
    collector.metrics
    |> list.filter(fn(m) { m.operation == "send_dm" })
    |> list.length

  AggregatedMetrics(
    posts_per_sec: int.to_float(posts) /. elapsed_secs,
    comments_per_sec: int.to_float(comments) /. elapsed_secs,
    votes_per_sec: int.to_float(votes) /. elapsed_secs,
    dms_per_sec: int.to_float(dms) /. elapsed_secs,
    errors: errors,
    total_ops: total_ops,
  )
}

/// Print terminal summary
pub fn print_summary(metrics: AggregatedMetrics) -> Nil {
  io.println("\n=== Simulation Summary ===")
  io.println("Total operations: " <> int.to_string(metrics.total_ops))
  io.println("Errors: " <> int.to_string(metrics.errors))
  io.println("Posts/sec: " <> float_to_string_simple(metrics.posts_per_sec))
  io.println(
    "Comments/sec: " <> float_to_string_simple(metrics.comments_per_sec),
  )
  io.println("Votes/sec: " <> float_to_string_simple(metrics.votes_per_sec))
  io.println("DMs/sec: " <> float_to_string_simple(metrics.dms_per_sec))
  io.println("=========================")
}

/// Write metrics to CSV file
pub fn write_csv(collector: Collector, filename: String) -> Result(Nil, String) {
  // Build CSV content
  let header = "timestamp_ms,operation,success,latency_ms\n"

  let rows =
    collector.metrics
    |> list.reverse
    |> list.map(fn(m) {
      int.to_string(m.timestamp_ms)
      <> ","
      <> m.operation
      <> ","
      <> bool_to_string(m.success)
      <> ","
      <> int.to_string(m.latency_ms)
      <> "\n"
    })
    |> string.concat

  let content = header <> rows

  // Write to file using Erlang's file:write_file
  // Convert Erlang result to Gleam Result
  case erlang_write_file(filename, content) {
    _ -> Ok(Nil)
  }
}

@external(erlang, "file", "write_file")
fn erlang_write_file(filename: String, content: String) -> Atom

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn float_to_string_simple(f: Float) -> String {
  // Simple float to string conversion
  let i = float_to_int_approx(f)
  let frac = f -. int.to_float(i)
  let frac_int = float_round(frac *. 100.0)
  int.to_string(i) <> "." <> pad_int(frac_int, 2)
}

fn pad_int(n: Int, width: Int) -> String {
  let s = int.to_string(n)
  let len = string.length(s)
  case len < width {
    True -> string.repeat("0", width - len) <> s
    False -> s
  }
}

fn float_to_int_approx(f: Float) -> Int {
  case f >=. 0.0 {
    True -> truncate_float(f)
    False -> 0 - truncate_float(0.0 -. f)
  }
}

@external(erlang, "erlang", "trunc")
fn truncate_float(f: Float) -> Int

fn float_round(f: Float) -> Int {
  truncate_float(f +. 0.5)
}

type Atom

@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Atom) -> Int

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(s: String) -> Atom

fn current_time_ms() -> Int {
  erlang_system_time(binary_to_atom("millisecond"))
}
