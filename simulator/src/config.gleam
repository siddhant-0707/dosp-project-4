import gleam/int
import gleam/io

/// Configuration for the simulator workload
pub type Config {
  Config(
    /// Number of users to simulate
    num_users: Int,
    /// Number of subreddits to create
    num_subreddits: Int,
    /// Duration of simulation in seconds
    duration_secs: Int,
    /// Zipf distribution parameter for subreddit membership
    zipf_s: Float,
    /// Post creation rate (probability per user per second)
    post_rate: Float,
    /// Comment creation rate (probability per user per second)
    comment_rate: Float,
    /// Vote rate (probability per user per second)
    vote_rate: Float,
    /// Direct message rate (probability per user per second)
    dm_rate: Float,
    /// Repost rate (probability per user per second)
    repost_rate: Float,
    /// Mean online duration in seconds
    online_mean_secs: Float,
    /// Mean offline duration in seconds
    offline_mean_secs: Float,
    /// Random seed for reproducibility
    seed: Int,
    /// Number of concurrent worker processes
    workers: Int,
  )
}

/// Default configuration with reasonable values
pub fn default() -> Config {
  Config(
    num_users: 100,
    num_subreddits: 10,
    duration_secs: 60,
    zipf_s: 1.1,
    post_rate: 0.01,
    comment_rate: 0.05,
    vote_rate: 0.1,
    dm_rate: 0.005,
    repost_rate: 0.01,
    online_mean_secs: 300.0,
    offline_mean_secs: 60.0,
    seed: 42,
    workers: 1,
    // Set to 1 to avoid SQLite concurrency issues
  )
}

/// Parse configuration from command-line arguments
/// For now, returns default config (will be enhanced with actual CLI parsing)
pub fn from_args(args: List(String)) -> Result(Config, String) {
  // For now, just return defaults
  // TODO: Parse actual arguments when gleam has a CLI parsing library
  let _ = args
  Ok(default())
}

/// Print configuration summary
pub fn print(config: Config) -> Nil {
  io.println("=== Simulator Configuration ===")
  io.println("Users: " <> int.to_string(config.num_users))
  io.println("Subreddits: " <> int.to_string(config.num_subreddits))
  io.println("Duration: " <> int.to_string(config.duration_secs) <> "s")
  io.println("Zipf parameter: " <> float_to_string(config.zipf_s))
  io.println("Post rate: " <> float_to_string(config.post_rate))
  io.println("Comment rate: " <> float_to_string(config.comment_rate))
  io.println("Vote rate: " <> float_to_string(config.vote_rate))
  io.println("DM rate: " <> float_to_string(config.dm_rate))
  io.println("Repost rate: " <> float_to_string(config.repost_rate))
  io.println("Online mean: " <> float_to_string(config.online_mean_secs) <> "s")
  io.println(
    "Offline mean: " <> float_to_string(config.offline_mean_secs) <> "s",
  )
  io.println("Seed: " <> int.to_string(config.seed))
  io.println("Workers: " <> int.to_string(config.workers))
  io.println("==============================")
}

fn float_to_string(f: Float) -> String {
  // Gleam doesn't have float.to_string in older versions, use int conversion
  case f {
    _ -> {
      let i = float_to_int_approx(f)
      let frac = f -. int.to_float(i)
      case frac {
        _ if frac <. 0.001 -> int.to_string(i)
        _ ->
          int.to_string(i) <> "." <> int.to_string(float_round(frac *. 1000.0))
      }
    }
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
