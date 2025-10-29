import config
import engine_api
import gleam/int
import gleam/io
import gleam/list
import metrics/collector
import workload/behavior
import workload/zipf

/// Main simulation entry point
pub fn run(cfg: config.Config) -> Nil {
  io.println("Starting Reddit simulator...")
  config.print(cfg)

  // Initialize metrics collector
  let metrics = collector.new()

  // Step 1: Initialize engine - create users and subreddits
  io.println("\n[1/4] Creating users and subreddits...")
  let #(users, subreddits) =
    initialize_engine(cfg.num_users, cfg.num_subreddits)

  // Step 2: Assign users to subreddits using Zipf distribution
  io.println("[2/4] Assigning users to subreddits (Zipf distribution)...")
  let subreddit_sizes =
    zipf.generate_memberships(cfg.num_users, cfg.num_subreddits, cfg.zipf_s)
  let user_memberships = assign_memberships(users, subreddits, subreddit_sizes)

  // Step 3: Run simulation workload
  io.println("[3/4] Running simulation workload...")
  let metrics = run_workload(cfg, users, user_memberships, metrics)

  // Step 4: Print results
  io.println("[4/4] Collecting metrics...")
  let aggregated = collector.aggregate(metrics)
  collector.print_summary(aggregated)
  let _ = collector.write_csv(metrics, "metrics/ops.csv")

  io.println("\nSimulation complete!")
}

/// Initialize engine by creating users and subreddits
fn initialize_engine(
  num_users: Int,
  num_subreddits: Int,
) -> #(List(Int), List(Int)) {
  // Create users
  let users =
    list.range(0, num_users - 1)
    |> list.filter_map(fn(i) {
      let username = "user" <> int.to_string(i)
      case engine_api.register(username) {
        Ok(account) -> Ok(account.id)
        Error(_) -> Error(Nil)
      }
    })

  // Create subreddits
  let subreddits =
    list.range(0, num_subreddits - 1)
    |> list.filter_map(fn(i) {
      let name = "sub" <> int.to_string(i)
      case engine_api.create_subreddit(name) {
        Ok(sr) -> Ok(sr.id)
        Error(_) -> Error(Nil)
      }
    })

  #(users, subreddits)
}

/// Assign users to subreddits based on Zipf distribution
fn assign_memberships(
  users: List(Int),
  subreddits: List(Int),
  subreddit_sizes: List(#(Int, Int)),
) -> List(#(Int, List(Int))) {
  // For each subreddit, assign the specified number of users
  let _ =
    subreddit_sizes
    |> list.each(fn(pair) {
      let #(sub_idx, size) = pair
      let sub_id = case get_at(subreddits, sub_idx) {
        Ok(id) -> id
        Error(_) -> 0
      }

      // Take first 'size' users and join them to this subreddit
      users
      |> list.take(size)
      |> list.each(fn(user_id) {
        let _ = engine_api.join_subreddit(user_id, sub_id)
        Nil
      })
    })

  // Return user -> subreddits mapping (simplified: each user joins first subreddit)
  users
  |> list.map(fn(user_id) {
    let user_subs = case subreddits {
      [] -> []
      [first, ..] -> [first]
    }
    #(user_id, user_subs)
  })
}

/// Run the main simulation workload
fn run_workload(
  _cfg: config.Config,
  users: List(Int),
  user_memberships: List(#(Int, List(Int))),
  metrics: collector.Collector,
) -> collector.Collector {
  // Simulate actions for each user
  // Simplified: each user performs a fixed number of actions
  let actions_per_user = 10

  users
  |> list.fold(metrics, fn(m, user_id) {
    list.range(0, actions_per_user - 1)
    |> list.fold(m, fn(m2, action_idx) {
      // Get user's subreddits
      let user_subs = case
        list.find(user_memberships, fn(pair) {
          let #(uid, _) = pair
          uid == user_id
        })
      {
        Ok(#(_, subs)) -> subs
        Error(_) -> []
      }

      // Generate and execute action
      let action = behavior.generate_action(user_id, user_subs, action_idx)
      let start = current_time_ms()
      let result = behavior.execute_action(action)
      let latency = current_time_ms() - start

      let success = case result {
        Ok(_) -> True
        Error(_) -> False
      }

      let op_name = action_name(action)
      collector.record(m2, op_name, success, latency)
    })
  })
}

/// Get action name for metrics
fn action_name(action: behavior.Action) -> String {
  case action {
    behavior.CreatePost(_, _) -> "create_post"
    behavior.CreateComment(_, _) -> "create_comment"
    behavior.VotePost(_, _, _) -> "vote_post"
    behavior.VoteComment(_, _, _) -> "vote_comment"
    behavior.SendDM(_, _) -> "send_dm"
    behavior.CheckFeed(_) -> "check_feed"
  }
}

type Atom

@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Atom) -> Int

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(s: String) -> Atom

fn current_time_ms() -> Int {
  erlang_system_time(binary_to_atom("millisecond"))
}

/// Get element at index from list
fn get_at(lst: List(a), idx: Int) -> Result(a, Nil) {
  lst
  |> list.drop(idx)
  |> list.first
}
