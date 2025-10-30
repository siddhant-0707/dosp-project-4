import client_actor
import config
import engine_api
import engine_server
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import workload/zipf

/// Run actor-based distributed simulation
/// Creates one engine actor and multiple client actors
pub fn run_actor_based(cfg: config.Config) -> Nil {
  io.println(
    "Starting actor-based simulation with "
    <> int.to_string(cfg.num_users)
    <> " client actors...",
  )

  // Step 1: Initialize database and create users/subreddits using direct API
  // (This is setup, so we do it synchronously before spawning actors)
  io.println("\n[Setup] Creating users and subreddits...")
  let #(user_ids, subreddit_ids) = initialize_data(cfg)

  // Step 2: Calculate Zipf distribution for subreddit memberships
  io.println("[Setup] Assigning users to subreddits (Zipf distribution)...")
  let subreddit_sizes =
    zipf.generate_memberships(cfg.num_users, cfg.num_subreddits, cfg.zipf_s)

  // Assign memberships via API
  let _ = assign_memberships_via_api(user_ids, subreddit_ids, subreddit_sizes)

  // Calculate subreddit popularity
  let subreddit_popularity =
    calculate_subreddit_popularity(
      list.map(subreddit_sizes, fn(pair) {
        let #(_, size) = pair
        size
      }),
    )

  // Step 3: Start the engine actor
  io.println("\n[Spawn] Starting engine actor...")
  let assert Ok(engine) = engine_server.start()
  io.println("Engine actor started!")

  // Step 4: Spawn client actors
  io.println(
    "[Spawn] Creating " <> int.to_string(cfg.num_users) <> " client actors...",
  )
  let clients =
    user_ids
    |> list.map(fn(user_id) {
      let assert Ok(client) = client_actor.start(user_id, engine)
      client
    })

  io.println("All client actors spawned!")

  // Step 5: Tell all clients to start performing actions concurrently
  io.println("\n[Run] Starting concurrent simulation...")
  let actions_per_client = 10

  clients
  |> list.index_map(fn(client, idx) {
    // Get user's subreddits (simplified: use first subreddit)
    let user_subs = case subreddit_ids {
      [] -> []
      [first, ..] -> [first]
    }

    // Get popularity for this user's subreddit
    let popularity = case
      get_at(subreddit_popularity, idx % list.length(subreddit_popularity))
    {
      Ok(pop) -> pop
      Error(_) -> 1.0
    }

    // Send message to start actions (non-blocking)
    process.send(
      client,
      client_actor.PerformActions(actions_per_client, user_subs, popularity),
    )
  })

  // Step 6: Wait for clients to complete and write their metrics
  io.println("Waiting for clients to complete...")
  process.sleep(15_000)
  // Wait 15 seconds for actions to complete and metrics to be written

  // Step 7: Clients have written their individual metrics files
  io.println("\n[Metrics] Client metrics written to metrics/client_*.csv")
  io.println(
    "Each of the "
    <> int.to_string(cfg.num_users)
    <> " clients wrote their own metrics file.",
  )
  io.println(
    "Total operations: approximately "
    <> int.to_string(cfg.num_users * actions_per_client)
    <> " actions",
  )

  // Step 8: Shutdown clients and engine
  io.println("\n[Cleanup] Shutting down actors...")
  list.each(clients, fn(client) { process.send(client, client_actor.Shutdown) })

  // Give clients time to shut down
  process.sleep(1000)

  // Shutdown engine
  let _ =
    process.call(engine, waiting: 5000, sending: fn(reply) {
      engine_server.Shutdown(reply)
    })

  io.println("\nSimulation complete!")
}

/// Initialize users and subreddits via direct API calls
fn initialize_data(cfg: config.Config) -> #(List(Int), List(Int)) {
  // Create users
  let users =
    list.range(0, cfg.num_users - 1)
    |> list.filter_map(fn(i) {
      let username = "user" <> int.to_string(i)
      case engine_api.register(username) {
        Ok(account) -> Ok(account.id)
        Error(_) -> Error(Nil)
      }
    })

  // Create subreddits
  let subreddits =
    list.range(0, cfg.num_subreddits - 1)
    |> list.filter_map(fn(i) {
      let name = "sub" <> int.to_string(i)
      case engine_api.create_subreddit(name) {
        Ok(sr) -> Ok(sr.id)
        Error(_) -> Error(Nil)
      }
    })

  #(users, subreddits)
}

/// Assign memberships using direct API
fn assign_memberships_via_api(
  users: List(Int),
  subreddits: List(Int),
  subreddit_sizes: List(#(Int, Int)),
) -> Nil {
  subreddit_sizes
  |> list.each(fn(pair) {
    let #(sub_idx, size) = pair
    let sub_id = case get_at(subreddits, sub_idx) {
      Ok(id) -> id
      Error(_) -> 0
    }

    users
    |> list.take(size)
    |> list.each(fn(user_id) {
      let _ = engine_api.join_subreddit(user_id, sub_id)
      Nil
    })
  })
}

/// Get element at index from list
fn get_at(lst: List(a), idx: Int) -> Result(a, Nil) {
  lst
  |> list.drop(idx)
  |> list.first
}

/// Calculate subreddit popularity multipliers based on member counts
fn calculate_subreddit_popularity(subreddit_sizes: List(Int)) -> List(Float) {
  // Find max size to normalize
  let max_size =
    list.fold(subreddit_sizes, 0, fn(acc, size) {
      case size > acc {
        True -> size
        False -> acc
      }
    })

  let max_float = int.to_float(max_size)

  // Normalize each size to a 1.0-3.0 range
  subreddit_sizes
  |> list.map(fn(size) {
    let ratio = int.to_float(size) /. max_float
    1.0 +. ratio *. 2.0
  })
}
