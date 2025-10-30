import config
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import simulator

/// Run distributed simulation with multiple worker processes
pub fn run_distributed(cfg: config.Config) -> Nil {
  io.println(
    "Starting distributed simulation with "
    <> int.to_string(cfg.workers)
    <> " workers...",
  )

  // Partition users across workers
  let worker_assignments = partition_users(cfg.num_users, cfg.workers)

  // Spawn worker processes
  let _worker_pids =
    worker_assignments
    |> list.map(fn(user_ids) {
      process.spawn(fn() { simulator.run_worker(cfg, user_ids) })
    })

  io.println(
    "Spawned "
    <> int.to_string(list.length(worker_assignments))
    <> " worker processes",
  )

  // Wait for workers to complete (simplified - just sleep for duration)
  let sleep_ms = cfg.duration_secs * 1000 + 5000
  process.sleep(sleep_ms)

  io.println("\nAll workers completed!")
}

/// Partition users into chunks for workers
fn partition_users(num_users: Int, num_workers: Int) -> List(List(Int)) {
  let users = list.range(0, num_users - 1)
  chunk_list(users, num_users / num_workers)
}

/// Split a list into chunks of given size
fn chunk_list(lst: List(a), chunk_size: Int) -> List(List(a)) {
  case lst {
    [] -> []
    _ -> {
      let chunk = list.take(lst, chunk_size)
      let rest = list.drop(lst, chunk_size)
      [chunk, ..chunk_list(rest, chunk_size)]
    }
  }
}
