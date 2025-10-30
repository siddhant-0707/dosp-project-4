import config
import coordinator

pub fn main() {
  // Load configuration (default for now)
  let cfg = config.default()

  // Always use actor-based simulation for proper concurrency
  coordinator.run_actor_based(cfg)
}
