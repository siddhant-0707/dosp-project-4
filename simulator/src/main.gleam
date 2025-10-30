import config
import coordinator
import simulator

pub fn main() {
  // Load configuration (default for now)
  let cfg = config.default()

  // Check if multi-worker mode
  case cfg.workers > 1 {
    True -> coordinator.run_distributed(cfg)
    False -> simulator.run(cfg)
  }
}
