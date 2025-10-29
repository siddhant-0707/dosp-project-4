import config
import simulator

pub fn main() {
  // Load configuration (default for now)
  let cfg = config.default()

  // Run simulator
  simulator.run(cfg)
}
