import config
import coordinator
import gleam/io
import gleam/list
import gleam/string

pub fn main() {
  // Parse command-line arguments
  case config.from_args() {
    Ok(cfg) -> {
      // Print configuration
      config.print(cfg)
      io.println("")
      
      // Run actor-based simulation
      coordinator.run_actor_based(cfg)
    }
    
    Error(e) -> {
      // Print error and usage
      io.println_error(
        e
        |> string.split("\n")
        |> list.map(fn(line) { "Error: " <> line })
        |> string.join("\n"),
      )
      io.println_error("")
      config.usage()
    }
  }
}
