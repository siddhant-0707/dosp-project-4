import gleam/erlang/process
import gleam/io
import mist
import sqlight
import storage/db
import storage/schema
import web
import wisp
import wisp/wisp_mist

pub fn main() {
  // Initialize database schema
  let result = {
    use conn <- sqlight.with_connection(db.default_db_path)
    schema.init(conn)
  }

  case result {
    Ok(_) -> io.println("[Engine] Database initialized successfully")
    Error(e) -> {
      io.println("[Engine] Database initialization failed: " <> e)
      panic as "Failed to initialize database"
    }
  }

  // Create Wisp secret key for sessions
  let secret_key_base = wisp.random_string(64)

  // Create handler that wraps web.handle_request with Context
  let handler = fn(req) { web.handle_request(req, web.Context) }

  // Start Mist web server with Wisp handler
  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(8080)
    |> mist.start

  io.println("[REST API] Server started on http://localhost:8080")
  io.println("[Engine] Server running. Press Ctrl+C to stop.")

  // Keep the process running
  process.sleep_forever()
}
