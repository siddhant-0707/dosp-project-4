import gleam/erlang/process
import gleam/io
import sqlight
import storage/db
import storage/schema
import web

pub fn main() {
  // Initialize database schema
  use conn <- sqlight.with_connection(db.default_db_path)
  let _ = schema.init(conn)
  io.println("[Engine] Database initialized")

  // Start REST API server on port 8080
  web.start(8080)

  // Keep the process running
  io.println("[Engine] Server running. Press Ctrl+C to stop.")
  process.sleep_forever()
}
