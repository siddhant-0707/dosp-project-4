import gleam/io
import sqlight
import storage/db
import storage/schema

pub fn main() {
  use conn <- sqlight.with_connection(db.default_db_path)
  let _ = schema.init(conn)
  io.println("engine started")
}
