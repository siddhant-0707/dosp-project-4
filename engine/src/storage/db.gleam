import sqlight

pub const default_db_path = "reddit.db"

pub fn with_engine_connection(f: fn(sqlight.Connection) -> a) -> a {
  use conn <- sqlight.with_connection(default_db_path)
  f(conn)
}
