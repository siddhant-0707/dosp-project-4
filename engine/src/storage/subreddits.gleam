import gleam/dynamic/decode
import sqlight

pub type Subreddit {
  Subreddit(id: Int, name: String, created_at: Int)
}

pub fn create(
  conn: sqlight.Connection,
  name: String,
) -> Result(Subreddit, String) {
  let sql =
    "insert into subreddits(name, created_at) values (?, strftime('%s','now')*1000) returning id, name, created_at;"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use n <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    decode.success(Subreddit(id: id, name: n, created_at: created))
  }
  case
    sqlight.query(sql, on: conn, with: [sqlight.text(name)], expecting: decoder)
  {
    Ok([subreddit]) -> Ok(subreddit)
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

/// Search for subreddits by name (case-insensitive partial match)
pub fn search(
  conn: sqlight.Connection,
  query: String,
) -> Result(List(Subreddit), String) {
  let sql =
    "select id, name, created_at from subreddits where lower(name) like lower(?) order by name limit 50;"
  let search_pattern = "%" <> query <> "%"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use n <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    decode.success(Subreddit(id: id, name: n, created_at: created))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.text(search_pattern)],
      expecting: decoder,
    )
  {
    Ok(results) -> Ok(results)
    Error(e) -> Error(error_to_string(e))
  }
}

/// Get all subreddits (limited to 100)
pub fn list_all(conn: sqlight.Connection) -> Result(List(Subreddit), String) {
  let sql =
    "select id, name, created_at from subreddits order by created_at desc limit 100;"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use n <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    decode.success(Subreddit(id: id, name: n, created_at: created))
  }
  case sqlight.query(sql, on: conn, with: [], expecting: decoder) {
    Ok(results) -> Ok(results)
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
