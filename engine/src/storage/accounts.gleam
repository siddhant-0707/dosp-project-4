import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import sqlight

pub type Account {
  Account(id: Int, username: String, created_at: Int)
}

pub fn create(
  conn: sqlight.Connection,
  username: String,
) -> Result(Account, String) {
  let sql =
    "insert into accounts(username, created_at) values (?, strftime('%s','now')*1000) returning id, username, created_at;"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use name <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    decode.success(Account(id: id, username: name, created_at: created))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.text(username)],
      expecting: decoder,
    )
  {
    Ok([account]) -> Ok(account)
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

pub fn by_username(
  conn: sqlight.Connection,
  username: String,
) -> Result(Option(Account), String) {
  let sql = "select id, username, created_at from accounts where username = ?"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use name <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    decode.success(Account(id: id, username: name, created_at: created))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.text(username)],
      expecting: decoder,
    )
  {
    Ok([account]) -> Ok(Some(account))
    Ok([]) -> Ok(None)
    Ok(_) -> Error("Expected 0 or 1 rows")
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
