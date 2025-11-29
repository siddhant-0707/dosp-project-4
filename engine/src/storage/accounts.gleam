import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import sqlight

pub type Account {
  Account(
    id: Int,
    username: String,
    created_at: Int,
    karma: Int,
    public_key: Option(String),
  )
}

/// Create account without public key (legacy support)
pub fn create(
  conn: sqlight.Connection,
  username: String,
) -> Result(Account, String) {
  create_with_key(conn, username, None)
}

/// Create account with optional public key for digital signatures
pub fn create_with_key(
  conn: sqlight.Connection,
  username: String,
  public_key: Option(String),
) -> Result(Account, String) {
  let sql =
    "insert into accounts(username, created_at, karma, public_key) values (?, strftime('%s','now')*1000, 0, ?) returning id, username, created_at, karma, public_key;"
  let pk = sqlight.nullable(sqlight.text, public_key)
  let decoder = {
    use id <- decode.field(0, decode.int)
    use name <- decode.field(1, decode.string)
    use created <- decode.field(2, decode.int)
    use karma <- decode.field(3, decode.int)
    use pubkey <- decode.field(4, decode.optional(decode.string))
    decode.success(Account(
      id: id,
      username: name,
      created_at: created,
      karma: karma,
      public_key: pubkey,
    ))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.text(username), pk],
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
  let sql =
    "select id, username, created_at, karma, public_key from accounts where username = ?"
  let decoder = account_decoder()
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

/// Get account by ID
pub fn by_id(
  conn: sqlight.Connection,
  account_id: Int,
) -> Result(Option(Account), String) {
  let sql =
    "select id, username, created_at, karma, public_key from accounts where id = ?"
  let decoder = account_decoder()
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(account_id)],
      expecting: decoder,
    )
  {
    Ok([account]) -> Ok(Some(account))
    Ok([]) -> Ok(None)
    Ok(_) -> Error("Expected 0 or 1 rows")
    Error(e) -> Error(error_to_string(e))
  }
}

/// Get public key for a user (for signature verification)
pub fn get_public_key(
  conn: sqlight.Connection,
  account_id: Int,
) -> Result(Option(String), String) {
  let sql = "select public_key from accounts where id = ?;"
  let decoder = {
    use pk <- decode.field(0, decode.optional(decode.string))
    decode.success(pk)
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(account_id)],
      expecting: decoder,
    )
  {
    Ok([pk]) -> Ok(pk)
    Ok([]) -> Error("Account not found")
    Ok(_) -> Error("Unexpected result")
    Error(e) -> Error(error_to_string(e))
  }
}

fn account_decoder() {
  use id <- decode.field(0, decode.int)
  use name <- decode.field(1, decode.string)
  use created <- decode.field(2, decode.int)
  use karma <- decode.field(3, decode.int)
  use pubkey <- decode.field(4, decode.optional(decode.string))
  decode.success(Account(
    id: id,
    username: name,
    created_at: created,
    karma: karma,
    public_key: pubkey,
  ))
}

/// Update karma for an account by adding delta
pub fn update_karma(
  conn: sqlight.Connection,
  account_id: Int,
  delta: Int,
) -> Result(Nil, String) {
  let sql = "update accounts set karma = karma + ? where id = ?;"
  let decoder = decode.success(Nil)
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(delta), sqlight.int(account_id)],
      expecting: decoder,
    )
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(error_to_string(e))
  }
}

/// Get karma for an account
pub fn get_karma(
  conn: sqlight.Connection,
  account_id: Int,
) -> Result(Int, String) {
  let sql = "select karma from accounts where id = ?;"
  let decoder = {
    use karma <- decode.field(0, decode.int)
    decode.success(karma)
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(account_id)],
      expecting: decoder,
    )
  {
    Ok([karma]) -> Ok(karma)
    Ok([]) -> Error("Account not found")
    Ok(_) -> Error("Unexpected query result")
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
