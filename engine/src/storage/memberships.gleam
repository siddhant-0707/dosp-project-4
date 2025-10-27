import gleam/dynamic/decode
import gleam/result
import sqlight

pub fn join(
  conn: sqlight.Connection,
  account_id: Int,
  subreddit_id: Int,
) -> Result(Nil, String) {
  let sql =
    "insert into memberships(account_id, subreddit_id, joined_at) values (?, ?, strftime('%s','now')*1000) on conflict(account_id, subreddit_id) do nothing;"
  let decoder = decode.success(Nil)
  sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(account_id), sqlight.int(subreddit_id)],
    expecting: decoder,
  )
  |> result.map(fn(_) { Nil })
  |> result.map_error(error_to_string)
}

pub fn leave(
  conn: sqlight.Connection,
  account_id: Int,
  subreddit_id: Int,
) -> Result(Nil, String) {
  let sql = "delete from memberships where account_id = ? and subreddit_id = ?;"
  let decoder = decode.success(Nil)
  sqlight.query(
    sql,
    on: conn,
    with: [sqlight.int(account_id), sqlight.int(subreddit_id)],
    expecting: decoder,
  )
  |> result.map(fn(_) { Nil })
  |> result.map_error(error_to_string)
}

pub fn subscribed_subreddit_ids(
  conn: sqlight.Connection,
  account_id: Int,
) -> Result(List(Int), String) {
  let sql = "select subreddit_id from memberships where account_id = ?"
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(id)
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(account_id)],
      expecting: decoder,
    )
  {
    Ok(ids) -> Ok(ids)
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
