import gleam/dynamic/decode
import sqlight
import storage/accounts

pub type EntityType {
  Post
  Comment
}

fn entity_to_text(e: EntityType) -> String {
  case e {
    Post -> "post"
    Comment -> "comment"
  }
}

pub fn upsert(
  conn: sqlight.Connection,
  entity_type: EntityType,
  entity_id: Int,
  voter_id: Int,
  value: Int,
) -> Result(Nil, String) {
  let et = entity_to_text(entity_type)

  // Get old vote value to calculate karma delta
  let old_value = get_vote_value(conn, entity_type, entity_id, voter_id)

  let sql =
    "insert into votes(entity_type, entity_id, voter_id, value, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000)\n"
    <> "on conflict(entity_type, entity_id, voter_id) do update set value = excluded.value, created_at = strftime('%s','now')*1000;"
  let decoder = decode.success(Nil)
  case
    sqlight.query(
      sql,
      on: conn,
      with: [
        sqlight.text(et),
        sqlight.int(entity_id),
        sqlight.int(voter_id),
        sqlight.int(value),
      ],
      expecting: decoder,
    )
  {
    Ok(_) -> {
      // Update score
      case update_score(conn, entity_type, entity_id) {
        Ok(_) -> {
          // Update author karma
          update_author_karma(conn, entity_type, entity_id, old_value, value)
        }
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(error_to_string(e))
  }
}

fn get_vote_value(
  conn: sqlight.Connection,
  entity_type: EntityType,
  entity_id: Int,
  voter_id: Int,
) -> Int {
  let et = entity_to_text(entity_type)
  let sql =
    "select value from votes where entity_type = ? and entity_id = ? and voter_id = ?;"
  let decoder = {
    use val <- decode.field(0, decode.int)
    decode.success(val)
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.text(et), sqlight.int(entity_id), sqlight.int(voter_id)],
      expecting: decoder,
    )
  {
    Ok([val]) -> val
    _ -> 0
  }
}

fn update_author_karma(
  conn: sqlight.Connection,
  entity_type: EntityType,
  entity_id: Int,
  old_value: Int,
  new_value: Int,
) -> Result(Nil, String) {
  let delta = new_value - old_value
  case delta {
    0 -> Ok(Nil)
    _ -> {
      // Get author_id
      case get_author_id(conn, entity_type, entity_id) {
        Ok(author_id) -> accounts.update_karma(conn, author_id, delta)
        Error(e) -> Error(e)
      }
    }
  }
}

fn get_author_id(
  conn: sqlight.Connection,
  entity_type: EntityType,
  entity_id: Int,
) -> Result(Int, String) {
  let sql = case entity_type {
    Post -> "select author_id from posts where id = ?;"
    Comment -> "select author_id from comments where id = ?;"
  }
  let decoder = {
    use author_id <- decode.field(0, decode.int)
    decode.success(author_id)
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(entity_id)],
      expecting: decoder,
    )
  {
    Ok([author_id]) -> Ok(author_id)
    Ok([]) -> Error("Entity not found")
    Ok(_) -> Error("Unexpected result")
    Error(e) -> Error(error_to_string(e))
  }
}

fn update_score(
  conn: sqlight.Connection,
  entity_type: EntityType,
  entity_id: Int,
) -> Result(Nil, String) {
  let et = entity_to_text(entity_type)
  let score_sql =
    "select coalesce(sum(value),0) from votes where entity_type = ? and entity_id = ?"
  let decoder = {
    use score <- decode.field(0, decode.int)
    decode.success(score)
  }
  case
    sqlight.query(
      score_sql,
      on: conn,
      with: [sqlight.text(et), sqlight.int(entity_id)],
      expecting: decoder,
    )
  {
    Ok([score]) -> {
      let update_sql = case entity_type {
        Post -> "update posts set score = ? where id = ?"
        Comment -> "update comments set score = ? where id = ?"
      }
      let decoder2 = decode.success(Nil)
      case
        sqlight.query(
          update_sql,
          on: conn,
          with: [sqlight.int(score), sqlight.int(entity_id)],
          expecting: decoder2,
        )
      {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(error_to_string(e))
      }
    }
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
