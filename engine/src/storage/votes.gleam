import gleam/dynamic/decode
import sqlight

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
    Ok(_) -> update_score(conn, entity_type, entity_id)
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
