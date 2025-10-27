import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import sqlight

pub type DM {
  DM(
    id: Int,
    sender_id: Int,
    recipient_id: Int,
    body: String,
    created_at: Int,
    in_reply_to: Option(Int),
  )
}

pub fn send(
  conn: sqlight.Connection,
  sender_id: Int,
  recipient_id: Int,
  body: String,
  in_reply_to: Option(Int),
) -> Result(DM, String) {
  let parent = sqlight.nullable(sqlight.int, in_reply_to)
  let sql =
    "insert into dms(sender_id, recipient_id, body, created_at, in_reply_to) values (?, ?, ?, strftime('%s','now')*1000, ?) returning id, sender_id, recipient_id, body, created_at, in_reply_to;"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use s <- decode.field(1, decode.int)
    use r <- decode.field(2, decode.int)
    use b <- decode.field(3, decode.string)
    use c <- decode.field(4, decode.int)
    use p <- decode.field(5, decode.optional(decode.int))
    decode.success(DM(
      id: id,
      sender_id: s,
      recipient_id: r,
      body: b,
      created_at: c,
      in_reply_to: p,
    ))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [
        sqlight.int(sender_id),
        sqlight.int(recipient_id),
        sqlight.text(body),
        parent,
      ],
      expecting: decoder,
    )
  {
    Ok([dm]) -> Ok(dm)
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

pub fn inbox(
  conn: sqlight.Connection,
  recipient_id: Int,
  limit: Int,
  after_id: Option(Int),
) -> Result(List(DM), String) {
  let base =
    "select id, sender_id, recipient_id, body, created_at, in_reply_to from dms where recipient_id = ?"
  let cond = case after_id {
    Some(_) -> " and id < ?"
    None -> ""
  }
  let sql = base <> cond <> " order by id desc limit ?"
  let args = case after_id {
    Some(id) -> [sqlight.int(recipient_id), sqlight.int(id), sqlight.int(limit)]
    None -> [sqlight.int(recipient_id), sqlight.int(limit)]
  }
  let decoder = {
    use id <- decode.field(0, decode.int)
    use s <- decode.field(1, decode.int)
    use r <- decode.field(2, decode.int)
    use b <- decode.field(3, decode.string)
    use c <- decode.field(4, decode.int)
    use p <- decode.field(5, decode.optional(decode.int))
    decode.success(DM(
      id: id,
      sender_id: s,
      recipient_id: r,
      body: b,
      created_at: c,
      in_reply_to: p,
    ))
  }
  case sqlight.query(sql, on: conn, with: args, expecting: decoder) {
    Ok(dms) -> Ok(dms)
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
