import gleam/dynamic/decode
import gleam/option.{type Option}
import sqlight

pub type Comment {
  Comment(
    id: Int,
    post_id: Int,
    parent_comment_id: Option(Int),
    author_id: Int,
    body: String,
    score: Int,
    created_at: Int,
  )
}

pub fn create(
  conn: sqlight.Connection,
  post_id: Int,
  parent_comment_id: Option(Int),
  author_id: Int,
  body: String,
) -> Result(Comment, String) {
  let sql =
    "insert into comments(post_id, parent_comment_id, author_id, body, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000) returning id, post_id, parent_comment_id, author_id, body, score, created_at;"
  let parent = sqlight.nullable(sqlight.int, parent_comment_id)
  let decoder = {
    use id <- decode.field(0, decode.int)
    use p <- decode.field(1, decode.int)
    use parent_id <- decode.field(2, decode.optional(decode.int))
    use a <- decode.field(3, decode.int)
    use b <- decode.field(4, decode.string)
    use s <- decode.field(5, decode.int)
    use c <- decode.field(6, decode.int)
    decode.success(Comment(
      id: id,
      post_id: p,
      parent_comment_id: parent_id,
      author_id: a,
      body: b,
      score: s,
      created_at: c,
    ))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [
        sqlight.int(post_id),
        parent,
        sqlight.int(author_id),
        sqlight.text(body),
      ],
      expecting: decoder,
    )
  {
    Ok([comment]) -> Ok(comment)
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

pub fn list_for_post(
  conn: sqlight.Connection,
  post_id: Int,
) -> Result(List(Comment), String) {
  let sql =
    "select id, post_id, parent_comment_id, author_id, body, score, created_at from comments where post_id = ? order by created_at asc"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use p <- decode.field(1, decode.int)
    use parent <- decode.field(2, decode.optional(decode.int))
    use a <- decode.field(3, decode.int)
    use b <- decode.field(4, decode.string)
    use s <- decode.field(5, decode.int)
    use c <- decode.field(6, decode.int)
    decode.success(Comment(
      id: id,
      post_id: p,
      parent_comment_id: parent,
      author_id: a,
      body: b,
      score: s,
      created_at: c,
    ))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [sqlight.int(post_id)],
      expecting: decoder,
    )
  {
    Ok(comments) -> Ok(comments)
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
