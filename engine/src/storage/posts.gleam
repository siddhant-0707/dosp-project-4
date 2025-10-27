import gleam/dynamic/decode
import gleam/list
import gleam/string
import sqlight

pub type Post {
  Post(
    id: Int,
    subreddit_id: Int,
    author_id: Int,
    title: String,
    body: String,
    score: Int,
    created_at: Int,
  )
}

pub fn create(
  conn: sqlight.Connection,
  subreddit_id: Int,
  author_id: Int,
  title: String,
  body: String,
) -> Result(Post, String) {
  let sql =
    "insert into posts(subreddit_id, author_id, title, body, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000) returning id, subreddit_id, author_id, title, body, score, created_at;"
  let decoder = {
    use id <- decode.field(0, decode.int)
    use s <- decode.field(1, decode.int)
    use a <- decode.field(2, decode.int)
    use t <- decode.field(3, decode.string)
    use b <- decode.field(4, decode.string)
    use sc <- decode.field(5, decode.int)
    use c <- decode.field(6, decode.int)
    decode.success(Post(
      id: id,
      subreddit_id: s,
      author_id: a,
      title: t,
      body: b,
      score: sc,
      created_at: c,
    ))
  }
  case
    sqlight.query(
      sql,
      on: conn,
      with: [
        sqlight.int(subreddit_id),
        sqlight.int(author_id),
        sqlight.text(title),
        sqlight.text(body),
      ],
      expecting: decoder,
    )
  {
    Ok([post]) -> Ok(post)
    Ok(_) -> Error("Expected exactly one row")
    Error(e) -> Error(error_to_string(e))
  }
}

pub fn list_for_subreddits(
  conn: sqlight.Connection,
  subreddit_ids: List(Int),
  limit: Int,
) -> Result(List(Post), String) {
  case subreddit_ids {
    [] -> Ok([])
    _ -> build_and_query(conn, subreddit_ids, limit)
  }
}

fn build_and_query(
  conn: sqlight.Connection,
  subreddit_ids: List(Int),
  limit: Int,
) -> Result(List(Post), String) {
  let placeholders =
    subreddit_ids |> list.map(fn(_) { "?" }) |> string.join(", ")
  let sql =
    "select id, subreddit_id, author_id, title, body, score, created_at from posts where subreddit_id in ("
    <> placeholders
    <> ") order by score desc, created_at desc limit ?"
  let args =
    subreddit_ids |> list.map(sqlight.int) |> list.append([sqlight.int(limit)])
  let decoder = {
    use id <- decode.field(0, decode.int)
    use s <- decode.field(1, decode.int)
    use a <- decode.field(2, decode.int)
    use t <- decode.field(3, decode.string)
    use b <- decode.field(4, decode.string)
    use sc <- decode.field(5, decode.int)
    use c <- decode.field(6, decode.int)
    decode.success(Post(
      id: id,
      subreddit_id: s,
      author_id: a,
      title: t,
      body: b,
      score: sc,
      created_at: c,
    ))
  }
  case sqlight.query(sql, on: conn, with: args, expecting: decoder) {
    Ok(posts) -> Ok(posts)
    Error(e) -> Error(error_to_string(e))
  }
}

fn error_to_string(e: sqlight.Error) -> String {
  case e {
    sqlight.SqlightError(_, message, _) -> message
  }
}
