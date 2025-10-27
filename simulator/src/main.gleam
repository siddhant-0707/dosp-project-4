import engine/engine_api
import gleam/int
import gleam/io
import gleam/list

pub fn main() {
  // simple smoke test
  let assert Ok(user) = engine_api.register("alice")
  let assert Ok(sr) = engine_api.create_subreddit("gleam")
  let assert Ok(Nil) = engine_api.join_subreddit(user.id, sr.id)
  let assert Ok(_post) =
    engine_api.create_post(sr.id, user.id, "Hello", "First post")
  let assert Ok(feed) = engine_api.feed_home(user.id, 10, engine_api.Hot)
  io.println(
    "simulator smoke test ok, posts in feed: "
    <> int.to_string(list.length(feed)),
  )
}
