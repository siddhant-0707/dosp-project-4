import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/otp/actor
import storage/accounts
import storage/comments
import storage/db
import storage/dms
import storage/memberships
import storage/posts
import storage/subreddits
import storage/votes

/// Messages that the engine server can handle
pub type EngineMessage {
  // Account operations
  Register(
    username: String,
    reply_to: Subject(Result(accounts.Account, String)),
  )
  GetKarma(account_id: Int, reply_to: Subject(Result(Int, String)))

  // Subreddit operations
  CreateSubreddit(
    name: String,
    reply_to: Subject(Result(subreddits.Subreddit, String)),
  )
  JoinSubreddit(
    user_id: Int,
    subreddit_id: Int,
    reply_to: Subject(Result(Nil, String)),
  )

  // Post operations
  CreatePost(
    subreddit_id: Int,
    author_id: Int,
    title: String,
    body: String,
    reply_to: Subject(Result(posts.Post, String)),
  )
  CreateRepost(
    original_post_id: Int,
    author_id: Int,
    reply_to: Subject(Result(posts.Post, String)),
  )

  // Comment operations
  CreateComment(
    post_id: Int,
    parent_id: Option(Int),
    author_id: Int,
    body: String,
    reply_to: Subject(Result(comments.Comment, String)),
  )

  // Vote operations
  VotePost(
    post_id: Int,
    voter_id: Int,
    value: Int,
    reply_to: Subject(Result(Nil, String)),
  )
  VoteComment(
    comment_id: Int,
    voter_id: Int,
    value: Int,
    reply_to: Subject(Result(Nil, String)),
  )

  // Feed operations
  GetHomeFeed(
    user_id: Int,
    limit: Int,
    sort: String,
    reply_to: Subject(Result(List(posts.Post), String)),
  )

  // DM operations
  SendDM(
    from_user: Int,
    to_user: Int,
    body: String,
    in_reply_to: Option(Int),
    reply_to: Subject(Result(dms.DM, String)),
  )

  // Shutdown
  Shutdown(reply_to: Subject(Nil))
}

/// Start the engine server actor
pub fn start() -> Result(Subject(EngineMessage), actor.StartError) {
  case
    actor.new(Nil)
    |> actor.on_message(handle_message)
    |> actor.start
  {
    Ok(actor.Started(_pid, subject)) -> Ok(subject)
    Error(e) -> Error(e)
  }
}

/// Handle incoming messages
fn handle_message(
  state: Nil,
  message: EngineMessage,
) -> actor.Next(Nil, EngineMessage) {
  case message {
    Register(username, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) { accounts.create(conn, username) })
      process.send(reply_to, result)
      actor.continue(state)
    }

    GetKarma(account_id, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          accounts.get_karma(conn, account_id)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    CreateSubreddit(name, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) { subreddits.create(conn, name) })
      process.send(reply_to, result)
      actor.continue(state)
    }

    JoinSubreddit(user_id, subreddit_id, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          memberships.join(conn, user_id, subreddit_id)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    CreatePost(subreddit_id, author_id, title, body, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          posts.create(conn, subreddit_id, author_id, title, body)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    CreateRepost(original_post_id, author_id, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          posts.create_repost(conn, original_post_id, author_id)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    CreateComment(post_id, parent_id, author_id, body, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          comments.create(conn, post_id, parent_id, author_id, body)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    VotePost(post_id, voter_id, value, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          votes.upsert(conn, votes.Post, post_id, voter_id, value)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    VoteComment(comment_id, voter_id, value, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          votes.upsert(conn, votes.Comment, comment_id, voter_id, value)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    GetHomeFeed(user_id, limit, _sort, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          // Get user's subscribed subreddits
          case memberships.subscribed_subreddit_ids(conn, user_id) {
            Ok(subreddit_ids) -> {
              // Get posts from those subreddits (no sort order in current API)
              posts.list_for_subreddits(conn, subreddit_ids, limit)
            }
            Error(e) -> Error(e)
          }
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    SendDM(from_user, to_user, body, in_reply_to, reply_to) -> {
      let result =
        db.with_engine_connection(fn(conn) {
          dms.send(conn, from_user, to_user, body, in_reply_to)
        })
      process.send(reply_to, result)
      actor.continue(state)
    }

    Shutdown(reply_to) -> {
      process.send(reply_to, Nil)
      actor.stop()
    }
  }
}
