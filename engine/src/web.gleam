/// REST API Server for Reddit Clone
/// Uses Erlang's built-in inets/httpd for HTTP handling
import engine_api
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import json_utils as json
import signature
import storage/accounts
import storage/comments
import storage/dms
import storage/posts
import storage/subreddits

/// Start the REST API server on the given port
pub fn start(port: Int) {
  io.println(
    "[REST API] Starting server on http://localhost:" <> int.to_string(port),
  )
  start_http_server(port)
}

@external(erlang, "web_server", "start")
fn start_http_server(port: Int) -> Nil

// ============= Request Handlers (called from Erlang) =============

/// Health check endpoint
pub fn handle_health() -> String {
  json.to_string(json.object([#("status", json.string("ok"))]))
}

/// Generate keypair endpoint
pub fn handle_generate_keypair() -> String {
  let keypair = signature.generate_keypair()
  io.println("[REST API] Generated new Ed25519 keypair")
  json.to_string(
    json.object([
      #("public_key", json.string(keypair.public_key)),
      #("private_key", json.string(keypair.private_key)),
    ]),
  )
}

/// Register user endpoint
pub fn handle_register(username: String, public_key: String) -> String {
  let result = case public_key {
    "" -> engine_api.register(username)
    pk -> engine_api.register_with_key(username, pk)
  }
  case result {
    Ok(account) -> {
      io.println("[REST API] Registered user: " <> account.username)
      json.to_string(account_to_json(account))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get account by ID
pub fn handle_get_account(id: Int) -> String {
  case engine_api.get_account(id) {
    Ok(Some(account)) -> json.to_string(account_to_json(account))
    Ok(None) -> json.to_string(error_json("Account not found"))
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get account by username
pub fn handle_get_account_by_username(username: String) -> String {
  case engine_api.get_account_by_username(username) {
    Ok(Some(account)) -> json.to_string(account_to_json(account))
    Ok(None) -> json.to_string(error_json("Account not found"))
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get public key for user
pub fn handle_get_public_key(id: Int) -> String {
  case engine_api.get_public_key(id) {
    Ok(Some(pk)) ->
      json.to_string(json.object([#("public_key", json.string(pk))]))
    Ok(None) -> json.to_string(json.object([#("public_key", json.null())]))
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Create subreddit
pub fn handle_create_subreddit(name: String) -> String {
  case engine_api.create_subreddit(name) {
    Ok(sr) -> {
      io.println("[REST API] Created subreddit: " <> sr.name)
      json.to_string(subreddit_to_json(sr))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Join subreddit
pub fn handle_join_subreddit(user_id: Int, subreddit_id: Int) -> String {
  case engine_api.join_subreddit(user_id, subreddit_id) {
    Ok(_) -> {
      io.println(
        "[REST API] User "
        <> int.to_string(user_id)
        <> " joined subreddit "
        <> int.to_string(subreddit_id),
      )
      json.to_string(json.object([#("success", json.bool(True))]))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Leave subreddit
pub fn handle_leave_subreddit(user_id: Int, subreddit_id: Int) -> String {
  case engine_api.leave_subreddit(user_id, subreddit_id) {
    Ok(_) -> {
      io.println(
        "[REST API] User "
        <> int.to_string(user_id)
        <> " left subreddit "
        <> int.to_string(subreddit_id),
      )
      json.to_string(json.object([#("success", json.bool(True))]))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Create post
pub fn handle_create_post(
  subreddit_id: Int,
  author_id: Int,
  title: String,
  body: String,
  signature: String,
) -> String {
  let result = case signature {
    "" -> engine_api.create_post(subreddit_id, author_id, title, body)
    sig ->
      engine_api.create_post_signed(subreddit_id, author_id, title, body, sig)
  }
  case result {
    Ok(post) -> {
      io.println(
        "[REST API] Created post: "
        <> post.title
        <> " (id: "
        <> int.to_string(post.id)
        <> ")",
      )
      json.to_string(post_to_json(post))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get post by ID
pub fn handle_get_post(id: Int) -> String {
  case engine_api.get_post(id) {
    Ok(Some(post)) -> json.to_string(post_to_json(post))
    Ok(None) -> json.to_string(error_json("Post not found"))
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get post with signature verification
pub fn handle_get_post_verified(id: Int) -> String {
  case engine_api.get_post_verified(id) {
    Ok(#(post, verified)) -> {
      io.println(
        "[REST API] Post "
        <> int.to_string(id)
        <> " signature verified: "
        <> bool_to_string(verified),
      )
      json.to_string(
        json.object([
          #("post", post_to_json(post)),
          #("signature_verified", json.bool(verified)),
        ]),
      )
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

/// Vote on post
pub fn handle_vote_post(post_id: Int, voter_id: Int, value: Int) -> String {
  case engine_api.vote_post(post_id, voter_id, value) {
    Ok(_) -> {
      io.println(
        "[REST API] User "
        <> int.to_string(voter_id)
        <> " voted "
        <> int.to_string(value)
        <> " on post "
        <> int.to_string(post_id),
      )
      json.to_string(json.object([#("success", json.bool(True))]))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Create comment
pub fn handle_create_comment(
  post_id: Int,
  author_id: Int,
  body: String,
  parent_id: Int,
) -> String {
  let parent = case parent_id {
    0 -> None
    id -> Some(id)
  }
  case engine_api.create_comment(post_id, parent, author_id, body) {
    Ok(comment) -> {
      io.println(
        "[REST API] Created comment on post " <> int.to_string(post_id),
      )
      json.to_string(comment_to_json(comment))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get comments for post
pub fn handle_get_comments(post_id: Int) -> String {
  case engine_api.get_comments(post_id) {
    Ok(cmts) -> {
      json.to_string(
        json.object([#("comments", json.array(cmts, comment_to_json))]),
      )
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Vote on comment
pub fn handle_vote_comment(comment_id: Int, voter_id: Int, value: Int) -> String {
  case engine_api.vote_comment(comment_id, voter_id, value) {
    Ok(_) -> {
      io.println(
        "[REST API] User "
        <> int.to_string(voter_id)
        <> " voted "
        <> int.to_string(value)
        <> " on comment "
        <> int.to_string(comment_id),
      )
      json.to_string(json.object([#("success", json.bool(True))]))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Repost
pub fn handle_repost(post_id: Int, author_id: Int) -> String {
  case engine_api.create_repost(post_id, author_id) {
    Ok(post) -> {
      io.println(
        "[REST API] Reposted post "
        <> int.to_string(post_id)
        <> " by user "
        <> int.to_string(author_id),
      )
      json.to_string(post_to_json(post))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get user feed
pub fn handle_feed(user_id: Int, limit: Int) -> String {
  case engine_api.feed_home(user_id, limit, engine_api.Hot) {
    Ok(feed_posts) -> {
      json.to_string(
        json.object([#("posts", json.array(feed_posts, post_to_json))]),
      )
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Send DM
pub fn handle_send_dm(
  sender_id: Int,
  recipient_id: Int,
  body: String,
  in_reply_to: Int,
) -> String {
  let reply = case in_reply_to {
    0 -> None
    id -> Some(id)
  }
  case engine_api.send_dm(sender_id, recipient_id, body, reply) {
    Ok(dm) -> {
      io.println(
        "[REST API] DM sent from "
        <> int.to_string(sender_id)
        <> " to "
        <> int.to_string(recipient_id),
      )
      json.to_string(dm_to_json(dm))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get inbox
pub fn handle_inbox(user_id: Int, limit: Int) -> String {
  case engine_api.list_dms(user_id, limit, None) {
    Ok(messages) -> {
      json.to_string(
        json.object([#("messages", json.array(messages, dm_to_json))]),
      )
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

/// Get karma
pub fn handle_karma(user_id: Int) -> String {
  case engine_api.get_karma(user_id) {
    Ok(karma) -> {
      json.to_string(json.object([#("karma", json.int(karma))]))
    }
    Error(e) -> json.to_string(error_json(e))
  }
}

// ============= JSON Helpers =============

fn error_json(message: String) -> json.Json {
  json.object([#("error", json.string(message))])
}

fn account_to_json(account: accounts.Account) -> json.Json {
  json.object([
    #("id", json.int(account.id)),
    #("username", json.string(account.username)),
    #("created_at", json.int(account.created_at)),
    #("karma", json.int(account.karma)),
    #("public_key", case account.public_key {
      Some(pk) -> json.string(pk)
      None -> json.null()
    }),
  ])
}

fn subreddit_to_json(sr: subreddits.Subreddit) -> json.Json {
  json.object([
    #("id", json.int(sr.id)),
    #("name", json.string(sr.name)),
    #("created_at", json.int(sr.created_at)),
  ])
}

fn post_to_json(post: posts.Post) -> json.Json {
  json.object([
    #("id", json.int(post.id)),
    #("subreddit_id", json.int(post.subreddit_id)),
    #("author_id", json.int(post.author_id)),
    #("title", json.string(post.title)),
    #("body", json.string(post.body)),
    #("score", json.int(post.score)),
    #("created_at", json.int(post.created_at)),
    #("is_repost", json.bool(post.is_repost)),
    #("original_post_id", case post.original_post_id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("signature", case post.signature {
      Some(sig) -> json.string(sig)
      None -> json.null()
    }),
  ])
}

fn comment_to_json(c: comments.Comment) -> json.Json {
  json.object([
    #("id", json.int(c.id)),
    #("post_id", json.int(c.post_id)),
    #("parent_comment_id", case c.parent_comment_id {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
    #("author_id", json.int(c.author_id)),
    #("body", json.string(c.body)),
    #("score", json.int(c.score)),
    #("created_at", json.int(c.created_at)),
  ])
}

fn dm_to_json(dm: dms.DM) -> json.Json {
  json.object([
    #("id", json.int(dm.id)),
    #("sender_id", json.int(dm.sender_id)),
    #("recipient_id", json.int(dm.recipient_id)),
    #("body", json.string(dm.body)),
    #("created_at", json.int(dm.created_at)),
    #("in_reply_to", case dm.in_reply_to {
      Some(id) -> json.int(id)
      None -> json.null()
    }),
  ])
}
