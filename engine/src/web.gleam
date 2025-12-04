/// REST API Server for Reddit Clone using Wisp
import engine_api
import gleam/dynamic
import gleam/dynamic/decode
import gleam/http.{Get, Post}
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import signature
import storage/accounts
import storage/comments
import storage/dms
import storage/posts
import storage/subreddits
import wisp.{type Request, type Response}

pub type Context {
  Context
}

/// Main request handler with Wisp middleware
pub fn handle_request(req: Request, _ctx: Context) -> Response {
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)

  case wisp.path_segments(req) {
    ["api", "health"] -> handle_health(req)
    ["api", "crypto", "generate_keypair"] -> handle_generate_keypair(req)
    ["api", "register"] -> handle_register(req)
    ["api", "accounts", id] -> handle_get_account(req, id)
    ["api", "accounts", "username", username] ->
      handle_get_account_by_username(req, username)
    ["api", "accounts", id, "public_key"] -> handle_get_public_key(req, id)
    ["api", "subreddits"] -> handle_create_subreddit(req)
    ["api", "subreddits", id, "join"] -> handle_join_subreddit(req, id)
    ["api", "subreddits", id, "leave"] -> handle_leave_subreddit(req, id)
    ["api", "posts"] -> handle_create_post(req)
    ["api", "posts", id] -> handle_get_post(req, id)
    ["api", "posts", id, "verified"] -> handle_get_post_verified(req, id)
    ["api", "posts", id, "vote"] -> handle_vote_post(req, id)
    ["api", "posts", id, "comments"] -> handle_post_comments(req, id)
    ["api", "comments", id, "vote"] -> handle_vote_comment(req, id)
    ["api", "feed", user_id] -> handle_feed(req, user_id)
    ["api", "dms"] -> handle_send_dm(req)
    ["api", "dms", "inbox", user_id] -> handle_inbox(req, user_id)
    ["api", "karma", user_id] -> handle_karma(req, user_id)
    _ -> wisp.not_found()
  }
}

// ============= Request Handlers =============

fn handle_health(_req: Request) -> Response {
  json_response(200, json.object([#("status", json.string("ok"))]))
}

fn handle_generate_keypair(_req: Request) -> Response {
  let keypair = signature.generate_keypair()
  io.println("[REST API] Generated new Ed25519 keypair")
  json_response(
    200,
    json.object([
      #("public_key", json.string(keypair.public_key)),
      #("private_key", json.string(keypair.private_key)),
    ]),
  )
}

fn handle_register(req: Request) -> Response {
  use json <- require_json(req)
  use username <- require_string(json, "username")
  use public_key <- require_string_optional(json, "public_key")

  let pk = option.unwrap(public_key, "")
  let result = case pk {
    "" -> engine_api.register(username)
    _ -> engine_api.register_with_key(username, pk)
  }

  case result {
    Ok(account) -> {
      io.println("[REST API] Registered user: " <> account.username)
      json_response(201, account_to_json(account))
    }
    Error(e) -> error_response(400, e)
  }
}

fn handle_get_account(_req: Request, id_str: String) -> Response {
  case int.parse(id_str) {
    Ok(id) -> {
      case engine_api.get_account(id) {
        Ok(Some(account)) -> json_response(200, account_to_json(account))
        Ok(None) -> error_response(404, "Account not found")
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid account ID")
  }
}

fn handle_get_account_by_username(_req: Request, username: String) -> Response {
  case engine_api.get_account_by_username(username) {
    Ok(Some(account)) -> json_response(200, account_to_json(account))
    Ok(None) -> error_response(404, "Account not found")
    Error(e) -> error_response(500, e)
  }
}

fn handle_get_public_key(_req: Request, id_str: String) -> Response {
  case int.parse(id_str) {
    Ok(id) -> {
      case engine_api.get_public_key(id) {
        Ok(Some(pk)) ->
          json_response(200, json.object([#("public_key", json.string(pk))]))
        Ok(None) ->
          json_response(200, json.object([#("public_key", json.null())]))
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid account ID")
  }
}

fn handle_create_subreddit(req: Request) -> Response {
  use json <- require_json(req)
  use name <- require_string(json, "name")

  case engine_api.create_subreddit(name) {
    Ok(sr) -> {
      io.println("[REST API] Created subreddit: " <> sr.name)
      json_response(201, subreddit_to_json(sr))
    }
    Error(e) -> error_response(400, e)
  }
}

fn handle_join_subreddit(req: Request, sub_id_str: String) -> Response {
  case int.parse(sub_id_str) {
    Ok(sub_id) -> {
      use json <- require_json(req)
      use user_id <- require_int(json, "user_id")

      case engine_api.join_subreddit(user_id, sub_id) {
        Ok(_) -> {
          io.println(
            "[REST API] User "
            <> int.to_string(user_id)
            <> " joined subreddit "
            <> int.to_string(sub_id),
          )
          json_response(200, json.object([#("success", json.bool(True))]))
        }
        Error(e) -> error_response(400, e)
      }
    }
    Error(_) -> error_response(400, "Invalid subreddit ID")
  }
}

fn handle_leave_subreddit(req: Request, sub_id_str: String) -> Response {
  case int.parse(sub_id_str) {
    Ok(sub_id) -> {
      use json <- require_json(req)
      use user_id <- require_int(json, "user_id")

      case engine_api.leave_subreddit(user_id, sub_id) {
        Ok(_) -> {
          io.println(
            "[REST API] User "
            <> int.to_string(user_id)
            <> " left subreddit "
            <> int.to_string(sub_id),
          )
          json_response(200, json.object([#("success", json.bool(True))]))
        }
        Error(e) -> error_response(400, e)
      }
    }
    Error(_) -> error_response(400, "Invalid subreddit ID")
  }
}

fn handle_create_post(req: Request) -> Response {
  use json <- require_json(req)
  use subreddit_id <- require_int(json, "subreddit_id")
  use author_id <- require_int(json, "author_id")
  use title <- require_string(json, "title")
  use body <- require_string(json, "body")
  use signature <- require_string_optional(json, "signature")

  let sig = option.unwrap(signature, "")
  let result = case sig {
    "" -> engine_api.create_post(subreddit_id, author_id, title, body)
    _ ->
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
      json_response(201, post_to_json(post))
    }
    Error(e) -> error_response(400, e)
  }
}

fn handle_get_post(_req: Request, id_str: String) -> Response {
  case int.parse(id_str) {
    Ok(id) -> {
      case engine_api.get_post(id) {
        Ok(Some(post)) -> json_response(200, post_to_json(post))
        Ok(None) -> error_response(404, "Post not found")
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid post ID")
  }
}

fn handle_get_post_verified(_req: Request, id_str: String) -> Response {
  case int.parse(id_str) {
    Ok(id) -> {
      case engine_api.get_post_verified(id) {
        Ok(#(post, verified)) -> {
          io.println(
            "[REST API] Post "
            <> int.to_string(id)
            <> " signature verified: "
            <> bool_to_string(verified),
          )
          json_response(
            200,
            json.object([
              #("post", post_to_json(post)),
              #("signature_verified", json.bool(verified)),
            ]),
          )
        }
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid post ID")
  }
}

fn handle_vote_post(req: Request, post_id_str: String) -> Response {
  case int.parse(post_id_str) {
    Ok(post_id) -> {
      use json <- require_json(req)
      use voter_id <- require_int(json, "voter_id")
      use value <- require_int(json, "value")

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
          json_response(200, json.object([#("success", json.bool(True))]))
        }
        Error(e) -> error_response(400, e)
      }
    }
    Error(_) -> error_response(400, "Invalid post ID")
  }
}

fn handle_post_comments(req: Request, post_id_str: String) -> Response {
  case int.parse(post_id_str) {
    Ok(post_id) -> {
      case req.method {
        Post -> {
          use json <- require_json(req)
          use author_id <- require_int(json, "author_id")
          use body <- require_string(json, "body")
          use parent_id <- require_int_optional(json, "parent_id")

          let parent = case option.unwrap(parent_id, 0) {
            0 -> None
            id -> Some(id)
          }

          case engine_api.create_comment(post_id, parent, author_id, body) {
            Ok(comment) -> {
              io.println(
                "[REST API] Created comment on post " <> int.to_string(post_id),
              )
              json_response(201, comment_to_json(comment))
            }
            Error(e) -> error_response(400, e)
          }
        }
        Get -> {
          case engine_api.get_comments(post_id) {
            Ok(cmts) -> {
              json_response(
                200,
                json.object([
                  #("comments", json.array(cmts, comment_to_json)),
                ]),
              )
            }
            Error(e) -> error_response(500, e)
          }
        }
        _ -> wisp.method_not_allowed([Post, Get])
      }
    }
    Error(_) -> error_response(400, "Invalid post ID")
  }
}

fn handle_vote_comment(req: Request, comment_id_str: String) -> Response {
  case int.parse(comment_id_str) {
    Ok(comment_id) -> {
      use json <- require_json(req)
      use voter_id <- require_int(json, "voter_id")
      use value <- require_int(json, "value")

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
          json_response(200, json.object([#("success", json.bool(True))]))
        }
        Error(e) -> error_response(400, e)
      }
    }
    Error(_) -> error_response(400, "Invalid comment ID")
  }
}

fn handle_feed(_req: Request, user_id_str: String) -> Response {
  case int.parse(user_id_str) {
    Ok(user_id) -> {
      case engine_api.feed_home(user_id, 20, engine_api.Hot) {
        Ok(feed_posts) -> {
          json_response(
            200,
            json.object([#("posts", json.array(feed_posts, post_to_json))]),
          )
        }
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid user ID")
  }
}

fn handle_send_dm(req: Request) -> Response {
  use json <- require_json(req)
  use sender_id <- require_int(json, "sender_id")
  use recipient_id <- require_int(json, "recipient_id")
  use body <- require_string(json, "body")
  use in_reply_to <- require_int_optional(json, "in_reply_to")

  let reply = case option.unwrap(in_reply_to, 0) {
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
      json_response(201, dm_to_json(dm))
    }
    Error(e) -> error_response(400, e)
  }
}

fn handle_inbox(_req: Request, user_id_str: String) -> Response {
  case int.parse(user_id_str) {
    Ok(user_id) -> {
      case engine_api.list_dms(user_id, 50, None) {
        Ok(messages) -> {
          json_response(
            200,
            json.object([#("messages", json.array(messages, dm_to_json))]),
          )
        }
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid user ID")
  }
}

fn handle_karma(_req: Request, user_id_str: String) -> Response {
  case int.parse(user_id_str) {
    Ok(user_id) -> {
      case engine_api.get_karma(user_id) {
        Ok(karma) -> {
          json_response(200, json.object([#("karma", json.int(karma))]))
        }
        Error(e) -> error_response(500, e)
      }
    }
    Error(_) -> error_response(400, "Invalid user ID")
  }
}

// ============= Helper Functions =============

fn json_response(status: Int, json_value: json.Json) -> Response {
  wisp.json_response(json.to_string(json_value), status)
}

fn error_response(status: Int, message: String) -> Response {
  json_response(status, json.object([#("error", json.string(message))]))
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

// ============= JSON Decoders =============

fn require_json(req: Request, next: fn(dynamic.Dynamic) -> Response) -> Response {
  case wisp.read_body_bits(req) {
    Ok(body) -> {
      // Parse JSON using gleam_json with dynamic decoder
      case json.parse_bits(body, decode.dynamic) {
        Ok(json_value) -> next(json_value)
        Error(_) -> error_response(400, "Invalid JSON in request body")
      }
    }
    Error(_) -> error_response(400, "Could not read request body")
  }
}

fn require_string(
  json: dynamic.Dynamic,
  field: String,
  next: fn(String) -> Response,
) -> Response {
  let decoder = decode.at([field], decode.string)
  case decode.run(json, decoder) {
    Ok(value) -> next(value)
    Error(_) -> error_response(400, "Missing or invalid field: " <> field)
  }
}

fn require_string_optional(
  json: dynamic.Dynamic,
  field: String,
  next: fn(Option(String)) -> Response,
) -> Response {
  let decoder = decode.optional(decode.at([field], decode.string))
  case decode.run(json, decoder) {
    Ok(value) -> next(value)
    Error(_) -> next(None)
  }
}

fn require_int(
  json: dynamic.Dynamic,
  field: String,
  next: fn(Int) -> Response,
) -> Response {
  let decoder = decode.at([field], decode.int)
  case decode.run(json, decoder) {
    Ok(value) -> next(value)
    Error(_) -> error_response(400, "Missing or invalid field: " <> field)
  }
}

fn require_int_optional(
  json: dynamic.Dynamic,
  field: String,
  next: fn(Option(Int)) -> Response,
) -> Response {
  let decoder = decode.optional(decode.at([field], decode.int))
  case decode.run(json, decoder) {
    Ok(value) -> next(value)
    Error(_) -> next(None)
  }
}

// ============= JSON Encoders =============

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
