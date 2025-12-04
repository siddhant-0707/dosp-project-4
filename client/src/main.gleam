/// Reddit Clone CLI Client in Gleam
/// Communicates with the engine via REST API using Erlang httpc
import argv
import gleam/bit_array
import gleam/int
import gleam/io
import json_utils as json

const api_base_url = "http://localhost:8080/api"

pub type ClientError {
  HttpError(String)
}

// External Erlang HTTP functions
@external(erlang, "http_ffi", "http_get")
fn http_get_ffi(url: String) -> Result(BitArray, #(Int, BitArray))

@external(erlang, "http_ffi", "http_post")
fn http_post_ffi(
  url: String,
  body: BitArray,
) -> Result(BitArray, #(Int, BitArray))

/// Make HTTP GET request
fn http_get(endpoint: String) -> Result(String, ClientError) {
  let url = api_base_url <> endpoint
  case http_get_ffi(url) {
    Ok(body) -> {
      case bit_array.to_string(body) {
        Ok(s) -> Ok(s)
        Error(_) -> Error(HttpError("Invalid UTF-8 in response"))
      }
    }
    Error(#(status, body)) -> {
      let body_str = case bit_array.to_string(body) {
        Ok(s) -> s
        Error(_) -> "<binary>"
      }
      Error(HttpError("HTTP " <> int.to_string(status) <> ": " <> body_str))
    }
  }
}

/// Make HTTP POST request
fn http_post(endpoint: String, body: String) -> Result(String, ClientError) {
  let url = api_base_url <> endpoint
  let body_bits = bit_array.from_string(body)
  case http_post_ffi(url, body_bits) {
    Ok(resp_body) -> {
      case bit_array.to_string(resp_body) {
        Ok(s) -> Ok(s)
        Error(_) -> Error(HttpError("Invalid UTF-8 in response"))
      }
    }
    Error(#(status, resp_body)) -> {
      let body_str = case bit_array.to_string(resp_body) {
        Ok(s) -> s
        Error(_) -> "<binary>"
      }
      Error(HttpError("HTTP " <> int.to_string(status) <> ": " <> body_str))
    }
  }
}

// API Functions

pub fn health_check() -> Result(String, ClientError) {
  http_get("/health")
}

pub fn generate_keypair() -> Result(String, ClientError) {
  http_get("/crypto/generate_keypair")
}

pub fn register(
  username: String,
  public_key: String,
) -> Result(String, ClientError) {
  let body =
    json.to_string(
      json.object([
        #("username", json.string(username)),
        #("public_key", json.string(public_key)),
      ]),
    )
  http_post("/register", body)
}

pub fn get_account(id: Int) -> Result(String, ClientError) {
  http_get("/accounts/" <> int.to_string(id))
}

pub fn get_account_by_username(username: String) -> Result(String, ClientError) {
  http_get("/accounts/username/" <> username)
}

pub fn get_public_key(id: Int) -> Result(String, ClientError) {
  http_get("/accounts/" <> int.to_string(id) <> "/public_key")
}

pub fn create_subreddit(name: String) -> Result(String, ClientError) {
  let body = json.to_string(json.object([#("name", json.string(name))]))
  http_post("/subreddits", body)
}

pub fn join_subreddit(
  user_id: Int,
  subreddit_id: Int,
) -> Result(String, ClientError) {
  let body = json.to_string(json.object([#("user_id", json.int(user_id))]))
  http_post("/subreddits/" <> int.to_string(subreddit_id) <> "/join", body)
}

pub fn create_post(
  subreddit_id: Int,
  author_id: Int,
  title: String,
  body: String,
  signature: String,
) -> Result(String, ClientError) {
  let json_body =
    json.to_string(
      json.object([
        #("subreddit_id", json.int(subreddit_id)),
        #("author_id", json.int(author_id)),
        #("title", json.string(title)),
        #("body", json.string(body)),
        #("signature", json.string(signature)),
      ]),
    )
  http_post("/posts", json_body)
}

pub fn get_post(id: Int, verify: Bool) -> Result(String, ClientError) {
  case verify {
    True -> http_get("/posts/" <> int.to_string(id) <> "/verified")
    False -> http_get("/posts/" <> int.to_string(id))
  }
}

pub fn vote_post(
  post_id: Int,
  voter_id: Int,
  value: Int,
) -> Result(String, ClientError) {
  let body =
    json.to_string(
      json.object([
        #("voter_id", json.int(voter_id)),
        #("value", json.int(value)),
      ]),
    )
  http_post("/posts/" <> int.to_string(post_id) <> "/vote", body)
}

pub fn get_feed(user_id: Int) -> Result(String, ClientError) {
  http_get("/feed/" <> int.to_string(user_id))
}

pub fn get_karma(user_id: Int) -> Result(String, ClientError) {
  http_get("/karma/" <> int.to_string(user_id))
}

// CLI Command Handler

pub fn main() {
  case argv.load().arguments {
    [] -> print_usage()
    ["health"] -> run_command(health_check())
    ["keygen"] -> run_command(generate_keypair())
    ["register", username] -> run_command(register(username, ""))
    ["register", username, pubkey] -> run_command(register(username, pubkey))
    ["get-account", id_str] -> {
      case int.parse(id_str) {
        Ok(id) -> run_command(get_account(id))
        Error(_) -> error("Invalid account ID")
      }
    }
    ["get-account-by-username", username] ->
      run_command(get_account_by_username(username))
    ["get-pubkey", id_str] -> {
      case int.parse(id_str) {
        Ok(id) -> run_command(get_public_key(id))
        Error(_) -> error("Invalid account ID")
      }
    }
    ["create-subreddit", name] -> run_command(create_subreddit(name))
    ["join-subreddit", uid_str, sid_str] -> {
      case int.parse(uid_str), int.parse(sid_str) {
        Ok(uid), Ok(sid) -> run_command(join_subreddit(uid, sid))
        _, _ -> error("Invalid IDs")
      }
    }
    ["create-post", sid_str, aid_str, title, body] -> {
      case int.parse(sid_str), int.parse(aid_str) {
        Ok(sid), Ok(aid) -> run_command(create_post(sid, aid, title, body, ""))
        _, _ -> error("Invalid IDs")
      }
    }
    ["get-post", pid_str] -> {
      case int.parse(pid_str) {
        Ok(pid) -> run_command(get_post(pid, False))
        Error(_) -> error("Invalid post ID")
      }
    }
    ["get-post", pid_str, "verified"] -> {
      case int.parse(pid_str) {
        Ok(pid) -> run_command(get_post(pid, True))
        Error(_) -> error("Invalid post ID")
      }
    }
    ["vote-post", pid_str, vid_str, val_str] -> {
      case int.parse(pid_str), int.parse(vid_str), int.parse(val_str) {
        Ok(pid), Ok(vid), Ok(val) -> run_command(vote_post(pid, vid, val))
        _, _, _ -> error("Invalid arguments")
      }
    }
    ["feed", uid_str] -> {
      case int.parse(uid_str) {
        Ok(uid) -> run_command(get_feed(uid))
        Error(_) -> error("Invalid user ID")
      }
    }
    ["karma", uid_str] -> {
      case int.parse(uid_str) {
        Ok(uid) -> run_command(get_karma(uid))
        Error(_) -> error("Invalid user ID")
      }
    }
    _ -> print_usage()
  }
}

fn run_command(result: Result(String, ClientError)) {
  case result {
    Ok(body) -> io.println(body)
    Error(HttpError(msg)) -> error(msg)
  }
}

fn error(msg: String) {
  io.println_error("Error: " <> msg)
}

fn print_usage() {
  io.println("Reddit Clone CLI Client (Gleam)")
  io.println("")
  io.println("Usage: gleam run -- <command> [args...]")
  io.println("")
  io.println("Commands:")
  io.println("  health                              - Check server health")
  io.println("  keygen                              - Generate Ed25519 keypair")
  io.println("  register <username> [pubkey]        - Register user")
  io.println("  get-account <id>                    - Get account by ID")
  io.println("  get-account-by-username <username>  - Get account by username")
  io.println("  get-pubkey <id>                     - Get user's public key")
  io.println("  create-subreddit <name>             - Create subreddit")
  io.println("  join-subreddit <uid> <sid>          - Join subreddit")
  io.println("  create-post <sid> <aid> <title> <body> - Create post")
  io.println(
    "  get-post <id> [verified]            - Get post (optionally verify)",
  )
  io.println("  vote-post <pid> <vid> <value>       - Vote on post")
  io.println("  feed <uid>                          - Get user feed")
  io.println("  karma <uid>                         - Get user karma")
}
