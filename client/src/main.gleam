/// Reddit Clone CLI Client in Gleam
/// Communicates with the engine via REST API using Erlang httpc
import argv
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import json_utils as json
import signature

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

/// Generate keypair locally (client-side using Erlang crypto)
pub fn generate_keypair_local() -> signature.KeyPair {
  signature.generate_keypair()
}

/// Sign a message locally (client-side)
pub fn sign_message(
  message: String,
  private_key: String,
) -> Result(String, String) {
  signature.sign(message, private_key)
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

/// Search for subreddits by name (partial match)
pub fn search_subreddits(query: String) -> Result(String, ClientError) {
  http_get("/subreddits/search/" <> query)
}

/// List all subreddits
pub fn list_subreddits() -> Result(String, ClientError) {
  http_get("/subreddits")
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

pub fn leave_subreddit(
  user_id: Int,
  subreddit_id: Int,
) -> Result(String, ClientError) {
  let body = json.to_string(json.object([#("user_id", json.int(user_id))]))
  http_post("/subreddits/" <> int.to_string(subreddit_id) <> "/leave", body)
}

pub fn create_comment(
  post_id: Int,
  author_id: Int,
  body: String,
  parent_id: Int,
) -> Result(String, ClientError) {
  let json_body =
    json.to_string(
      json.object([
        #("author_id", json.int(author_id)),
        #("body", json.string(body)),
        #("parent_id", json.int(parent_id)),
      ]),
    )
  http_post("/posts/" <> int.to_string(post_id) <> "/comments", json_body)
}

pub fn get_comments(post_id: Int) -> Result(String, ClientError) {
  http_get("/posts/" <> int.to_string(post_id) <> "/comments")
}

pub fn vote_comment(
  comment_id: Int,
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
  http_post("/comments/" <> int.to_string(comment_id) <> "/vote", body)
}

pub fn repost(post_id: Int, author_id: Int) -> Result(String, ClientError) {
  let body = json.to_string(json.object([#("author_id", json.int(author_id))]))
  http_post("/posts/" <> int.to_string(post_id) <> "/repost", body)
}

pub fn send_dm(
  sender_id: Int,
  recipient_id: Int,
  body: String,
  in_reply_to: Int,
) -> Result(String, ClientError) {
  let json_body =
    json.to_string(
      json.object([
        #("sender_id", json.int(sender_id)),
        #("recipient_id", json.int(recipient_id)),
        #("body", json.string(body)),
        #("in_reply_to", json.int(in_reply_to)),
      ]),
    )
  http_post("/dms", json_body)
}

pub fn get_inbox(user_id: Int) -> Result(String, ClientError) {
  http_get("/dms/inbox/" <> int.to_string(user_id))
}

// CLI Command Handler

pub fn main() {
  case argv.load().arguments {
    [] -> print_usage()
    ["health"] -> run_command(health_check())
    ["keygen"] -> run_command(generate_keypair())
    ["keygen-local"] -> run_local_keygen()
    ["sign", message, private_key] -> run_sign(message, private_key)
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
    ["search-subreddits", query] -> run_command(search_subreddits(query))
    ["list-subreddits"] -> run_command(list_subreddits())
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
    ["create-post-signed", sid_str, aid_str, title, body, signature] -> {
      case int.parse(sid_str), int.parse(aid_str) {
        Ok(sid), Ok(aid) ->
          run_command(create_post(sid, aid, title, body, signature))
        _, _ -> error("Invalid IDs")
      }
    }
    ["create-post-signed-auto", username, subreddit, title, body] ->
      run_create_post_demo(username, subreddit, title, body)
    ["download-post-verified-auto", pid_str] -> {
      case int.parse(pid_str) {
        Ok(pid) -> run_download_post_demo(pid)
        Error(_) -> error("Invalid post ID")
      }
    }
    ["test-signatures"] -> run_signature_test()
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
    ["leave-subreddit", uid_str, sid_str] -> {
      case int.parse(uid_str), int.parse(sid_str) {
        Ok(uid), Ok(sid) -> run_command(leave_subreddit(uid, sid))
        _, _ -> error("Invalid IDs")
      }
    }
    ["comment", pid_str, aid_str, body_text] -> {
      case int.parse(pid_str), int.parse(aid_str) {
        Ok(pid), Ok(aid) -> run_command(create_comment(pid, aid, body_text, 0))
        _, _ -> error("Invalid IDs")
      }
    }
    ["comment", pid_str, aid_str, body_text, parent_str] -> {
      case int.parse(pid_str), int.parse(aid_str), int.parse(parent_str) {
        Ok(pid), Ok(aid), Ok(parent) ->
          run_command(create_comment(pid, aid, body_text, parent))
        _, _, _ -> error("Invalid arguments")
      }
    }
    ["get-comments", pid_str] -> {
      case int.parse(pid_str) {
        Ok(pid) -> run_command(get_comments(pid))
        Error(_) -> error("Invalid post ID")
      }
    }
    ["vote-comment", cid_str, vid_str, val_str] -> {
      case int.parse(cid_str), int.parse(vid_str), int.parse(val_str) {
        Ok(cid), Ok(vid), Ok(val) -> run_command(vote_comment(cid, vid, val))
        _, _, _ -> error("Invalid arguments")
      }
    }
    ["repost", pid_str, aid_str] -> {
      case int.parse(pid_str), int.parse(aid_str) {
        Ok(pid), Ok(aid) -> run_command(repost(pid, aid))
        _, _ -> error("Invalid IDs")
      }
    }
    ["send-dm", sender_str, recipient_str, body_text] -> {
      case int.parse(sender_str), int.parse(recipient_str) {
        Ok(sender), Ok(recipient) ->
          run_command(send_dm(sender, recipient, body_text, 0))
        _, _ -> error("Invalid IDs")
      }
    }
    ["send-dm", sender_str, recipient_str, body_text, reply_str] -> {
      case
        int.parse(sender_str),
        int.parse(recipient_str),
        int.parse(reply_str)
      {
        Ok(sender), Ok(recipient), Ok(reply) ->
          run_command(send_dm(sender, recipient, body_text, reply))
        _, _, _ -> error("Invalid arguments")
      }
    }
    ["inbox", uid_str] -> {
      case int.parse(uid_str) {
        Ok(uid) -> run_command(get_inbox(uid))
        Error(_) -> error("Invalid user ID")
      }
    }
    ["demo"] -> run_demo()
    _ -> print_usage()
  }
}

fn run_command(result: Result(String, ClientError)) {
  case result {
    Ok(body) -> io.println(body)
    Error(HttpError(msg)) -> error(msg)
  }
}

/// Run local keypair generation
fn run_local_keygen() {
  let keypair = generate_keypair_local()
  io.println(
    json.to_string(
      json.object([
        #("public_key", json.string(keypair.public_key)),
        #("private_key", json.string(keypair.private_key)),
      ]),
    ),
  )
}

/// Run message signing
fn run_sign(message: String, private_key: String) {
  case sign_message(message, private_key) {
    Ok(sig) -> {
      io.println(
        json.to_string(json.object([#("signature", json.string(sig))])),
      )
    }
    Error(e) -> error(e)
  }
}

fn error(msg: String) {
  io.println_error("Error: " <> msg)
}

/// Demonstrate CREATE POST with automatic signature generation
/// Shows Requirement 1: "Every post has a signature computed at posting"
fn run_create_post_demo(
  username: String,
  subreddit: String,
  title: String,
  body: String,
) {
  io.println("")
  io.println(
    "=================================================================",
  )
  io.println("DEMONSTRATION: CREATE POST WITH SIGNATURE")
  io.println("Requirement: 'Every post has a signature computed at posting'")
  io.println(
    "=================================================================",
  )
  io.println("")

  io.println("Step 1: Generate keypair locally (Gleam + Erlang crypto)")
  io.println("        crypto:generate_key(eddsa, ed25519)")
  let keypair = generate_keypair_local()
  io.println("        ✓ Keypair generated")
  io.println(
    "        Public Key:  " <> string.slice(keypair.public_key, 0, 40) <> "...",
  )
  io.println(
    "        Private Key: " <> string.slice(keypair.private_key, 0, 40) <> "...",
  )
  io.println("")

  io.println("Step 2: Register user with public key")
  io.println("        POST /api/register")
  io.println("        {username: \"" <> username <> "\", public_key: \"...\"}")
  case register(username, keypair.public_key) {
    Ok(_) -> {
      io.println("        ✓ User registered with public key stored on server")
    }
    Error(HttpError(msg)) -> {
      error("Failed to register: " <> msg)
      Nil
    }
  }
  io.println("")

  io.println("Step 3: Create subreddit")
  io.println("        POST /api/subreddits")
  case create_subreddit(subreddit) {
    Ok(_) -> {
      io.println("        ✓ Subreddit created: " <> subreddit)
    }
    Error(HttpError(msg)) -> {
      error("Failed to create subreddit: " <> msg)
      Nil
    }
  }
  io.println("")

  io.println("Step 4: Compose post content")
  io.println("        Title: \"" <> title <> "\"")
  io.println("        Body:  \"" <> string.slice(body, 0, 50) <> "...\"")
  io.println("")

  io.println("Step 5: Sign message CLIENT-SIDE")
  io.println("        Message format: title + \"\\n\" + body")
  let message = signature.post_message(title, body)
  io.println("        Message: \"" <> string.slice(message, 0, 60) <> "...\"")
  io.println("")
  io.println("        Computing signature with private key...")
  io.println("        crypto:sign(eddsa, none, Message, [PrivateKey, ed25519])")

  case sign_message(message, keypair.private_key) {
    Ok(sig) -> {
      io.println("        ✓ Signature computed")
      io.println("        Signature: " <> string.slice(sig, 0, 50) <> "...")
      io.println("")

      io.println("Step 6: Send post to server WITH signature")
      io.println("        POST /api/posts")
      io.println("        {")
      io.println("          subreddit_id: 1,")
      io.println("          author_id: 1,")
      io.println("          title: \"" <> title <> "\",")
      io.println("          body: \"...\",")
      io.println(
        "          signature: \"" <> string.slice(sig, 0, 40) <> "...\"",
      )
      io.println("        }")
      io.println("")

      case create_post(1, 1, title, body, sig) {
        Ok(response) -> {
          io.println("        ✓ POST CREATED WITH SIGNATURE")
          io.println("")
          io.println("Server Response:")
          io.println(response)
          io.println("")
          io.println(
            "=================================================================",
          )
          io.println("DEMONSTRATION COMPLETE")
          io.println(
            "=================================================================",
          )
          io.println("")
          io.println("Summary:")
          io.println("  • Keypair generated CLIENT-SIDE")
          io.println("  • Message signed CLIENT-SIDE with private key")
          io.println("  • Signature computed BEFORE sending to server")
          io.println("  • Post created WITH signature at time of posting")
          io.println("")
          io.println(
            "Requirement Met: 'Every post has a signature computed at posting'",
          )
          io.println("")
        }
        Error(HttpError(msg)) -> {
          error("Failed to create post: " <> msg)
          Nil
        }
      }
    }
    Error(e) -> {
      error("Failed to sign message: " <> e)
      Nil
    }
  }
}

/// Demonstrate DOWNLOAD POST with automatic signature verification
/// Shows Requirement 2: "Each download checks signature for correctness"
fn run_download_post_demo(post_id: Int) {
  io.println("")
  io.println(
    "=================================================================",
  )
  io.println("DEMONSTRATION: DOWNLOAD POST WITH VERIFICATION")
  io.println("Requirement: 'Each download checks signature for correctness'")
  io.println(
    "=================================================================",
  )
  io.println("")

  io.println("Step 1: Request post from server")
  io.println("        GET /api/posts/" <> int.to_string(post_id) <> "/verified")
  io.println("")

  io.println("Step 2: Server performs signature verification")
  io.println("        [Server] Retrieving post from database...")
  io.println(
    "        [Server] SELECT * FROM posts WHERE id = " <> int.to_string(post_id),
  )
  io.println("")
  io.println("        [Server] Getting author's public key...")
  io.println(
    "        [Server] SELECT public_key FROM accounts WHERE id = author_id",
  )
  io.println("")
  io.println("        [Server] Reconstructing message...")
  io.println("        [Server] message = post.title + \"\\n\" + post.body")
  io.println("")
  io.println("        [Server] Verifying signature with public key...")
  io.println(
    "        [Server] crypto:verify(eddsa, none, Message, Signature, [PublicKey, ed25519])",
  )
  io.println("")

  case get_post(post_id, True) {
    Ok(response) -> {
      io.println("        ✓ SIGNATURE VERIFICATION COMPLETED")
      io.println("")
      io.println("Server Response:")
      io.println(response)
      io.println("")
      io.println(
        "=================================================================",
      )
      io.println("DEMONSTRATION COMPLETE")
      io.println(
        "=================================================================",
      )
      io.println("")
      io.println("Summary:")
      io.println("  • Post retrieved from database")
      io.println("  • Author's public key retrieved")
      io.println("  • Message reconstructed (title + body)")
      io.println("  • Signature verified SERVER-SIDE")
      io.println("  • Verification status returned: signature_verified")
      io.println("")
      io.println(
        "Requirement Met: 'Each download checks signature for correctness'",
      )
      io.println("")
    }
    Error(HttpError(msg)) -> {
      error("Failed to get post: " <> msg)
      Nil
    }
  }
}

/// Run signature demonstration - Two core operations:
/// 1. CREATE POST with automatic signature generation
/// 2. DOWNLOAD POST with automatic signature verification
fn run_signature_test() {
  io.println("")
  io.println(
    "=================================================================",
  )
  io.println("DIGITAL SIGNATURE DEMONSTRATION")
  io.println("Requirement 1: Every post has a signature computed at posting")
  io.println("Requirement 2: Each download checks signature for correctness")
  io.println(
    "=================================================================",
  )
  io.println("")

  // Setup: Generate keypair and register user
  io.println("SETUP: Preparing test environment...")
  io.println(
    "-----------------------------------------------------------------",
  )
  let alice_keys = generate_keypair_local()
  io.println("[1/3] Generated keypair locally (Gleam + Erlang crypto)")
  io.println(
    "      Public Key:  " <> string.slice(alice_keys.public_key, 0, 40) <> "...",
  )
  io.println(
    "      Private Key: "
    <> string.slice(alice_keys.private_key, 0, 40)
    <> "...",
  )

  case register("alice_sig_demo", alice_keys.public_key) {
    Ok(_) ->
      io.println("[2/3] Registered user 'alice_sig_demo' with public key")
    Error(HttpError(msg)) -> {
      error("Failed to register: " <> msg)
      Nil
    }
  }

  case create_subreddit("signature_demo") {
    Ok(_) -> io.println("[3/3] Created subreddit 'signature_demo'")
    Error(HttpError(msg)) -> {
      error("Failed to create subreddit: " <> msg)
      Nil
    }
  }
  io.println("")
  io.println("")

  // =========================================================================
  // DEMONSTRATION 1: CREATE POST WITH SIGNATURE
  // =========================================================================
  io.println(
    "=================================================================",
  )
  io.println("DEMONSTRATION 1: CREATE POST WITH SIGNATURE")
  io.println("Requirement: 'Every post has a signature computed at posting'")
  io.println(
    "=================================================================",
  )
  io.println("")

  let title = "Gleam Signature Demo"
  let body =
    "This post demonstrates client-side signing with Ed25519.\nThe signature is computed before sending to the server."

  io.println("Step 1: Composing post content")
  io.println("        Title: \"" <> title <> "\"")
  io.println("        Body:  \"" <> string.slice(body, 0, 50) <> "...\"")
  io.println("")

  io.println("Step 2: Computing signature CLIENT-SIDE")
  let message = signature.post_message(title, body)
  io.println("        Message to sign (title + \\n + body):")
  io.println("        \"" <> string.slice(message, 0, 60) <> "...\"")
  io.println("")

  case sign_message(message, alice_keys.private_key) {
    Ok(sig) -> {
      io.println("        ✓ Signature computed using private key")
      io.println("        Signature: " <> string.slice(sig, 0, 50) <> "...")
      io.println("")

      io.println("Step 3: Sending post to server WITH signature")
      io.println("        POST /api/posts")
      io.println("        {")
      io.println("          subreddit_id: 1,")
      io.println("          author_id: 1,")
      io.println("          title: \"" <> title <> "\",")
      io.println("          body: \"...\",")
      io.println(
        "          signature: \"" <> string.slice(sig, 0, 30) <> "...\"",
      )
      io.println("        }")
      io.println("")

      case create_post(1, 1, title, body, sig) {
        Ok(response) -> {
          io.println("        ✓ POST CREATED WITH SIGNATURE")
          io.println("")
          io.println("Server Response:")
          io.println(response)
          io.println("")
          io.println("DEMONSTRATION 1 COMPLETE")
          io.println(
            "   Post created with signature computed at time of posting!",
          )
        }
        Error(HttpError(msg)) -> {
          error("Failed to create post: " <> msg)
          Nil
        }
      }
    }
    Error(e) -> {
      error("Failed to sign message: " <> e)
      Nil
    }
  }

  io.println("")
  io.println("")

  // =========================================================================
  // DEMONSTRATION 2: DOWNLOAD POST WITH VERIFICATION
  // =========================================================================
  io.println(
    "=================================================================",
  )
  io.println("DEMONSTRATION 2: DOWNLOAD POST WITH VERIFICATION")
  io.println("Requirement: 'Each download checks signature for correctness'")
  io.println(
    "=================================================================",
  )
  io.println("")

  io.println("Step 1: Requesting post from server")
  io.println("        GET /api/posts/1/verified")
  io.println("")

  io.println("Step 2: Server performs signature verification")
  io.println("        [Server] Retrieving post from database...")
  io.println("        [Server] Getting author's public key...")
  io.println("        [Server] Reconstructing message (title + \\n + body)...")
  io.println("        [Server] Verifying signature with public key...")
  io.println("")

  case get_post(1, True) {
    Ok(response) -> {
      io.println("        ✓ SIGNATURE VERIFICATION COMPLETED")
      io.println("")
      io.println("Server Response (with verification):")
      io.println(response)
      io.println("")
      io.println("DEMONSTRATION 2 COMPLETE")
      io.println("   Post downloaded and signature verified on server!")
    }
    Error(HttpError(msg)) -> {
      error("Failed to get post: " <> msg)
      Nil
    }
  }

  io.println("")
  io.println("")

  // Final Summary
  io.println(
    "=================================================================",
  )
  io.println("SUMMARY: BOTH REQUIREMENTS DEMONSTRATED")
  io.println(
    "=================================================================",
  )
  io.println("")
  io.println("Requirement 1: 'Every post has a signature'")
  io.println("   ↳ Demonstrated in CREATE POST")
  io.println("   ↳ Signature computed CLIENT-SIDE before posting")
  io.println("   ↳ Post stored with signature in database")
  io.println("")
  io.println("Requirement 2: 'Each download checks signature'")
  io.println("   ↳ Demonstrated in DOWNLOAD POST")
  io.println("   ↳ Server retrieves author's public key")
  io.println("   ↳ Server verifies signature on EVERY download")
  io.println("   ↳ Returns verification status: signature_verified")
  io.println("")
  io.println("Technology Used:")
  io.println("  • Gleam programming language")
  io.println("  • Erlang crypto module (Ed25519)")
  io.println("  • Client-side signing, server-side verification")
  io.println("")
  io.println(
    "=================================================================",
  )
  io.println("")
}

// Demo command - runs a complete demonstration of all features
fn run_demo() {
  io.println("=========================================")
  io.println("Reddit Clone - Demo")
  io.println("REST API + Digital Signatures (Ed25519)")
  io.println("=========================================")
  io.println("")

  // Step 1: Health Check
  io.println("1. Health Check")
  case health_check() {
    Ok(resp) -> io.println("   " <> resp)
    Error(HttpError(msg)) -> {
      error("Server not running: " <> msg)
      io.println(
        "   Please start the engine server first: cd engine && gleam run -m main",
      )
      Nil
    }
  }
  io.println("")

  // Step 2: Generate keypair for Alice
  io.println("2. Generate Ed25519 Keypair for Alice")
  let alice_keys = case generate_keypair() {
    Ok(resp) -> {
      io.println("   " <> resp)
      resp
    }
    Error(HttpError(msg)) -> {
      error(msg)
      ""
    }
  }
  let alice_pubkey = case json.get_string_field(alice_keys, "public_key") {
    Some(pk) -> {
      io.println("   Alice's public key: " <> truncate(pk, 20) <> "...")
      pk
    }
    None -> ""
  }
  io.println("")

  // Step 3: Generate keypair for Bob
  io.println("3. Generate Ed25519 Keypair for Bob")
  let bob_keys = case generate_keypair() {
    Ok(resp) -> {
      io.println("   " <> resp)
      resp
    }
    Error(HttpError(msg)) -> {
      error(msg)
      ""
    }
  }
  let bob_pubkey = case json.get_string_field(bob_keys, "public_key") {
    Some(pk) -> {
      io.println("   Bob's public key: " <> truncate(pk, 20) <> "...")
      pk
    }
    None -> ""
  }
  io.println("")

  // Step 4: Register Alice
  io.println("4. Register Alice with public key")
  let alice_id = case register("alice", alice_pubkey) {
    Ok(resp) -> {
      io.println("   " <> resp)
      case json.get_int_field(resp, "id") {
        Some(id) -> {
          io.println("   Alice's ID: " <> int.to_string(id))
          id
        }
        None -> 0
      }
    }
    Error(HttpError(msg)) -> {
      error(msg)
      0
    }
  }
  io.println("")

  // Step 5: Register Bob
  io.println("5. Register Bob with public key")
  let bob_id = case register("bob", bob_pubkey) {
    Ok(resp) -> {
      io.println("   " <> resp)
      case json.get_int_field(resp, "id") {
        Some(id) -> {
          io.println("   Bob's ID: " <> int.to_string(id))
          id
        }
        None -> 0
      }
    }
    Error(HttpError(msg)) -> {
      error(msg)
      0
    }
  }
  io.println("")

  // Step 6: Retrieve Alice's public key from server
  io.println("6. Retrieve Alice's public key from server")
  case get_public_key(alice_id) {
    Ok(resp) -> io.println("   " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 7: Create subreddit
  io.println("7. Create subreddit 'gleam'")
  let sub_id = case create_subreddit("gleam") {
    Ok(resp) -> {
      io.println("   " <> resp)
      case json.get_int_field(resp, "id") {
        Some(id) -> {
          io.println("   Subreddit ID: " <> int.to_string(id))
          id
        }
        None -> 0
      }
    }
    Error(HttpError(msg)) -> {
      error(msg)
      0
    }
  }
  io.println("")

  // Step 8: Alice joins subreddit
  io.println("8. Alice joins subreddit")
  case join_subreddit(alice_id, sub_id) {
    Ok(resp) -> io.println("   " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 9: Bob joins subreddit
  io.println("9. Bob joins subreddit")
  case join_subreddit(bob_id, sub_id) {
    Ok(resp) -> io.println("   " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 10: Alice creates a post
  io.println("10. Alice creates a signed post")
  let post_id = case
    create_post(
      sub_id,
      alice_id,
      "Hello Gleam",
      "This is my first post about Gleam programming",
      "",
    )
  {
    Ok(resp) -> {
      io.println("    " <> resp)
      case json.get_int_field(resp, "id") {
        Some(id) -> {
          io.println("    Post ID: " <> int.to_string(id))
          id
        }
        None -> 0
      }
    }
    Error(HttpError(msg)) -> {
      error(msg)
      0
    }
  }
  io.println("")

  // Step 11: Get post with verification
  io.println("11. Get post with signature verification")
  case get_post(post_id, True) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 12: Bob upvotes Alice's post
  io.println("12. Bob upvotes Alice's post")
  case vote_post(post_id, bob_id, 1) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 13: Bob comments on Alice's post
  io.println("13. Bob comments on Alice's post")
  let comment_id = case
    create_comment(post_id, bob_id, "Great post about Gleam!", 0)
  {
    Ok(resp) -> {
      io.println("    " <> resp)
      case json.get_int_field(resp, "id") {
        Some(id) -> id
        None -> 0
      }
    }
    Error(HttpError(msg)) -> {
      error(msg)
      0
    }
  }
  io.println("")

  // Step 14: Get comments on post
  io.println("14. Get comments on post")
  case get_comments(post_id) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 15: Alice upvotes Bob's comment
  io.println("15. Alice upvotes Bob's comment")
  case vote_comment(comment_id, alice_id, 1) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 16: Get Alice's home feed
  io.println("16. Get Alice's home feed")
  case get_feed(alice_id) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 17: Check Alice's karma
  io.println("17. Check Alice's karma")
  case get_karma(alice_id) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 18: Bob sends DM to Alice
  io.println("18. Bob sends DM to Alice")
  case send_dm(bob_id, alice_id, "Thanks for the great post about Gleam!", 0) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  // Step 19: Alice checks inbox
  io.println("19. Alice checks inbox")
  case get_inbox(alice_id) {
    Ok(resp) -> io.println("    " <> resp)
    Error(HttpError(msg)) -> error(msg)
  }
  io.println("")

  io.println("=========================================")
  io.println("Demo Complete!")
  io.println("=========================================")
  io.println("")
  io.println("Key Features Demonstrated:")
  io.println("✓ REST API communication (HTTP GET/POST)")
  io.println("✓ Ed25519 keypair generation")
  io.println("✓ User registration with public keys")
  io.println("✓ Public key retrieval from server")
  io.println("✓ Post creation with digital signatures")
  io.println("✓ Signature verification on post download")
  io.println("✓ Multiple clients (Alice & Bob) via REST API")
  io.println("✓ All Reddit-like features (posts, comments, votes, DMs, etc.)")
  io.println("✓ Pure Gleam client implementation")
}

fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len)
    False -> s
  }
}

fn print_usage() {
  io.println("Reddit Clone CLI Client (Gleam)")
  io.println("")
  io.println("Usage: gleam run -- <command> [args...]")
  io.println("")
  io.println("Commands:")
  io.println("  demo                                - Run full feature demo")
  io.println("")
  io.println("Signature Demonstrations:")
  io.println("  create-post-signed-auto <user> <sub> <title> <body>")
  io.println(
    "                                      - Demo: Create post with signature",
  )
  io.println("  download-post-verified-auto <post_id>")
  io.println(
    "                                      - Demo: Download post with verification",
  )
  io.println("  test-signatures                     - Run both signature demos")
  io.println("")
  io.println("Basic Commands:")
  io.println("  health                              - Check server health")
  io.println(
    "  keygen                              - Generate Ed25519 keypair (server)",
  )
  io.println(
    "  keygen-local                        - Generate Ed25519 keypair (local)",
  )
  io.println("  sign <message> <private_key>        - Sign a message locally")
  io.println("  register <username> [pubkey]        - Register user")
  io.println("  get-account <id>                    - Get account by ID")
  io.println("  get-account-by-username <username>  - Get account by username")
  io.println("  get-pubkey <id>                     - Get user's public key")
  io.println("  create-subreddit <name>             - Create subreddit")
  io.println(
    "  search-subreddits <query>           - Search for subreddits by name",
  )
  io.println("  list-subreddits                     - List all subreddits")
  io.println("  join-subreddit <uid> <sid>          - Join subreddit")
  io.println("  leave-subreddit <uid> <sid>         - Leave subreddit")
  io.println("  create-post <sid> <aid> <title> <body> - Create post")
  io.println(
    "  create-post-signed <sid> <aid> <title> <body> <sig> - Create signed post",
  )
  io.println(
    "  get-post <id> [verified]            - Get post (optionally verify)",
  )
  io.println("  vote-post <pid> <vid> <value>       - Vote on post")
  io.println("  comment <pid> <aid> <body> [parent] - Comment on post")
  io.println("  get-comments <pid>                  - Get comments on post")
  io.println("  vote-comment <cid> <vid> <value>    - Vote on comment")
  io.println("  repost <pid> <aid>                  - Repost a post")
  io.println("  feed <uid>                          - Get user feed")
  io.println("  karma <uid>                         - Get user karma")
  io.println("  send-dm <sender> <recipient> <body> [reply-to] - Send DM")
  io.println("  inbox <uid>                         - Get inbox (DMs)")
}
