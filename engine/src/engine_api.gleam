import gleam/option.{type Option, None, Some}
import signature
import storage/accounts
import storage/comments
import storage/db
import storage/dms
import storage/memberships
import storage/posts
import storage/subreddits
import storage/votes

pub type FeedAlgo {
  Hot
}

/// Register without public key (legacy)
pub fn register(username: String) -> Result(accounts.Account, String) {
  db.with_engine_connection(fn(conn) { accounts.create(conn, username) })
}

/// Register with public key for digital signatures
pub fn register_with_key(
  username: String,
  public_key: String,
) -> Result(accounts.Account, String) {
  db.with_engine_connection(fn(conn) {
    accounts.create_with_key(conn, username, Some(public_key))
  })
}

/// Get public key for a user (for signature verification)
pub fn get_public_key(account_id: Int) -> Result(Option(String), String) {
  db.with_engine_connection(fn(conn) {
    accounts.get_public_key(conn, account_id)
  })
}

/// Get account by username
pub fn get_account_by_username(
  username: String,
) -> Result(Option(accounts.Account), String) {
  db.with_engine_connection(fn(conn) { accounts.by_username(conn, username) })
}

/// Get account by ID
pub fn get_account(account_id: Int) -> Result(Option(accounts.Account), String) {
  db.with_engine_connection(fn(conn) { accounts.by_id(conn, account_id) })
}

pub fn create_subreddit(name: String) -> Result(subreddits.Subreddit, String) {
  db.with_engine_connection(fn(conn) { subreddits.create(conn, name) })
}

pub fn join_subreddit(account_id: Int, subreddit_id: Int) -> Result(Nil, String) {
  db.with_engine_connection(fn(conn) {
    memberships.join(conn, account_id, subreddit_id)
  })
}

pub fn leave_subreddit(
  account_id: Int,
  subreddit_id: Int,
) -> Result(Nil, String) {
  db.with_engine_connection(fn(conn) {
    memberships.leave(conn, account_id, subreddit_id)
  })
}

/// Create post without signature (legacy)
pub fn create_post(
  subreddit_id: Int,
  author_id: Int,
  title: String,
  body: String,
) -> Result(posts.Post, String) {
  db.with_engine_connection(fn(conn) {
    posts.create(conn, subreddit_id, author_id, title, body)
  })
}

/// Create post with digital signature
pub fn create_post_signed(
  subreddit_id: Int,
  author_id: Int,
  title: String,
  body: String,
  signature: String,
) -> Result(posts.Post, String) {
  db.with_engine_connection(fn(conn) {
    posts.create_with_signature(
      conn,
      subreddit_id,
      author_id,
      title,
      body,
      Some(signature),
    )
  })
}

/// Get a post by ID
pub fn get_post(post_id: Int) -> Result(Option(posts.Post), String) {
  db.with_engine_connection(fn(conn) { posts.by_id(conn, post_id) })
}

/// Get a post and verify its signature
/// Returns the post along with signature verification status
pub fn get_post_verified(post_id: Int) -> Result(#(posts.Post, Bool), String) {
  db.with_engine_connection(fn(conn) {
    case posts.by_id(conn, post_id) {
      Ok(Some(post)) -> {
        // Verify signature if present
        let verified = case post.signature {
          Some(sig) -> {
            case accounts.get_public_key(conn, post.author_id) {
              Ok(Some(pub_key)) -> {
                let message = signature.post_message(post.title, post.body)
                case signature.verify(message, sig, pub_key) {
                  Ok(True) -> True
                  _ -> False
                }
              }
              _ -> False
            }
          }
          None -> False
          // No signature = not verified
        }
        Ok(#(post, verified))
      }
      Ok(None) -> Error("Post not found")
      Error(e) -> Error(e)
    }
  })
}

/// Get comments for a post
pub fn get_comments(post_id: Int) -> Result(List(comments.Comment), String) {
  db.with_engine_connection(fn(conn) { comments.list_for_post(conn, post_id) })
}

pub fn create_comment(
  post_id: Int,
  parent_comment_id: Option(Int),
  author_id: Int,
  body: String,
) -> Result(comments.Comment, String) {
  db.with_engine_connection(fn(conn) {
    comments.create(conn, post_id, parent_comment_id, author_id, body)
  })
}

pub fn vote_post(post_id: Int, voter_id: Int, value: Int) -> Result(Nil, String) {
  db.with_engine_connection(fn(conn) {
    votes.upsert(conn, votes.Post, post_id, voter_id, value)
  })
}

pub fn vote_comment(
  comment_id: Int,
  voter_id: Int,
  value: Int,
) -> Result(Nil, String) {
  db.with_engine_connection(fn(conn) {
    votes.upsert(conn, votes.Comment, comment_id, voter_id, value)
  })
}

pub fn feed_home(
  account_id: Int,
  limit: Int,
  _algo: FeedAlgo,
) -> Result(List(posts.Post), String) {
  db.with_engine_connection(fn(conn) {
    case memberships.subscribed_subreddit_ids(conn, account_id) {
      Ok(ids) -> posts.list_for_subreddits(conn, ids, limit)
      Error(e) -> Error(e)
    }
  })
}

pub fn send_dm(
  sender_id: Int,
  recipient_id: Int,
  body: String,
  in_reply_to: Option(Int),
) -> Result(dms.DM, String) {
  db.with_engine_connection(fn(conn) {
    dms.send(conn, sender_id, recipient_id, body, in_reply_to)
  })
}

pub fn list_dms(
  recipient_id: Int,
  limit: Int,
  after_id: Option(Int),
) -> Result(List(dms.DM), String) {
  db.with_engine_connection(fn(conn) {
    dms.inbox(conn, recipient_id, limit, after_id)
  })
}

/// Get karma for an account
pub fn get_karma(account_id: Int) -> Result(Int, String) {
  db.with_engine_connection(fn(conn) { accounts.get_karma(conn, account_id) })
}

/// Create a repost
pub fn create_repost(
  original_post_id: Int,
  author_id: Int,
) -> Result(posts.Post, String) {
  db.with_engine_connection(fn(conn) {
    posts.create_repost(conn, original_post_id, author_id)
  })
}
