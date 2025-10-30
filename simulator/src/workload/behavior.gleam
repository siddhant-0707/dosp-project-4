import engine_api
import gleam/int
import gleam/option.{None}

/// User action types
pub type Action {
  CreatePost(subreddit_id: Int, user_id: Int)
  CreateComment(post_id: Int, user_id: Int)
  VotePost(post_id: Int, user_id: Int, value: Int)
  VoteComment(comment_id: Int, user_id: Int, value: Int)
  SendDM(from_user: Int, to_user: Int)
  CheckFeed(user_id: Int)
  GetKarma(user_id: Int)
  Repost(post_id: Int, user_id: Int)
}

/// Execute a user action and return success status
pub fn execute_action(action: Action) -> Result(Nil, String) {
  case action {
    CreatePost(subreddit_id, user_id) -> {
      let title = "Post by user " <> int.to_string(user_id)
      let body = "This is a post in subreddit " <> int.to_string(subreddit_id)
      case engine_api.create_post(subreddit_id, user_id, title, body) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    CreateComment(post_id, user_id) -> {
      let body = "Comment by user " <> int.to_string(user_id)
      case engine_api.create_comment(post_id, None, user_id, body) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    VotePost(post_id, user_id, value) -> {
      case engine_api.vote_post(post_id, user_id, value) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    VoteComment(comment_id, user_id, value) -> {
      case engine_api.vote_comment(comment_id, user_id, value) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    SendDM(from_user, to_user) -> {
      let body = "Message from " <> int.to_string(from_user)
      case engine_api.send_dm(from_user, to_user, body, None) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    CheckFeed(user_id) -> {
      case engine_api.feed_home(user_id, 20, engine_api.Hot) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    GetKarma(user_id) -> {
      case engine_api.get_karma(user_id) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    Repost(post_id, user_id) -> {
      case engine_api.create_repost(post_id, user_id) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }
  }
}

/// Generate a random action for a user based on probabilities
/// (Simplified: just cycle through actions for now)
/// subreddit_popularity is a multiplier (higher for popular subreddits)
pub fn generate_action(
  user_id: Int,
  user_subreddits: List(Int),
  action_counter: Int,
  subreddit_popularity: Float,
) -> Action {
  let sub_id = case user_subreddits {
    [] -> 0
    [first, ..] -> first
  }

  // Scale post frequency by subreddit popularity
  // Popular subreddits (high multiplier) will have more post actions
  let post_weight = float_to_int_approx(subreddit_popularity *. 3.0)
  let total_weight = post_weight + 8
  // 8 for other actions

  case action_counter % total_weight {
    n if n < post_weight -> CreatePost(sub_id, user_id)
    _ -> {
      // Other actions cycle through
      case action_counter % 8 {
        0 -> CreateComment(1, user_id)
        1 -> VotePost(1, user_id, 1)
        2 -> VoteComment(1, user_id, 1)
        3 -> SendDM(user_id, { user_id + 1 } % 100)
        4 -> CheckFeed(user_id)
        5 -> GetKarma(user_id)
        6 -> Repost(1, user_id)
        _ -> CheckFeed(user_id)
      }
    }
  }
}

fn float_to_int_approx(f: Float) -> Int {
  case f >=. 0.0 {
    True -> truncate_float(f)
    False -> 0 - truncate_float(0.0 -. f)
  }
}

@external(erlang, "erlang", "trunc")
fn truncate_float(f: Float) -> Int
