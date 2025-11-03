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
  JoinSubreddit(user_id: Int, subreddit_id: Int)
  LeaveSubreddit(user_id: Int, subreddit_id: Int)
  CreateSubreddit(user_id: Int, name: String)
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

    JoinSubreddit(user_id, subreddit_id) -> {
      case engine_api.join_subreddit(user_id, subreddit_id) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    LeaveSubreddit(user_id, subreddit_id) -> {
      case engine_api.leave_subreddit(user_id, subreddit_id) {
        Ok(_) -> Ok(Nil)
        Error(e) -> Error(e)
      }
    }

    CreateSubreddit(_user_id, name) -> {
      case engine_api.create_subreddit(name) {
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
    [] -> 1
    // Default to subreddit 1 if user has no subs
    [first, ..] -> first
    // Just use first subreddit for simplicity
  }

  // Simple round-robin through all action types to ensure diversity
  // Apply Zipf distribution by giving popular subreddits more post actions
  // But still cycle through all other action types

  let popularity_multiplier = float_to_int_approx(subreddit_popularity)

  // Determine which action type based on round-robin with popularity bias for posts
  // For popular subs (multiplier >= 2), do more posts
  // Action pattern: for every 12 actions, do (multiplier) posts and 12-multiplier other actions
  let cycle_length = 12
  let post_slots = case popularity_multiplier {
    n if n > 3 -> 3
    n if n < 1 -> 1
    n -> n
  }

  let action_slot = action_counter % cycle_length

  // Generate a plausible post_id based on action_counter (posts created earlier)
  // Use a small value to ensure we're targeting posts that likely exist
  let estimated_post_id = case action_counter > 2 {
    True -> { action_counter / 4 } + 1
    False -> 1
  }

  case action_slot < post_slots {
    True -> CreatePost(sub_id, user_id)
    False -> {
      // Cycle through other 11 action types evenly
      // Map slots [post_slots..11] to action types [0..10]
      let other_action_index = action_slot - post_slots

      case other_action_index % 11 {
        0 -> CreateComment(estimated_post_id, user_id)
        1 -> VotePost(estimated_post_id, user_id, 1)
        2 -> VoteComment(estimated_post_id, user_id, 1)
        3 -> SendDM(user_id, { user_id + 1 } % 100)
        4 -> CheckFeed(user_id)
        5 -> GetKarma(user_id)
        6 -> Repost(estimated_post_id, user_id)
        7 -> {
          // Join a random subreddit (1-10)
          let random_sub = { action_counter / 10 } % 10 + 1
          JoinSubreddit(user_id, random_sub)
        }
        8 -> {
          // Leave a subreddit they're in (if any)
          case user_subreddits {
            [] -> CheckFeed(user_id)
            [first, ..] -> LeaveSubreddit(user_id, first)
          }
        }
        9 -> {
          // Create a new subreddit
          let sub_name =
            "sub_user"
            <> int.to_string(user_id)
            <> "_"
            <> int.to_string(action_counter)
          CreateSubreddit(user_id, sub_name)
        }
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
