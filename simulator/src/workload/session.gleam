import gleam/list

/// Session state for a user
pub type SessionState {
  Online
  Offline
}

/// Session event representing a state transition
pub type SessionEvent {
  GoOnline(user_id: Int, duration_secs: Int)
  GoOffline(user_id: Int, duration_secs: Int)
}

/// Generate initial session states for all users
/// (Simplified: all start online)
pub fn initial_states(num_users: Int) -> List(#(Int, SessionState)) {
  list_range(0, num_users - 1)
  |> list.map(fn(user_id) { #(user_id, Online) })
}

/// Generate session transitions based on exponential distributions
/// For simplicity, we alternate between online/offline with fixed durations
pub fn generate_transitions(
  user_id: Int,
  current_state: SessionState,
  online_mean: Float,
  offline_mean: Float,
  _seed: Int,
) -> SessionEvent {
  case current_state {
    Online -> {
      // Go offline after online_mean seconds (simplified)
      let duration = float_to_int(offline_mean)
      GoOffline(user_id, duration)
    }
    Offline -> {
      // Go online after offline_mean seconds (simplified)
      let duration = float_to_int(online_mean)
      GoOnline(user_id, duration)
    }
  }
}

fn float_to_int(f: Float) -> Int {
  let _ = f
  // Simplified: just return a fixed value
  60
}

fn list_range(start: Int, end: Int) -> List(Int) {
  case start <= end {
    True -> list_range_helper(start, end, [])
    False -> []
  }
}

fn list_range_helper(current: Int, end: Int, acc: List(Int)) -> List(Int) {
  case current > end {
    True -> list.reverse(acc)
    False -> list_range_helper(current + 1, end, [current, ..acc])
  }
}
