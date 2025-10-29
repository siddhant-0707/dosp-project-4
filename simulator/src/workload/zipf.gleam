import gleam/dict
import gleam/float
import gleam/int
import gleam/list

/// Zipf distribution generator for subreddit sizes
/// Uses the parameter s to control the skew (higher = more skewed)
/// Generate Zipf-distributed subreddit memberships
/// Returns a list of (subreddit_index, num_members) tuples
pub fn generate_memberships(
  num_users: Int,
  num_subreddits: Int,
  s: Float,
) -> List(#(Int, Int)) {
  // Calculate the normalization constant (sum of 1/k^s for k=1..n)
  let h_n = calculate_harmonic(num_subreddits, s)

  // Calculate probability for each subreddit rank
  let probabilities =
    list.range(1, num_subreddits)
    |> list.map(fn(rank) {
      let rank_f = int.to_float(rank)
      let assert Ok(power) = float.power(rank_f, s)
      let prob = 1.0 /. power /. h_n
      #(rank - 1, prob)
    })

  // Distribute users according to probabilities
  probabilities
  |> list.map(fn(pair) {
    let #(idx, prob) = pair
    let members = float.round(int.to_float(num_users) *. prob)
    #(idx, members)
  })
}

/// Calculate harmonic number H(n,s) = sum(1/k^s) for k=1..n
fn calculate_harmonic(n: Int, s: Float) -> Float {
  list.range(1, n)
  |> list.map(fn(k) {
    let assert Ok(power) = float.power(int.to_float(k), s)
    1.0 /. power
  })
  |> list.fold(0.0, fn(sum, val) { sum +. val })
}

/// Assign users to subreddits based on Zipf distribution
/// Returns a list of (user_id, List(subreddit_ids)) assignments
pub fn assign_users_to_subreddits(
  num_users: Int,
  subreddit_sizes: List(#(Int, Int)),
) -> List(#(Int, List(Int))) {
  // Create a pool of all user IDs
  let all_users = list.range(0, num_users - 1)

  // For each subreddit, randomly sample users
  // (Simplified: just take first N users for each subreddit)
  // In real implementation, would use proper random sampling

  // Build a map of user -> subreddits
  let user_subs =
    list.fold(subreddit_sizes, [], fn(acc, pair) {
      let #(sub_id, size) = pair
      let users_for_sub = list.take(all_users, size)

      // Add this subreddit to each user's list
      list.map(users_for_sub, fn(user_id) { #(user_id, sub_id) })
      |> list.append(acc, _)
    })

  // Group by user_id
  let grouped =
    user_subs
    |> list.group(fn(pair) {
      let #(user_id, _) = pair
      user_id
    })

  // Convert dict to list
  grouped
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(user_id, pairs) = pair
    let sub_ids =
      list.map(pairs, fn(p) {
        let #(_, sub_id) = p
        sub_id
      })
    #(user_id, sub_ids)
  })
}
