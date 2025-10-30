import engine_server.{type EngineMessage}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{None}
import gleam/otp/actor
import metrics/collector
import workload/behavior

/// Messages that a client actor can handle
pub type ClientMessage {
  PerformActions(
    actions_count: Int,
    user_subreddits: List(Int),
    popularity: Float,
  )
  GetMetrics(reply_to: Subject(collector.Collector))
  Shutdown
}

/// Client actor state
pub type ClientState {
  ClientState(
    user_id: Int,
    engine: Subject(EngineMessage),
    metrics: collector.Collector,
    action_counter: Int,
  )
}

/// Start a client actor
pub fn start(
  user_id: Int,
  engine: Subject(EngineMessage),
) -> Result(Subject(ClientMessage), actor.StartError) {
  let state =
    ClientState(
      user_id: user_id,
      engine: engine,
      metrics: collector.new(),
      action_counter: 0,
    )
  case
    actor.new(state)
    |> actor.on_message(handle_message)
    |> actor.start
  {
    Ok(actor.Started(_pid, subject)) -> Ok(subject)
    Error(e) -> Error(e)
  }
}

/// Handle incoming messages
fn handle_message(
  state: ClientState,
  message: ClientMessage,
) -> actor.Next(ClientState, ClientMessage) {
  case message {
    PerformActions(actions_count, user_subreddits, popularity) -> {
      // Perform N actions
      let new_state =
        perform_n_actions(state, actions_count, user_subreddits, popularity, 0)

      // Write metrics immediately after completing actions
      write_metrics(new_state.user_id, new_state.metrics)

      actor.continue(new_state)
    }

    GetMetrics(reply_to) -> {
      process.send(reply_to, state.metrics)
      actor.continue(state)
    }

    Shutdown -> {
      actor.stop()
    }
  }
}

/// Write client metrics to a file
fn write_metrics(user_id: Int, metrics: collector.Collector) -> Nil {
  let filename = "metrics/client_" <> int.to_string(user_id) <> ".csv"
  case collector.write_csv(metrics, filename) {
    Ok(_) -> Nil
    Error(_) -> Nil
  }
}

/// Perform N actions sequentially
fn perform_n_actions(
  state: ClientState,
  remaining: Int,
  user_subreddits: List(Int),
  popularity: Float,
  current_action: Int,
) -> ClientState {
  case remaining <= 0 {
    True -> state
    False -> {
      // Generate action
      let action =
        behavior.generate_action(
          state.user_id,
          user_subreddits,
          current_action,
          popularity,
        )

      // Execute action and measure time
      let start = current_time_ms()
      let result = execute_action_via_engine(action, state.engine)
      let latency = current_time_ms() - start

      let success = case result {
        Ok(_) -> True
        Error(_) -> False
      }

      // Record metrics
      let op_name = action_name(action)
      let new_metrics =
        collector.record(state.metrics, op_name, success, latency)

      // Continue with next action
      let new_state =
        ClientState(
          ..state,
          metrics: new_metrics,
          action_counter: current_action + 1,
        )

      perform_n_actions(
        new_state,
        remaining - 1,
        user_subreddits,
        popularity,
        current_action + 1,
      )
    }
  }
}

/// Execute an action by sending messages to the engine
fn execute_action_via_engine(
  action: behavior.Action,
  engine: Subject(EngineMessage),
) -> Result(Nil, String) {
  case action {
    behavior.CreatePost(subreddit_id, user_id) -> {
      let title = "Post by user " <> int.to_string(user_id)
      let body = "This is a post in subreddit " <> int.to_string(subreddit_id)

      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.CreatePost(subreddit_id, user_id, title, body, reply)
      })
      |> result_to_nil
    }

    behavior.CreateComment(post_id, user_id) -> {
      let body = "Comment by user " <> int.to_string(user_id)

      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.CreateComment(post_id, None, user_id, body, reply)
      })
      |> result_to_nil
    }

    behavior.VotePost(post_id, user_id, value) -> {
      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.VotePost(post_id, user_id, value, reply)
      })
      |> result_flatten
    }

    behavior.VoteComment(comment_id, user_id, value) -> {
      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.VoteComment(comment_id, user_id, value, reply)
      })
      |> result_flatten
    }

    behavior.SendDM(from_user, to_user) -> {
      let body = "Message from " <> int.to_string(from_user)

      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.SendDM(from_user, to_user, body, None, reply)
      })
      |> result_to_nil
    }

    behavior.CheckFeed(user_id) -> {
      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.GetHomeFeed(user_id, 20, "hot", reply)
      })
      |> result_to_nil
    }

    behavior.GetKarma(user_id) -> {
      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.GetKarma(user_id, reply)
      })
      |> result_to_nil
    }

    behavior.Repost(post_id, user_id) -> {
      process.call(engine, waiting: 5000, sending: fn(reply) {
        engine_server.CreateRepost(post_id, user_id, reply)
      })
      |> result_to_nil
    }
  }
}

/// Convert Result(T, String) to Result(Nil, String)
fn result_to_nil(result: Result(a, b)) -> Result(Nil, b) {
  case result {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

/// Flatten nested Result from call (just unwrap inner result)
fn result_flatten(result: Result(Nil, String)) -> Result(Nil, String) {
  result
}

/// Get action name for metrics
fn action_name(action: behavior.Action) -> String {
  case action {
    behavior.CreatePost(_, _) -> "create_post"
    behavior.CreateComment(_, _) -> "create_comment"
    behavior.VotePost(_, _, _) -> "vote_post"
    behavior.VoteComment(_, _, _) -> "vote_comment"
    behavior.SendDM(_, _) -> "send_dm"
    behavior.CheckFeed(_) -> "check_feed"
    behavior.GetKarma(_) -> "get_karma"
    behavior.Repost(_, _) -> "repost"
  }
}

type Atom

@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Atom) -> Int

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(s: String) -> Atom

fn current_time_ms() -> Int {
  erlang_system_time(binary_to_atom("millisecond"))
}
