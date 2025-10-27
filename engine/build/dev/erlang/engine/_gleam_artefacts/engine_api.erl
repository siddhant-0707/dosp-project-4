-module(engine_api).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/engine_api.gleam").
-export([register/1, create_subreddit/1, join_subreddit/2, leave_subreddit/2, create_post/4, create_comment/4, vote_post/3, vote_comment/3, feed_home/3, send_dm/4, list_dms/3]).
-export_type([feed_algo/0]).

-type feed_algo() :: hot.

-file("src/engine_api.gleam", 15).
-spec register(binary()) -> {ok, storage@accounts:account()} | {error, binary()}.
register(Username) ->
    storage@db:with_engine_connection(
        fun(Conn) -> storage@accounts:create(Conn, Username) end
    ).

-file("src/engine_api.gleam", 19).
-spec create_subreddit(binary()) -> {ok, storage@subreddits:subreddit()} |
    {error, binary()}.
create_subreddit(Name) ->
    storage@db:with_engine_connection(
        fun(Conn) -> storage@subreddits:create(Conn, Name) end
    ).

-file("src/engine_api.gleam", 23).
-spec join_subreddit(integer(), integer()) -> {ok, nil} | {error, binary()}.
join_subreddit(Account_id, Subreddit_id) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@memberships:join(Conn, Account_id, Subreddit_id)
        end
    ).

-file("src/engine_api.gleam", 29).
-spec leave_subreddit(integer(), integer()) -> {ok, nil} | {error, binary()}.
leave_subreddit(Account_id, Subreddit_id) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@memberships:leave(Conn, Account_id, Subreddit_id)
        end
    ).

-file("src/engine_api.gleam", 38).
-spec create_post(integer(), integer(), binary(), binary()) -> {ok,
        storage@posts:post()} |
    {error, binary()}.
create_post(Subreddit_id, Author_id, Title, Body) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@posts:create(Conn, Subreddit_id, Author_id, Title, Body)
        end
    ).

-file("src/engine_api.gleam", 49).
-spec create_comment(
    integer(),
    gleam@option:option(integer()),
    integer(),
    binary()
) -> {ok, storage@comments:comment()} | {error, binary()}.
create_comment(Post_id, Parent_comment_id, Author_id, Body) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@comments:create(
                Conn,
                Post_id,
                Parent_comment_id,
                Author_id,
                Body
            )
        end
    ).

-file("src/engine_api.gleam", 60).
-spec vote_post(integer(), integer(), integer()) -> {ok, nil} |
    {error, binary()}.
vote_post(Post_id, Voter_id, Value) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@votes:upsert(Conn, post, Post_id, Voter_id, Value)
        end
    ).

-file("src/engine_api.gleam", 66).
-spec vote_comment(integer(), integer(), integer()) -> {ok, nil} |
    {error, binary()}.
vote_comment(Comment_id, Voter_id, Value) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@votes:upsert(Conn, comment, Comment_id, Voter_id, Value)
        end
    ).

-file("src/engine_api.gleam", 76).
-spec feed_home(integer(), integer(), feed_algo()) -> {ok,
        list(storage@posts:post())} |
    {error, binary()}.
feed_home(Account_id, Limit, _) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            case storage@memberships:subscribed_subreddit_ids(Conn, Account_id) of
                {ok, Ids} ->
                    storage@posts:list_for_subreddits(Conn, Ids, Limit);

                {error, E} ->
                    {error, E}
            end
        end
    ).

-file("src/engine_api.gleam", 89).
-spec send_dm(integer(), integer(), binary(), gleam@option:option(integer())) -> {ok,
        storage@dms:d_m()} |
    {error, binary()}.
send_dm(Sender_id, Recipient_id, Body, In_reply_to) ->
    storage@db:with_engine_connection(
        fun(Conn) ->
            storage@dms:send(Conn, Sender_id, Recipient_id, Body, In_reply_to)
        end
    ).

-file("src/engine_api.gleam", 100).
-spec list_dms(integer(), integer(), gleam@option:option(integer())) -> {ok,
        list(storage@dms:d_m())} |
    {error, binary()}.
list_dms(Recipient_id, Limit, After_id) ->
    storage@db:with_engine_connection(
        fun(Conn) -> storage@dms:inbox(Conn, Recipient_id, Limit, After_id) end
    ).
