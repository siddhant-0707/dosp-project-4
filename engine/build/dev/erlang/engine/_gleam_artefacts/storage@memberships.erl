-module(storage@memberships).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/memberships.gleam").
-export([join/3, leave/3, subscribed_subreddit_ids/2]).

-file("src/storage/memberships.gleam", 62).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/memberships.gleam", 5).
-spec join(sqlight:connection(), integer(), integer()) -> {ok, nil} |
    {error, binary()}.
join(Conn, Account_id, Subreddit_id) ->
    Sql = <<"insert into memberships(account_id, subreddit_id, joined_at) values (?, ?, strftime('%s','now')*1000) on conflict(account_id, subreddit_id) do nothing;"/utf8>>,
    Decoder = gleam@dynamic@decode:success(nil),
    _pipe = sqlight:'query'(
        Sql,
        Conn,
        [sqlight:int(Account_id), sqlight:int(Subreddit_id)],
        Decoder
    ),
    _pipe@1 = gleam@result:map(_pipe, fun(_) -> nil end),
    gleam@result:map_error(_pipe@1, fun error_to_string/1).

-file("src/storage/memberships.gleam", 23).
-spec leave(sqlight:connection(), integer(), integer()) -> {ok, nil} |
    {error, binary()}.
leave(Conn, Account_id, Subreddit_id) ->
    Sql = <<"delete from memberships where account_id = ? and subreddit_id = ?;"/utf8>>,
    Decoder = gleam@dynamic@decode:success(nil),
    _pipe = sqlight:'query'(
        Sql,
        Conn,
        [sqlight:int(Account_id), sqlight:int(Subreddit_id)],
        Decoder
    ),
    _pipe@1 = gleam@result:map(_pipe, fun(_) -> nil end),
    gleam@result:map_error(_pipe@1, fun error_to_string/1).

-file("src/storage/memberships.gleam", 40).
-spec subscribed_subreddit_ids(sqlight:connection(), integer()) -> {ok,
        list(integer())} |
    {error, binary()}.
subscribed_subreddit_ids(Conn, Account_id) ->
    Sql = <<"select subreddit_id from memberships where account_id = ?"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) -> gleam@dynamic@decode:success(Id) end
        )
    end,
    case sqlight:'query'(Sql, Conn, [sqlight:int(Account_id)], Decoder) of
        {ok, Ids} ->
            {ok, Ids};

        {error, E} ->
            {error, error_to_string(E)}
    end.
