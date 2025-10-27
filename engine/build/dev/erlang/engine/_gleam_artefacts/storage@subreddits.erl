-module(storage@subreddits).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/subreddits.gleam").
-export([create/2]).
-export_type([subreddit/0]).

-type subreddit() :: {subreddit, integer(), binary(), integer()}.

-file("src/storage/subreddits.gleam", 29).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/subreddits.gleam", 8).
-spec create(sqlight:connection(), binary()) -> {ok, subreddit()} |
    {error, binary()}.
create(Conn, Name) ->
    Sql = <<"insert into subreddits(name, created_at) values (?, strftime('%s','now')*1000) returning id, name, created_at;"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(N) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(Created) ->
                                gleam@dynamic@decode:success(
                                    {subreddit, Id, N, Created}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case sqlight:'query'(Sql, Conn, [sqlight:text(Name)], Decoder) of
        {ok, [Subreddit]} ->
            {ok, Subreddit};

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.
