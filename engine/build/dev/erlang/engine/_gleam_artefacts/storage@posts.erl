-module(storage@posts).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/posts.gleam").
-export([create/5, list_for_subreddits/3]).
-export_type([post/0]).

-type post() :: {post,
        integer(),
        integer(),
        integer(),
        binary(),
        binary(),
        integer(),
        integer()}.

-file("src/storage/posts.gleam", 112).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/posts.gleam", 18).
-spec create(sqlight:connection(), integer(), integer(), binary(), binary()) -> {ok,
        post()} |
    {error, binary()}.
create(Conn, Subreddit_id, Author_id, Title, Body) ->
    Sql = <<"insert into posts(subreddit_id, author_id, title, body, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000) returning id, subreddit_id, author_id, title, body, score, created_at;"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(S) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(A) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    fun(T) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1},
                                            fun(B) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    {decoder,
                                                        fun gleam@dynamic@decode:decode_int/1},
                                                    fun(Sc) ->
                                                        gleam@dynamic@decode:field(
                                                            6,
                                                            {decoder,
                                                                fun gleam@dynamic@decode:decode_int/1},
                                                            fun(C) ->
                                                                gleam@dynamic@decode:success(
                                                                    {post,
                                                                        Id,
                                                                        S,
                                                                        A,
                                                                        T,
                                                                        B,
                                                                        Sc,
                                                                        C}
                                                                )
                                                            end
                                                        )
                                                    end
                                                )
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case sqlight:'query'(
        Sql,
        Conn,
        [sqlight:int(Subreddit_id),
            sqlight:int(Author_id),
            sqlight:text(Title),
            sqlight:text(Body)],
        Decoder
    ) of
        {ok, [Post]} ->
            {ok, Post};

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.

-file("src/storage/posts.gleam", 75).
-spec build_and_query(sqlight:connection(), list(integer()), integer()) -> {ok,
        list(post())} |
    {error, binary()}.
build_and_query(Conn, Subreddit_ids, Limit) ->
    Placeholders = begin
        _pipe = Subreddit_ids,
        _pipe@1 = gleam@list:map(_pipe, fun(_) -> <<"?"/utf8>> end),
        gleam@string:join(_pipe@1, <<", "/utf8>>)
    end,
    Sql = <<<<"select id, subreddit_id, author_id, title, body, score, created_at from posts where subreddit_id in ("/utf8,
            Placeholders/binary>>/binary,
        ") order by score desc, created_at desc limit ?"/utf8>>,
    Args = begin
        _pipe@2 = Subreddit_ids,
        _pipe@3 = gleam@list:map(_pipe@2, fun sqlight:int/1),
        lists:append(_pipe@3, [sqlight:int(Limit)])
    end,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(S) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(A) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    fun(T) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1},
                                            fun(B) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    {decoder,
                                                        fun gleam@dynamic@decode:decode_int/1},
                                                    fun(Sc) ->
                                                        gleam@dynamic@decode:field(
                                                            6,
                                                            {decoder,
                                                                fun gleam@dynamic@decode:decode_int/1},
                                                            fun(C) ->
                                                                gleam@dynamic@decode:success(
                                                                    {post,
                                                                        Id,
                                                                        S,
                                                                        A,
                                                                        T,
                                                                        B,
                                                                        Sc,
                                                                        C}
                                                                )
                                                            end
                                                        )
                                                    end
                                                )
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case sqlight:'query'(Sql, Conn, Args, Decoder) of
        {ok, Posts} ->
            {ok, Posts};

        {error, E} ->
            {error, error_to_string(E)}
    end.

-file("src/storage/posts.gleam", 64).
-spec list_for_subreddits(sqlight:connection(), list(integer()), integer()) -> {ok,
        list(post())} |
    {error, binary()}.
list_for_subreddits(Conn, Subreddit_ids, Limit) ->
    case Subreddit_ids of
        [] ->
            {ok, []};

        _ ->
            build_and_query(Conn, Subreddit_ids, Limit)
    end.
