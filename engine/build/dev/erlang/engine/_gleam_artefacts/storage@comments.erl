-module(storage@comments).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/comments.gleam").
-export([create/5, list_for_post/2]).
-export_type([comment/0]).

-type comment() :: {comment,
        integer(),
        integer(),
        gleam@option:option(integer()),
        integer(),
        binary(),
        integer(),
        integer()}.

-file("src/storage/comments.gleam", 101).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/comments.gleam", 17).
-spec create(
    sqlight:connection(),
    integer(),
    gleam@option:option(integer()),
    integer(),
    binary()
) -> {ok, comment()} | {error, binary()}.
create(Conn, Post_id, Parent_comment_id, Author_id, Body) ->
    Sql = <<"insert into comments(post_id, parent_comment_id, author_id, body, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000) returning id, post_id, parent_comment_id, author_id, body, score, created_at;"/utf8>>,
    Parent = sqlight:nullable(fun sqlight:int/1, Parent_comment_id),
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(P) ->
                        gleam@dynamic@decode:field(
                            2,
                            gleam@dynamic@decode:optional(
                                {decoder, fun gleam@dynamic@decode:decode_int/1}
                            ),
                            fun(Parent_id) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_int/1},
                                    fun(A) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1},
                                            fun(B) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    {decoder,
                                                        fun gleam@dynamic@decode:decode_int/1},
                                                    fun(S) ->
                                                        gleam@dynamic@decode:field(
                                                            6,
                                                            {decoder,
                                                                fun gleam@dynamic@decode:decode_int/1},
                                                            fun(C) ->
                                                                gleam@dynamic@decode:success(
                                                                    {comment,
                                                                        Id,
                                                                        P,
                                                                        Parent_id,
                                                                        A,
                                                                        B,
                                                                        S,
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
        [sqlight:int(Post_id),
            Parent,
            sqlight:int(Author_id),
            sqlight:text(Body)],
        Decoder
    ) of
        {ok, [Comment]} ->
            {ok, Comment};

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.

-file("src/storage/comments.gleam", 64).
-spec list_for_post(sqlight:connection(), integer()) -> {ok, list(comment())} |
    {error, binary()}.
list_for_post(Conn, Post_id) ->
    Sql = <<"select id, post_id, parent_comment_id, author_id, body, score, created_at from comments where post_id = ? order by created_at asc"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(P) ->
                        gleam@dynamic@decode:field(
                            2,
                            gleam@dynamic@decode:optional(
                                {decoder, fun gleam@dynamic@decode:decode_int/1}
                            ),
                            fun(Parent) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_int/1},
                                    fun(A) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1},
                                            fun(B) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    {decoder,
                                                        fun gleam@dynamic@decode:decode_int/1},
                                                    fun(S) ->
                                                        gleam@dynamic@decode:field(
                                                            6,
                                                            {decoder,
                                                                fun gleam@dynamic@decode:decode_int/1},
                                                            fun(C) ->
                                                                gleam@dynamic@decode:success(
                                                                    {comment,
                                                                        Id,
                                                                        P,
                                                                        Parent,
                                                                        A,
                                                                        B,
                                                                        S,
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
    case sqlight:'query'(Sql, Conn, [sqlight:int(Post_id)], Decoder) of
        {ok, Comments} ->
            {ok, Comments};

        {error, E} ->
            {error, error_to_string(E)}
    end.
