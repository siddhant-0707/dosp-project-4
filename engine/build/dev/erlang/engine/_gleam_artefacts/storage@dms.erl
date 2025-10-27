-module(storage@dms).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/dms.gleam").
-export([send/5, inbox/4]).
-export_type([d_m/0]).

-type d_m() :: {d_m,
        integer(),
        integer(),
        integer(),
        binary(),
        integer(),
        gleam@option:option(integer())}.

-file("src/storage/dms.gleam", 100).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/dms.gleam", 16).
-spec send(
    sqlight:connection(),
    integer(),
    integer(),
    binary(),
    gleam@option:option(integer())
) -> {ok, d_m()} | {error, binary()}.
send(Conn, Sender_id, Recipient_id, Body, In_reply_to) ->
    Parent = sqlight:nullable(fun sqlight:int/1, In_reply_to),
    Sql = <<"insert into dms(sender_id, recipient_id, body, created_at, in_reply_to) values (?, ?, ?, strftime('%s','now')*1000, ?) returning id, sender_id, recipient_id, body, created_at, in_reply_to;"/utf8>>,
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
                            fun(R) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    fun(B) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_int/1},
                                            fun(C) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    gleam@dynamic@decode:optional(
                                                        {decoder,
                                                            fun gleam@dynamic@decode:decode_int/1}
                                                    ),
                                                    fun(P) ->
                                                        gleam@dynamic@decode:success(
                                                            {d_m,
                                                                Id,
                                                                S,
                                                                R,
                                                                B,
                                                                C,
                                                                P}
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
        [sqlight:int(Sender_id),
            sqlight:int(Recipient_id),
            sqlight:text(Body),
            Parent],
        Decoder
    ) of
        {ok, [Dm]} ->
            {ok, Dm};

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.

-file("src/storage/dms.gleam", 61).
-spec inbox(
    sqlight:connection(),
    integer(),
    integer(),
    gleam@option:option(integer())
) -> {ok, list(d_m())} | {error, binary()}.
inbox(Conn, Recipient_id, Limit, After_id) ->
    Base = <<"select id, sender_id, recipient_id, body, created_at, in_reply_to from dms where recipient_id = ?"/utf8>>,
    Cond = case After_id of
        {some, _} ->
            <<" and id < ?"/utf8>>;

        none ->
            <<""/utf8>>
    end,
    Sql = <<<<Base/binary, Cond/binary>>/binary,
        " order by id desc limit ?"/utf8>>,
    Args = case After_id of
        {some, Id} ->
            [sqlight:int(Recipient_id), sqlight:int(Id), sqlight:int(Limit)];

        none ->
            [sqlight:int(Recipient_id), sqlight:int(Limit)]
    end,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id@1) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(S) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(R) ->
                                gleam@dynamic@decode:field(
                                    3,
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    fun(B) ->
                                        gleam@dynamic@decode:field(
                                            4,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_int/1},
                                            fun(C) ->
                                                gleam@dynamic@decode:field(
                                                    5,
                                                    gleam@dynamic@decode:optional(
                                                        {decoder,
                                                            fun gleam@dynamic@decode:decode_int/1}
                                                    ),
                                                    fun(P) ->
                                                        gleam@dynamic@decode:success(
                                                            {d_m,
                                                                Id@1,
                                                                S,
                                                                R,
                                                                B,
                                                                C,
                                                                P}
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
        {ok, Dms} ->
            {ok, Dms};

        {error, E} ->
            {error, error_to_string(E)}
    end.
