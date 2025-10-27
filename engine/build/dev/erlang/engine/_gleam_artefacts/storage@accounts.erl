-module(storage@accounts).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/accounts.gleam").
-export([create/2, by_username/2]).
-export_type([account/0]).

-type account() :: {account, integer(), binary(), integer()}.

-file("src/storage/accounts.gleam", 61).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/accounts.gleam", 9).
-spec create(sqlight:connection(), binary()) -> {ok, account()} |
    {error, binary()}.
create(Conn, Username) ->
    Sql = <<"insert into accounts(username, created_at) values (?, strftime('%s','now')*1000) returning id, username, created_at;"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Name) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(Created) ->
                                gleam@dynamic@decode:success(
                                    {account, Id, Name, Created}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case sqlight:'query'(Sql, Conn, [sqlight:text(Username)], Decoder) of
        {ok, [Account]} ->
            {ok, Account};

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.

-file("src/storage/accounts.gleam", 35).
-spec by_username(sqlight:connection(), binary()) -> {ok,
        gleam@option:option(account())} |
    {error, binary()}.
by_username(Conn, Username) ->
    Sql = <<"select id, username, created_at from accounts where username = ?"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Id) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Name) ->
                        gleam@dynamic@decode:field(
                            2,
                            {decoder, fun gleam@dynamic@decode:decode_int/1},
                            fun(Created) ->
                                gleam@dynamic@decode:success(
                                    {account, Id, Name, Created}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case sqlight:'query'(Sql, Conn, [sqlight:text(Username)], Decoder) of
        {ok, [Account]} ->
            {ok, {some, Account}};

        {ok, []} ->
            {ok, none};

        {ok, _} ->
            {error, <<"Expected 0 or 1 rows"/utf8>>};

        {error, E} ->
            {error, error_to_string(E)}
    end.
