-module(storage@votes).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/votes.gleam").
-export([upsert/5]).
-export_type([entity_type/0]).

-type entity_type() :: post | comment.

-file("src/storage/votes.gleam", 9).
-spec entity_to_text(entity_type()) -> binary().
entity_to_text(E) ->
    case E of
        post ->
            <<"post"/utf8>>;

        comment ->
            <<"comment"/utf8>>
    end.

-file("src/storage/votes.gleam", 89).
-spec error_to_string(sqlight:error()) -> binary().
error_to_string(E) ->
    case E of
        {sqlight_error, _, Message, _} ->
            Message
    end.

-file("src/storage/votes.gleam", 46).
-spec update_score(sqlight:connection(), entity_type(), integer()) -> {ok, nil} |
    {error, binary()}.
update_score(Conn, Entity_type, Entity_id) ->
    Et = entity_to_text(Entity_type),
    Score_sql = <<"select coalesce(sum(value),0) from votes where entity_type = ? and entity_id = ?"/utf8>>,
    Decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Score) -> gleam@dynamic@decode:success(Score) end
        )
    end,
    case sqlight:'query'(
        Score_sql,
        Conn,
        [sqlight:text(Et), sqlight:int(Entity_id)],
        Decoder
    ) of
        {ok, [Score@1]} ->
            Update_sql = case Entity_type of
                post ->
                    <<"update posts set score = ? where id = ?"/utf8>>;

                comment ->
                    <<"update comments set score = ? where id = ?"/utf8>>
            end,
            Decoder2 = gleam@dynamic@decode:success(nil),
            case sqlight:'query'(
                Update_sql,
                Conn,
                [sqlight:int(Score@1), sqlight:int(Entity_id)],
                Decoder2
            ) of
                {ok, _} ->
                    {ok, nil};

                {error, E} ->
                    {error, error_to_string(E)}
            end;

        {ok, _} ->
            {error, <<"Expected exactly one row"/utf8>>};

        {error, E@1} ->
            {error, error_to_string(E@1)}
    end.

-file("src/storage/votes.gleam", 16).
-spec upsert(
    sqlight:connection(),
    entity_type(),
    integer(),
    integer(),
    integer()
) -> {ok, nil} | {error, binary()}.
upsert(Conn, Entity_type, Entity_id, Voter_id, Value) ->
    Et = entity_to_text(Entity_type),
    Sql = <<"insert into votes(entity_type, entity_id, voter_id, value, created_at) values (?, ?, ?, ?, strftime('%s','now')*1000)\n"/utf8,
        "on conflict(entity_type, entity_id, voter_id) do update set value = excluded.value, created_at = strftime('%s','now')*1000;"/utf8>>,
    Decoder = gleam@dynamic@decode:success(nil),
    case sqlight:'query'(
        Sql,
        Conn,
        [sqlight:text(Et),
            sqlight:int(Entity_id),
            sqlight:int(Voter_id),
            sqlight:int(Value)],
        Decoder
    ) of
        {ok, _} ->
            update_score(Conn, Entity_type, Entity_id);

        {error, E} ->
            {error, error_to_string(E)}
    end.
