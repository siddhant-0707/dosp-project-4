-module(storage@db).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/db.gleam").
-export([with_engine_connection/1]).

-file("src/storage/db.gleam", 5).
-spec with_engine_connection(fun((sqlight:connection()) -> AHQ)) -> AHQ.
with_engine_connection(F) ->
    sqlight:with_connection(<<"reddit.db"/utf8>>, fun(Conn) -> F(Conn) end).
