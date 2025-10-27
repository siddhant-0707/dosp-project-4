-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/main.gleam").
-export([main/0]).

-file("src/main.gleam", 6).
-spec main() -> nil.
main() ->
    sqlight:with_connection(
        <<"reddit.db"/utf8>>,
        fun(Conn) ->
            _ = storage@schema:init(Conn),
            gleam_stdlib:println(<<"engine started"/utf8>>)
        end
    ).
