-module(web_server).
-export([start/1, accept_loop/1]).

%% Start simple HTTP server using gen_tcp
start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http_bin},
        {active, false},
        {reuseaddr, true}
    ]),
    io:format("[REST API] Server listening on port ~p~n", [Port]),
    spawn(?MODULE, accept_loop, [ListenSocket]),
    ok.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_connection(Socket) end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            Headers = recv_headers(Socket, []),
            ContentLengthRaw = proplists:get_value('Content-Length', Headers, <<"0">>),
            
            %% Parse Content-Length (could be binary or integer)
            ContentLength = case ContentLengthRaw of
                LenInt when is_integer(LenInt) -> LenInt;
                Bin when is_binary(Bin) ->
                    case catch binary_to_integer(Bin) of
                        Int when is_integer(Int) -> Int;
                        _ -> 0
                    end;
                List when is_list(List) ->
                    case catch list_to_integer(List) of
                        Int when is_integer(Int) -> Int;
                        _ -> 0
                    end;
                _ -> 0
            end,
            
            %% Switch to raw mode to read body
            inet:setopts(Socket, [{packet, raw}]),
            Body = if
                ContentLength > 0 ->
                    case gen_tcp:recv(Socket, ContentLength, 5000) of
                        {ok, B} -> B;
                        {error, _} -> <<>>
                    end;
                true -> <<>>
            end,
            
            io:format("[DEBUG] Body received (~p bytes): ~p~n", [byte_size(Body), Body]),
            
            %% Handle the request
            {Status, ResponseBody} = handle_request(Method, Path, Body),
            
            %% Send response
            Response = build_response(Status, ResponseBody),
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

recv_headers(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Socket, [{header_name(Name), Value} | Acc]);
        {ok, http_eoh} ->
            Acc;
        {ok, {http_error, _}} ->
            Acc;
        {error, _} ->
            Acc
    end.

header_name(Name) when is_atom(Name) -> Name;
header_name(Name) when is_binary(Name) -> binary_to_atom(Name, utf8).

build_response(Status, Body) ->
    StatusText = status_text(Status),
    BodyBin = if is_binary(Body) -> Body; true -> list_to_binary(Body) end,
    ContentLength = byte_size(BodyBin),
    iolist_to_binary([
        <<"HTTP/1.1 ">>, integer_to_binary(Status), <<" ">>, StatusText, <<"\r\n">>,
        <<"Content-Type: application/json\r\n">>,
        <<"Content-Length: ">>, integer_to_binary(ContentLength), <<"\r\n">>,
        <<"Access-Control-Allow-Origin: *\r\n">>,
        <<"Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n">>,
        <<"Access-Control-Allow-Headers: Content-Type\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>,
        BodyBin
    ]).

status_text(200) -> <<"OK">>;
status_text(201) -> <<"Created">>;
status_text(400) -> <<"Bad Request">>;
status_text(404) -> <<"Not Found">>;
status_text(500) -> <<"Internal Server Error">>;
status_text(_) -> <<"Unknown">>.

%% Route requests to appropriate handlers
handle_request('OPTIONS', _, _) ->
    {200, <<"{}">>};

handle_request('GET', <<"/api/health">>, _) ->
    {200, web:handle_health()};

handle_request('GET', <<"/api/crypto/generate_keypair">>, _) ->
    {200, web:handle_generate_keypair()};

handle_request('POST', <<"/api/register">>, Body) ->
    try
        Json = decode_json(Body),
        io:format("[DEBUG] Decoded JSON: ~p~n", [Json]),
        Username = get_string(Json, <<"username">>, <<>>),
        PublicKey = get_string(Json, <<"public_key">>, <<>>),
        io:format("[DEBUG] Username: ~p, PublicKey: ~p~n", [Username, PublicKey]),
        {201, web:handle_register(binary_to_list(Username), binary_to_list(PublicKey))}
    catch
        Class:Error:Stack ->
            io:format("[ERROR] Register failed: ~p:~p~n~p~n", [Class, Error, Stack]),
            {400, <<"{\"error\":\"Invalid request\"}">>}
    end;

handle_request('GET', <<"/api/accounts/", Rest/binary>>, _) ->
    case binary:split(Rest, <<"/">>) of
        [IdBin, <<"public_key">>] ->
            case catch binary_to_integer(IdBin) of
                Id when is_integer(Id) -> {200, web:handle_get_public_key(Id)};
                _ -> {400, <<"{\"error\":\"Invalid ID\"}">>}
            end;
        [IdBin] ->
            case catch binary_to_integer(IdBin) of
                Id when is_integer(Id) -> {200, web:handle_get_account(Id)};
                _ -> {400, <<"{\"error\":\"Invalid ID\"}">>}
            end;
        _ -> {404, <<"{\"error\":\"Not found\"}">>}
    end;

handle_request('GET', <<"/api/accounts/username/", Username/binary>>, _) ->
    {200, web:handle_get_account_by_username(binary_to_list(Username))};

handle_request('POST', <<"/api/subreddits">>, Body) ->
    try
        Json = decode_json(Body),
        Name = get_string(Json, <<"name">>, <<>>),
        {201, web:handle_create_subreddit(binary_to_list(Name))}
    catch _:_ ->
        {400, <<"{\"error\":\"Invalid request\"}">>}
    end;

handle_request('POST', <<"/api/subreddits/", Rest/binary>>, Body) ->
    case binary:split(Rest, <<"/">>) of
        [IdBin, <<"join">>] ->
            try
                SubId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                UserId = get_int(Json, <<"user_id">>, 0),
                {200, web:handle_join_subreddit(UserId, SubId)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        [IdBin, <<"leave">>] ->
            try
                SubId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                UserId = get_int(Json, <<"user_id">>, 0),
                {200, web:handle_leave_subreddit(UserId, SubId)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        _ -> {404, <<"{\"error\":\"Not found\"}">>}
    end;

handle_request('POST', <<"/api/posts">>, Body) ->
    try
        Json = decode_json(Body),
        SubId = get_int(Json, <<"subreddit_id">>, 0),
        AuthorId = get_int(Json, <<"author_id">>, 0),
        Title = get_string(Json, <<"title">>, <<>>),
        PostBody = get_string(Json, <<"body">>, <<>>),
        Signature = get_string(Json, <<"signature">>, <<>>),
        {201, web:handle_create_post(SubId, AuthorId, binary_to_list(Title), binary_to_list(PostBody), binary_to_list(Signature))}
    catch _:_ ->
        {400, <<"{\"error\":\"Invalid request\"}">>}
    end;

handle_request('GET', <<"/api/posts/", Rest/binary>>, _) ->
    case binary:split(Rest, <<"/">>) of
        [IdBin, <<"verified">>] ->
            case catch binary_to_integer(IdBin) of
                Id when is_integer(Id) -> {200, web:handle_get_post_verified(Id)};
                _ -> {400, <<"{\"error\":\"Invalid ID\"}">>}
            end;
        [IdBin, <<"comments">>] ->
            case catch binary_to_integer(IdBin) of
                Id when is_integer(Id) -> {200, web:handle_get_comments(Id)};
                _ -> {400, <<"{\"error\":\"Invalid ID\"}">>}
            end;
        [IdBin] ->
            case catch binary_to_integer(IdBin) of
                Id when is_integer(Id) -> {200, web:handle_get_post(Id)};
                _ -> {400, <<"{\"error\":\"Invalid ID\"}">>}
            end;
        _ -> {404, <<"{\"error\":\"Not found\"}">>}
    end;

handle_request('POST', <<"/api/posts/", Rest/binary>>, Body) ->
    case binary:split(Rest, <<"/">>) of
        [IdBin, <<"vote">>] ->
            try
                PostId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                VoterId = get_int(Json, <<"voter_id">>, 0),
                Value = get_int(Json, <<"value">>, 0),
                {200, web:handle_vote_post(PostId, VoterId, Value)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        [IdBin, <<"comments">>] ->
            try
                PostId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                AuthorId = get_int(Json, <<"author_id">>, 0),
                CommentBody = get_string(Json, <<"body">>, <<>>),
                ParentId = get_int(Json, <<"parent_id">>, 0),
                {201, web:handle_create_comment(PostId, AuthorId, binary_to_list(CommentBody), ParentId)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        [IdBin, <<"repost">>] ->
            try
                PostId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                AuthorId = get_int(Json, <<"author_id">>, 0),
                {201, web:handle_repost(PostId, AuthorId)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        _ -> {404, <<"{\"error\":\"Not found\"}">>}
    end;

handle_request('POST', <<"/api/comments/", Rest/binary>>, Body) ->
    case binary:split(Rest, <<"/">>) of
        [IdBin, <<"vote">>] ->
            try
                CommentId = binary_to_integer(IdBin),
                Json = decode_json(Body),
                VoterId = get_int(Json, <<"voter_id">>, 0),
                Value = get_int(Json, <<"value">>, 0),
                {200, web:handle_vote_comment(CommentId, VoterId, Value)}
            catch _:_ ->
                {400, <<"{\"error\":\"Invalid request\"}">>}
            end;
        _ -> {404, <<"{\"error\":\"Not found\"}">>}
    end;

handle_request('GET', <<"/api/feed/", UserIdBin/binary>>, _) ->
    case catch binary_to_integer(UserIdBin) of
        UserId when is_integer(UserId) -> {200, web:handle_feed(UserId, 20)};
        _ -> {400, <<"{\"error\":\"Invalid user ID\"}">>}
    end;

handle_request('POST', <<"/api/dms">>, Body) ->
    try
        Json = decode_json(Body),
        SenderId = get_int(Json, <<"sender_id">>, 0),
        RecipientId = get_int(Json, <<"recipient_id">>, 0),
        DmBody = get_string(Json, <<"body">>, <<>>),
        InReplyTo = get_int(Json, <<"in_reply_to">>, 0),
        {201, web:handle_send_dm(SenderId, RecipientId, binary_to_list(DmBody), InReplyTo)}
    catch _:_ ->
        {400, <<"{\"error\":\"Invalid request\"}">>}
    end;

handle_request('GET', <<"/api/dms/inbox/", UserIdBin/binary>>, _) ->
    case catch binary_to_integer(UserIdBin) of
        UserId when is_integer(UserId) -> {200, web:handle_inbox(UserId, 50)};
        _ -> {400, <<"{\"error\":\"Invalid user ID\"}">>}
    end;

handle_request('GET', <<"/api/karma/", UserIdBin/binary>>, _) ->
    case catch binary_to_integer(UserIdBin) of
        UserId when is_integer(UserId) -> {200, web:handle_karma(UserId)};
        _ -> {400, <<"{\"error\":\"Invalid user ID\"}">>}
    end;

handle_request(_, _, _) ->
    {404, <<"{\"error\":\"Not found\"}">>}.

%% Simple JSON parsing using Erlang's built-in term_to_binary approach
%% For production, use a proper JSON library
decode_json(<<>>) -> #{};
decode_json(Binary) ->
    %% Very simple JSON object parser for our specific use case
    %% Handles {"key":"value","key2":"value2"} format
    Str = binary_to_list(Binary),
    parse_simple_json(string:trim(Str)).
%% Simple JSON object parser
parse_simple_json([]) -> #{};
parse_simple_json([${|Rest]) ->
    parse_json_pairs(Rest, #{});
parse_simple_json(_) -> #{}.

parse_json_pairs([$}|_], Acc) -> Acc;
parse_json_pairs([], Acc) -> Acc;
parse_json_pairs(Str, Acc) ->
    case parse_json_pair(string:trim(Str)) of
        {Key, Value, Rest} ->
            parse_json_pairs(Rest, Acc#{Key => Value});
        done -> Acc
    end.

parse_json_pair([]) -> done;
parse_json_pair([$}|_]) -> done;
parse_json_pair([$"|Rest]) ->
    {Key, AfterKey} = parse_json_string(Rest, []),
    AfterColon = skip_chars(AfterKey, [$:, $\s, $\t]),
    {Value, AfterValue} = parse_json_value(AfterColon),
    AfterComma = skip_chars(AfterValue, [$,, $\s, $\t]),
    {list_to_binary(Key), Value, AfterComma};
parse_json_pair(_) -> done.

parse_json_string([$"|Rest], Acc) -> {lists:reverse(Acc), Rest};
parse_json_string([$\\, C|Rest], Acc) -> parse_json_string(Rest, [C|Acc]);
parse_json_string([C|Rest], Acc) -> parse_json_string(Rest, [C|Acc]);
parse_json_string([], Acc) -> {lists:reverse(Acc), []}.

parse_json_value([$"|Rest]) ->
    {Str, After} = parse_json_string(Rest, []),
    {list_to_binary(Str), After};
parse_json_value([C|_] = Str) when C >= $0, C =< $9; C == $- ->
    parse_json_number(Str, []);
parse_json_value("true" ++ Rest) -> {true, Rest};
parse_json_value("false" ++ Rest) -> {false, Rest};
parse_json_value("null" ++ Rest) -> {null, Rest};
parse_json_value(Str) -> {null, Str}.

parse_json_number([C|Rest], Acc) when C >= $0, C =< $9; C == $-; C == $. ->
    parse_json_number(Rest, [C|Acc]);
parse_json_number(Rest, Acc) ->
    NumStr = lists:reverse(Acc),
    Num = try
        list_to_integer(NumStr)
    catch _:_ ->
        try list_to_float(NumStr)
        catch _:_ -> 0
        end
    end,
    {Num, Rest}.

skip_chars(Str, Chars) ->
    string:trim(Str, leading, Chars).

get_string(Map, Key, Default) when is_map(Map) ->
    case maps:get(Key, Map, Default) of
        V when is_binary(V) -> V;
        V when is_list(V) -> list_to_binary(V);
        _ -> Default
    end;
get_string(_, _, Default) -> Default.

get_int(Map, Key, Default) when is_map(Map) ->
    case maps:get(Key, Map, Default) of
        V when is_integer(V) -> V;
        V when is_binary(V) -> 
            case catch binary_to_integer(V) of
                I when is_integer(I) -> I;
                _ -> Default
            end;
        _ -> Default
    end;
get_int(_, _, Default) -> Default.
