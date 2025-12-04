-module(http_ffi).
-export([http_get/1, http_post/2]).

%% Simple HTTP GET using Erlang's httpc
http_get(Url) ->
    inets:start(),
    case httpc:request(get, {binary_to_list(Url), []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, 201, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Simple HTTP POST using Erlang's httpc
http_post(Url, JsonBody) ->
    inets:start(),
    Request = {
        binary_to_list(Url),
        [{"content-type", "application/json"}],
        "application/json",
        JsonBody
    },
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, 201, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

