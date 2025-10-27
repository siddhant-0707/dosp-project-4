-module(storage@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/storage/schema.gleam").
-export([init/1]).

-file("src/storage/schema.gleam", 40).
-spec map_sql_err({ok, nil} | {error, sqlight:error()}) -> {ok, nil} |
    {error, binary()}.
map_sql_err(Res) ->
    case Res of
        {ok, _} ->
            {ok, nil};

        {error, {sqlight_error, _, Message, _}} ->
            {error, Message}
    end.

-file("src/storage/schema.gleam", 4).
-spec init(sqlight:connection()) -> {ok, nil} | {error, binary()}.
init(Conn) ->
    Statements = [<<"pragma foreign_keys = on;"/utf8>>,
        <<"create table if not exists accounts (\n  id integer primary key autoincrement,\n  username text not null unique,\n  created_at integer not null\n);"/utf8>>,
        <<"create table if not exists subreddits (\n  id integer primary key autoincrement,\n  name text not null unique,\n  created_at integer not null\n);"/utf8>>,
        <<"create table if not exists memberships (\n  account_id integer not null,\n  subreddit_id integer not null,\n  joined_at integer not null,\n  primary key (account_id, subreddit_id),\n  foreign key(account_id) references accounts(id) on delete cascade,\n  foreign key(subreddit_id) references subreddits(id) on delete cascade\n);"/utf8>>,
        <<"create table if not exists posts (\n  id integer primary key autoincrement,\n  subreddit_id integer not null,\n  author_id integer not null,\n  title text not null,\n  body text not null,\n  score integer not null default 0,\n  created_at integer not null,\n  foreign key(subreddit_id) references subreddits(id) on delete cascade,\n  foreign key(author_id) references accounts(id) on delete cascade\n);"/utf8>>,
        <<"create table if not exists comments (\n  id integer primary key autoincrement,\n  post_id integer not null,\n  parent_comment_id integer,\n  author_id integer not null,\n  body text not null,\n  score integer not null default 0,\n  created_at integer not null,\n  foreign key(post_id) references posts(id) on delete cascade,\n  foreign key(parent_comment_id) references comments(id) on delete cascade,\n  foreign key(author_id) references accounts(id) on delete cascade\n);"/utf8>>,
        <<"create table if not exists votes (\n  entity_type text not null,\n  entity_id integer not null,\n  voter_id integer not null,\n  value integer not null,\n  created_at integer not null,\n  primary key(entity_type, entity_id, voter_id)\n);"/utf8>>,
        <<"create table if not exists dms (\n  id integer primary key autoincrement,\n  sender_id integer not null,\n  recipient_id integer not null,\n  body text not null,\n  created_at integer not null,\n  in_reply_to integer\n);"/utf8>>,
        <<"create index if not exists idx_posts_subreddit_created on posts(subreddit_id, created_at desc);"/utf8>>,
        <<"create index if not exists idx_posts_score_created on posts(score desc, created_at desc);"/utf8>>,
        <<"create index if not exists idx_comments_post_parent on comments(post_id, parent_comment_id);"/utf8>>,
        <<"create index if not exists idx_memberships_subreddit on memberships(subreddit_id);"/utf8>>,
        <<"create index if not exists idx_votes_entity on votes(entity_type, entity_id);"/utf8>>,
        <<"create index if not exists idx_dms_recipient_created on dms(recipient_id, created_at desc);"/utf8>>],
    case gleam@list:try_map(
        Statements,
        fun(Stmt) -> _pipe = sqlight:exec(Stmt, Conn),
            map_sql_err(_pipe) end
    ) of
        {ok, _} ->
            {ok, nil};

        {error, E} ->
            {error, E}
    end.
