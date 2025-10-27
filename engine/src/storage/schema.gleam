import gleam/list
import sqlight

pub fn init(conn: sqlight.Connection) -> Result(Nil, String) {
  let statements = [
    "pragma foreign_keys = on;",
    // accounts
    "create table if not exists accounts (\n  id integer primary key autoincrement,\n  username text not null unique,\n  created_at integer not null\n);",
    // subreddits
    "create table if not exists subreddits (\n  id integer primary key autoincrement,\n  name text not null unique,\n  created_at integer not null\n);",
    // memberships
    "create table if not exists memberships (\n  account_id integer not null,\n  subreddit_id integer not null,\n  joined_at integer not null,\n  primary key (account_id, subreddit_id),\n  foreign key(account_id) references accounts(id) on delete cascade,\n  foreign key(subreddit_id) references subreddits(id) on delete cascade\n);",
    // posts
    "create table if not exists posts (\n  id integer primary key autoincrement,\n  subreddit_id integer not null,\n  author_id integer not null,\n  title text not null,\n  body text not null,\n  score integer not null default 0,\n  created_at integer not null,\n  foreign key(subreddit_id) references subreddits(id) on delete cascade,\n  foreign key(author_id) references accounts(id) on delete cascade\n);",
    // comments
    "create table if not exists comments (\n  id integer primary key autoincrement,\n  post_id integer not null,\n  parent_comment_id integer,\n  author_id integer not null,\n  body text not null,\n  score integer not null default 0,\n  created_at integer not null,\n  foreign key(post_id) references posts(id) on delete cascade,\n  foreign key(parent_comment_id) references comments(id) on delete cascade,\n  foreign key(author_id) references accounts(id) on delete cascade\n);",
    // votes
    "create table if not exists votes (\n  entity_type text not null,\n  entity_id integer not null,\n  voter_id integer not null,\n  value integer not null,\n  created_at integer not null,\n  primary key(entity_type, entity_id, voter_id)\n);",
    // dms
    "create table if not exists dms (\n  id integer primary key autoincrement,\n  sender_id integer not null,\n  recipient_id integer not null,\n  body text not null,\n  created_at integer not null,\n  in_reply_to integer\n);",
    // indexes
    "create index if not exists idx_posts_subreddit_created on posts(subreddit_id, created_at desc);",
    "create index if not exists idx_posts_score_created on posts(score desc, created_at desc);",
    "create index if not exists idx_comments_post_parent on comments(post_id, parent_comment_id);",
    "create index if not exists idx_memberships_subreddit on memberships(subreddit_id);",
    "create index if not exists idx_votes_entity on votes(entity_type, entity_id);",
    "create index if not exists idx_dms_recipient_created on dms(recipient_id, created_at desc);",
  ]

  case
    list.try_map(statements, fn(stmt) {
      sqlight.exec(stmt, conn) |> map_sql_err
    })
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(e)
  }
}

fn map_sql_err(res: Result(Nil, sqlight.Error)) -> Result(Nil, String) {
  case res {
    Ok(_) -> Ok(Nil)
    Error(sqlight.SqlightError(_, message, _)) -> Error(message)
  }
}
