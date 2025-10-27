-record(comment, {
    id :: integer(),
    post_id :: integer(),
    parent_comment_id :: gleam@option:option(integer()),
    author_id :: integer(),
    body :: binary(),
    score :: integer(),
    created_at :: integer()
}).
