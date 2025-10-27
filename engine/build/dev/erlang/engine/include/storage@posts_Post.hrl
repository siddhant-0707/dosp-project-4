-record(post, {
    id :: integer(),
    subreddit_id :: integer(),
    author_id :: integer(),
    title :: binary(),
    body :: binary(),
    score :: integer(),
    created_at :: integer()
}).
