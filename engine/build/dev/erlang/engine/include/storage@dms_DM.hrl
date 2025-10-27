-record(d_m, {
    id :: integer(),
    sender_id :: integer(),
    recipient_id :: integer(),
    body :: binary(),
    created_at :: integer(),
    in_reply_to :: gleam@option:option(integer())
}).
