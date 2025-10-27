{application, engine, [
    {vsn, "0.1.0"},
    {applications, [gleam_erlang,
                    gleam_otp,
                    gleam_stdlib,
                    gleeunit,
                    sqlight]},
    {description, "Reddit-like engine (actors + SQLite)"},
    {modules, [engine_api,
               storage@memberships]},
    {registered, []}
]}.
