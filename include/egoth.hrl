-record(client, {grant_type    = undefined :: binary()     | undefined,
                 auth_url      = undefined :: binary()     | undefined,
                 access_token  = undefined :: binary()     | undefined,
                 token_type    = undefined :: token_type() | undefined,
                 refresh_token = undefined :: binary()     | undefined,
                 id            = undefined :: binary()     | undefined,
                 secret        = undefined :: binary()     | undefined,
                 scope         = undefined :: binary()     | undefined,
                 expire_time   = undefined :: integer()    | undefined,
                 credentials_path = undefined :: binary()  | undefined
                }).

-record(credentials_file, {auth_provider_x509_cert_url    = undefined :: binary()     | undefined,
    auth_uri      = undefined :: binary()     | undefined,
    client_email = undefined :: binary()  | undefined,
    client_id = undefined :: binary() | undefined,
    client_x509_cert_url = undefined :: binary() | undefined,
    private_key = undefined :: binary() | undefined,
    private_key_id = undefined :: binary() | undefined,
    project_id = undefined :: binary() | undefined,
    token_uri = undefined :: binary() | undefined,
    type = undefined :: binary() | undefined
}).

-type method()         :: head    |
                          get     |
                          put     |
                          patch   |
                          post    |
                          trace   |
                          options |
                          delete.
-type url()            :: binary().
%% <<"password">> or <<"client_credentials">>
-type at_type()        :: binary().
-type headers()        :: [header()].
-type header()         :: {binary(), binary()}.
-type status_codes()   :: [status_code()].
-type status_code()    :: integer().
-type reason()         :: term().
-type content_type()   :: json | xml | percent.
-type property()       :: atom() | tuple().
-type proplist()       :: [property()].
-type options()        :: proplist().
-type body()           :: proplist().
-type restc_response() :: { ok
                          , Status::status_code()
                          , Headers::headers()
                          , Body::body()}          |
                          { error
                          , Status::status_code()
                          , Headers::headers()
                          , Body::body()}          |
                          { error, Reason::reason()}.
-type response()       :: {restc_response(), #client{}}.
-type token_type()     :: bearer | unsupported.
-type client()         :: #client{}.
