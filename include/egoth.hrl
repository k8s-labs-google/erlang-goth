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

-record(config, {
    account :: string(),
    scope   :: string(),
    sub     = undefined :: string() | undefined,

    json   = undefined :: string() | undefined,
    config = undefined :: string() | undefined,
    actor_email = undefined :: string() | undefined,
    project_id = undefined :: string() | undefined,
    client_email = undefined :: string() | undefined,

    % use creds_file record?
    token_source = undefined :: string() | undefined,
    type = undefined :: binary() | undefined,
    client_id = undefined :: string() | undefined,
    client_secret = undefined :: string() | undefined,
    refresh_token = undefined :: string() | undefined,
    private_key = undefined :: string() | undefined
}).

-record(token, {
    token   :: string(),
    type    = undefined :: string() | undefined,
    scope   = undefined :: string() | undefined,
    sub     = undefined :: string() | undefined,
    expires :: non_neg_integer(),
    account :: string()
}).
