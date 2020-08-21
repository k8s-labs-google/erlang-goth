-module(egoth).

-export([client/4]).
-export([client/5]).

-export([gcp_access_token/2]).
-export([gcp_access_token/3]).
-export([gcp_access_token/4]).

-export([request/3]).
-export([request/4]).
-export([request/5]).
-export([request/6]).
-export([request/7]).
-export([request/8]).

-define(DEFAULT_ENCODING, json).
-define(TOKEN_CACHE_SERVER, oauth2c_token_cache).

-include("egoth.hrl").

%%% API ========================================================================

-spec client(Type, URL, ID, Secret) -> client() when
    Type   :: at_type(),
    URL    :: url(),
    ID     :: binary(),
    Secret :: binary().
client(Type, URL, ID, Secret) ->
  client(Type, URL, ID, Secret, undefined).

-spec client(Type, URL, ID, Secret, Scope) -> client() when
    Type   :: at_type(),
    URL    :: url(),
    ID     :: binary(),
    Secret :: binary(),
    Scope  :: binary() | undefined.
client(Type, URL, ID, Secret, Scope) ->
   #client{ grant_type = Type
          , auth_url  = URL
          , id        = ID
          , secret    = Secret
          , scope     = Scope
          }.

gcp_access_token(oauth_jwt, CREDENTIALS_PATH) ->
  Client = #client{
    auth_url  = <<"https://www.googleapis.com/oauth2/v4/token">>,
    credentials_path        = CREDENTIALS_PATH
  },
  Request = #{headers => [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    body => [{<<"grant_type">>, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>}, {<<"assertion">>, jwt(Client)}]},
  #{headers := RequestHeaders,
    body := RequestBody} = Request,

  case restc:request(post, percent, Client#client.auth_url,
                    [200], RequestHeaders, RequestBody, [])
  of
    {ok, _, Headers, Body} ->
      % move to handle_response
      AccessToken = proplists:get_value(<<"access_token">>, Body),
      TokenType = proplists:get_value(<<"token_type">>, Body, ""),
      ExpireTime =
        case proplists:get_value(<<"expires_in">>, Body) of
          undefined -> undefined;
          ExpiresIn -> erlang:system_time(second) + ExpiresIn
        end,
      RefreshToken = proplists:get_value(<<"refresh_token">>,
                                        Body,
                                        Client#client.refresh_token),
      Result = #client{ grant_type    = Client#client.grant_type
                      , auth_url      = Client#client.auth_url
                      , access_token  = AccessToken
                      , refresh_token = RefreshToken
                      , token_type    = get_token_type(TokenType)
                      , id            = Client#client.id
                      , secret        = Client#client.secret
                      , scope         = Client#client.scope
                      , expire_time   = ExpireTime
                      },
      {ok, Headers, Result};
    {error, _, _, Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason}
  end.

-spec gcp_access_token(Type, URL, CREDENTIALS_PATH) ->
  {ok, Headers::headers(), client()} | {error, Reason :: binary()} when
  Type    :: at_type(),
  URL     :: url(),
  CREDENTIALS_PATH      :: binary().
gcp_access_token(Type, Url, CREDENTIALS_PATH) ->
  gcp_access_token(Type, Url, CREDENTIALS_PATH, []).

-spec gcp_access_token(Type, URL, CREDENTIALS_PATH, Options) ->
    {ok, Headers::headers(), client()} | {error, Reason :: binary()} when
    Type    :: at_type(),
    URL     :: url(),
    CREDENTIALS_PATH      :: binary(),
    Options :: options().
  gcp_access_token(Type, Url, CREDENTIALS_PATH, Options) ->
  Client = #client{ grant_type = Type
                  , auth_url  = Url
                  , credentials_path        = CREDENTIALS_PATH
                  },
  do_retrieve_access_token(Client, Options).

-spec request(Method, Url, Client) -> Response::response() when
    Method :: method(),
    Url    :: url(),
    Client :: client().
request(Method, Url, Client) ->
  request(Method, ?DEFAULT_ENCODING, Url, [], [], [], Client).

-spec request(Method, Url, Expect, Client) -> Response::response() when
    Method :: method(),
    Url    :: url(),
    Expect :: status_codes(),
    Client :: client().
request(Method, Url, Expect, Client) ->
  request(Method, ?DEFAULT_ENCODING, Url, Expect, [], [], Client).

-spec request(Method, Type, Url, Expect, Client) -> Response::response() when
    Method :: method(),
    Type   :: content_type(),
    Url    :: url(),
    Expect :: status_codes(),
    Client :: client().
request(Method, Type, Url, Expect, Client) ->
  request(Method, Type, Url, Expect, [], [], Client).

-spec request(Method, Type, Url, Expect, Headers, Client) ->
        Response::response() when
    Method  :: method(),
    Type    :: content_type(),
    Url     :: url(),
    Expect  :: status_codes(),
    Headers :: headers(),
    Client  :: client().
request(Method, Type, Url, Expect, Headers, Client) ->
    request(Method, Type, Url, Expect, Headers, [], Client).

-spec request(Method, Type, Url, Expect, Headers, Body, Client) ->
        Response::response() when
    Method  :: method(),
    Type    :: content_type(),
    Url     :: url(),
    Expect  :: status_codes(),
    Headers :: headers(),
    Body    :: body(),
    Client  :: client().
request(Method, Type, Url, Expect, Headers, Body, Client) ->
  request(Method, Type, Url, Expect, Headers, Body, [], Client).

-spec request(Method, Type, Url, Expect, Headers, Body, Options, Client) ->
        Response::response() when
    Method  :: method(),
    Type    :: content_type(),
    Url     :: url(),
    Expect  :: status_codes(),
    Headers :: headers(),
    Body    :: body(),
    Options :: options(),
    Client  :: client().

request(Method, Type, Url, Expect, Headers, Body, Options, Client0) ->
  Client1 = ensure_client_has_access_token(Client0, Options),
  case do_request(Method,Type,Url,Expect,Headers,Body,Options,Client1) of
    {{_, 401, _, _}, Client2} ->
      {ok, Client3} = get_access_token(Client2#client{access_token = undefined},
                                      [force_revalidate | Options]),
      do_request(Method, Type, Url, Expect, Headers, Body, Options, Client3);
    Result -> Result
  end.

%%% INTERNAL ===================================================================

ensure_client_has_access_token(Client0, Options) ->
  case Client0 of
    #client{access_token = undefined} ->
      {ok, Client} = get_access_token(Client0, Options),
      Client;
    _ ->
      Client0
  end.

do_retrieve_access_token(Client, Opts) ->
  Request = #{
    headers => [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    body => [{<<"grant_type">>, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>}, {<<"assertion">>, jwt(Client)}]
  },
  #{headers := RequestHeaders,
    body := RequestBody} = Request,
  % move to handle_request
  case restc:request(post, percent, Client#client.auth_url,
                     [200], RequestHeaders, RequestBody, Opts)
  of
    {ok, _, Headers, Body} ->
      AccessToken = proplists:get_value(<<"access_token">>, Body),
      TokenType = proplists:get_value(<<"token_type">>, Body, ""),
      ExpireTime =
        case proplists:get_value(<<"expires_in">>, Body) of
          undefined -> undefined;
          ExpiresIn -> erlang:system_time(second) + ExpiresIn
        end,
      RefreshToken = proplists:get_value(<<"refresh_token">>,
                                         Body,
                                         Client#client.refresh_token),
      Result = #client{ grant_type    = Client#client.grant_type
                      , auth_url      = Client#client.auth_url
                      , access_token  = AccessToken
                      , refresh_token = RefreshToken
                      , token_type    = get_token_type(TokenType)
                      , id            = Client#client.id
                      , secret        = Client#client.secret
                      , scope         = Client#client.scope
                      , expire_time   = ExpireTime
                      },
      {ok, Headers, Result};
    {error, _, _, Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason}
  end.

% prepare_token_request(Client, Opts) ->
  % BaseRequest = base_request(Client),
  % Request0 = add_client(BaseRequest, Client, Opts),
  % add_fields(Request0, Client).

claims(Data) ->
  ClientEmail = maps:get(<<"client_email">>, Data),
  % TODO: pass in scopes
  Scope = <<"https://www.googleapis.com/auth/cloud-platform https://www.googleapis.com/auth/trace.append">>,
  {MegaSecs, Secs, _} = erlang:timestamp(),
  UnixTime = MegaSecs * 1000000 + Secs,
  #{
    <<"iss">> => ClientEmail,
    <<"scope">> => Scope,
    <<"aud">> => <<"https://www.googleapis.com/oauth2/v4/token">>,
    <<"iat">> => UnixTime,
    <<"exp">> => UnixTime + 3600
  }.

jwt(Client) ->
  #client{ credentials_path = CredentialsPath } = Client,
  {ok, File} = file:read_file(CredentialsPath),
  % TODO: stoare in state, don't read during every request
  Data = jiffy:decode(File, [ return_maps ]),
  PrivateKey = maps:get(<<"private_key">>, Data),
  % using records would be nice
  % #credentials_file{ <<"private_key">> = PrivateKey } = Data,
  Claims = claims(Data),
  {ok, Token} = jwt:encode(<<"RS256">>, Claims, PrivateKey),
  Token.

% base_request(#client{grant_type = <<"gcp_client_credentials">>} = Client) ->
%   #{headers => [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
%     body => [{<<"grant_type">>, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>}, {<<"assertion">>, jwt(Client)}]};
% base_request(#client{grant_type = GrantType}) ->
%   #{headers => [], body => [{<<"grant_type">>, GrantType}]}.

% add_client(Request0, Client, Opts) ->
%   #client{id = Id, secret = Secret} = Client,
%   case
%     {Client#client.grant_type =:= <<"gcp_client_credentials">> orelse
%      proplists:get_value(credentials_in_body, Opts, false)}
%   of
%     {false} ->
%       #{headers := Headers0} = Request0,
%       Auth = base64:encode(<<Id/binary, ":", Secret/binary>>),
%       Headers = [{<<"Authorization">>, <<"Basic ", Auth/binary>>}
%                  | Headers0],
%       Request0#{headers => Headers};
%     {true} ->
%       Request0
%   end.

% add_fields(Request, #client{scope=undefined}) ->
%   Request;
% add_fields(Request, #client{grant_type = <<"gcp_client_credentials">>}) ->
%   Request.

-spec get_token_type(binary()) -> token_type().
get_token_type(Type) ->
  get_str_token_type(string:to_lower(binary_to_list(Type))).

-spec get_str_token_type(string()) -> token_type().
get_str_token_type("bearer") -> bearer;
get_str_token_type(_Else) -> unsupported.

do_request(Method, Type, Url, Expect, Headers0, Body, Options, Client) ->
  Headers = add_auth_header(Headers0, Client),
  {restc:request(Method, Type, Url, Expect, Headers, Body, Options), Client}.

add_auth_header(Headers, #client{grant_type = <<"gcp_client_credentials">>,
                                 access_token = AccessToken}) ->
  AH = {<<"Authorization">>, <<"Bearer ", AccessToken/binary>>},
  [AH | proplists:delete(<<"Authorization">>, Headers)];
add_auth_header(Headers, #client{token_type = bearer,
                                 access_token = AccessToken}) ->
  AH = {<<"Authorization">>, <<"bearer ", AccessToken/binary>>},
  [AH | proplists:delete(<<"Authorization">>, Headers)];
add_auth_header(Headers, #client{access_token = AccessToken}) ->
  AH = {<<"Authorization">>, <<"token ", AccessToken/binary>>},
  [AH | proplists:delete(<<"Authorization">>, Headers)].

retrieve_access_token_fun(Client0, Options) ->
  fun() ->
      case do_retrieve_access_token(Client0, Options) of
        {ok, _Headers, Client} -> {ok, Client, Client#client.expire_time};
        {error, Reason} -> {error, Reason}
      end
  end.

get_access_token(#client{expire_time = ExpireTime} = Client0, Options) ->
  case {proplists:get_value(cache_token, Options, false),
        proplists:get_value(force_revalidate, Options, false)}
  of
    {false, _} ->
      {ok, _Headers, Client} = do_retrieve_access_token(Client0, Options),
      {ok, Client};
    {true, false} ->
      Key = hash_client(Client0),
      case oauth2c_token_cache:get(Key) of
        {error, not_found} ->
          RevalidateFun = retrieve_access_token_fun(Client0, Options),
          oauth2c_token_cache:set_and_get(Key, RevalidateFun);
        {ok, Client} ->
          {ok, Client}
      end;
    {true, true} ->
      Key = hash_client(Client0),
      RevalidateFun = retrieve_access_token_fun(Client0, Options),
      oauth2c_token_cache:set_and_get(Key, RevalidateFun, ExpireTime)
  end.

hash_client(#client{grant_type = Type,
                    auth_url = AuthUrl,
                    id = ID,
                    secret = Secret,
                    scope = Scope}) ->
  erlang:phash2({Type, AuthUrl, ID, Secret, Scope}).

%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
