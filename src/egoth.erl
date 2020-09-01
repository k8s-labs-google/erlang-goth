%%%-----------------------------------------------------------------------------
%%%
%%% @doc Google Auth in erlang.
%%%
%%% Essentially a port of the Elixir package goth
%%% @author Will Beebe
%%% @end
%%%-----------------------------------------------------------------------------
-module(egoth).

-export([for_scope/1]).
-export([get_access_token/1]).
-export([get_access_token/2]).
-export ([retrieve_metadata_project/0]).

-define(OAUTH_URL, <<"https://www.googleapis.com/oauth2/v4/token">>).
-define(OAUTH_REFRESH_URL(ClientId),
  lists:concat(["https://www.googleapis.com/oauth2/v4/token?client_id=", ClientId, ",access_type=offline,nonc=foobar,response_type=code,"])).

-include("egoth.hrl").

%%% API ========================================================================

% is there a way for this method to live in token.erl but still be consumable in
% downstream modules?
- spec for_scope(Scope) -> Return when
  Scope :: binary(),
  Return :: {ok, #token{}}.
for_scope(Scope) ->
  token:for_scope(Scope).

- spec get_access_token(Scope) -> Return when
  Scope  :: #config{},
  Return :: {ok, #token{}}  | {'error', _}.
get_access_token(#config{}=Scope) ->
  Config = config:get(),
  {_, TokenSource} = Config#config.token_source,
  get_access_token({TokenSource}, Scope).

%%------------------------------------------------------------------------------
%% @doc pass in an atom for oauth_jwt, oauth_refresh, or metata.
%%
%% @see get_access_token/2
%% @end
%%------------------------------------------------------------------------------
- spec get_access_token({atom()}, Scope) -> Return when
  Scope  :: #config{},
  Return :: {ok, #token{}}  | {'error', _}.
get_access_token({oauth_jwt}, #config{}=Scope) ->
  Headers = [{
    <<"Content-Type">>, <<"application/x-www-form-urlencoded">>
  }],
  Assertion = jwt(Scope),
  Body = [{
    <<"grant_type">>, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>},
    {<<"assertion">>, Assertion
  }],

  Options = [],
  Return = case hackney:post(?OAUTH_URL, Headers, {form, Body}, Options)
  of
    {ok, _, _, ClientRef} ->
      {ok, ResponseBody} = hackney:body(ClientRef),
      % should be Scope => Config
      Token = token:from_response_json(Scope, ResponseBody),
      {ok, Token};
    {error, Reason} ->
      {error, Reason}
  end,
  Return;
get_access_token({oauth_refresh}, #config{}= _Scope) ->
  Config = config:get(),
  {_, RefreshToken} = Config#config.refresh_token,
  {_, ClientId} = Config#config.client_id,
  {_, ClientSecret} = Config#config.client_secret,
  Headers = [{
    <<"Content-Type">>, <<"application/x-www-form-urlencoded">>
  }],
  Body = [#{
    grant_type => authorization_code,
    refresh_token => RefreshToken,
    client_id => ClientId,
    client_secret => ClientSecret,
    access_type => offline
  }],
  Payload = jiffy:encode(Body),
  Options = [],
  URL = list_to_binary(?OAUTH_REFRESH_URL(binary_to_list(ClientId))),
  Return = case hackney:post(URL, Headers, Payload, Options)
  of
    {ok, _, _, ClientRef} ->
      {ok, ResponseBody} = hackney:body(ClientRef),
      {token = Token} = jiffy:decode(ResponseBody, [ return_maps ]),
      {ok, Token};
    {error, Reason} ->
      {error, Reason}
  end,
  Return;
get_access_token({metadata}, _Scope) ->
  io:fwrite("get_access_token metata"),
  % TODO: account needs to be specificed in URL
  URL = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default",
  Headers = [
    {
      <<"Metadata-Flavor">>, <<"Google">>
    }
  ],
  Payload = <<>>,
  Options = [],
  case hackney:request(get, URL, Headers, Payload, Options)
  of
    {ok, _, _, ClientRef} ->
      {ok, ResponseBody} = hackney:body(ClientRef),
      {token = Token} = jiffy:decode(ResponseBody, [ return_maps ]),
      {ok, Token};
    {error, Reason} ->
      {error, Reason}
  end.

retrieve_metadata_project() ->
  URL = "http://metadata.google.internal/computeMetadata/v1/project/project-id",
  Headers = [{
      <<"Metadata-Flavor">>, <<"Google">>
  }],
  Options = [],
  {ok, _, _, ClientRef} = hackney:request(get, URL, Headers, <<>>, Options),
  {ok, ResponseBody} = hackney:body(ClientRef),
  ResponseBody.


%%% INTERNAL ===================================================================

- spec claims(Scope) -> Return when
  Scope :: #config{},
  Return :: map().
claims(Scope) ->
  Config = config:get(),
  {_, Value} = Config#config.client_email,
  {MegaSecs, Secs, _} = erlang:timestamp(),
  UnixTime = MegaSecs * 1000000 + Secs,
  #{
    <<"iss">> => Value,
    <<"scope">> => Scope#config.scope,
    <<"aud">> => <<"https://www.googleapis.com/oauth2/v4/token">>,
    <<"iat">> => UnixTime,
    <<"exp">> => UnixTime + 3600
  }.

- spec jwt(Scope) -> Return when
  Scope :: #config{},
  Return :: binary().
jwt(Scope) ->
  Claims = claims(Scope),
  Config = config:get(),
  {_, PrivateKey} = Config#config.private_key,
  {ok, Token} = jwt:encode(<<"RS256">>, Claims, PrivateKey),
  Token.

%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
