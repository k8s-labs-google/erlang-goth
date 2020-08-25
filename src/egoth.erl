-module(egoth).

% new API
-export([for_scope/1]).

-export([get_access_token/1]).
-export([get_access_token/2]).

-export ([retrieve_metadata_project/0]).

-define(OAUTH_URL, <<"https://www.googleapis.com/oauth2/v4/token">>).

-include("egoth.hrl").

%%% API ========================================================================

% is there a way for this method to live in token.erl but still be consumable in
% downstream modules?

for_scope(Scope) ->
  token:for_scope(Scope).

get_access_token(Scope) ->
  get_access_token(oauth_jwt, Scope).

get_access_token({oauth_jwt}, Scope) ->
  Headers = [{
    <<"Content-Type">>, <<"application/x-www-form-urlencoded">>
  }],
  Body = [{
      <<"grant_type">>, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>
    }, {
      <<"assertion">>, jwt(Scope)
  }],
  Payload = jiffy:encode(Body),
  Options = [],
  case hackney:post(?OAUTH_URL, Headers, Payload, Options)
  of
    {ok, _, _, ClientRef} ->
      {token = Token} = jiffy:decode(hackney:body(ClientRef), [ return_maps ]),
      {ok, Token};
    {error, Reason} ->
      {error, Reason}
  end;
get_access_token({metadata}, _Scope) ->
  % TODO: account needs to be specificed
  URL = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default",
  Headers = [
    {
      <<"Metadata-Flavor">>, <<"Google">>
    }
  ],
  Options = [],
  % https://github.com/benoitc/hackney
  {ok, _, _, ClientRef} = hackney:get(URL, Headers, Options),
  {token = Token} = jiffy:decode(hackney:body(ClientRef), [ return_maps ]),
  {ok, Token}.

%   headers = [{"Metadata-Flavor", "Google"}]
%   account = Application.get_env(:goth, :metadata_account, "default")
%   metadata = Application.get_env(:goth, :metadata_url, "http://metadata.google.internal")
%   endpoint = "computeMetadata/v1/instance/service-accounts"
%   url_base = "#{metadata}/#{endpoint}/#{account}"

%   url = "#{url_base}/token"
%   {:ok, token} = HTTPoison.get(url, headers)
%   {:ok, Token.from_response_json({service_account, scope}, token.body)}
% end

% handle_response(Client, Body) ->
%   AccessToken = proplists:get_value(<<"access_token">>, Body),
%   TokenType = proplists:get_value(<<"token_type">>, Body, ""),
%   ExpireTime =
%     case proplists:get_value(<<"expires_in">>, Body) of
%       undefined -> undefined;
%       ExpiresIn -> erlang:system_time(second) + ExpiresIn
%     end,
%   RefreshToken = proplists:get_value(<<"refresh_token">>,
%                                     Body,
%                                     Client#client.refresh_token),
%   Result = #client{access_token  = AccessToken
%                   , refresh_token = RefreshToken
%                   , id            = Client#client.id
%                   , secret        = Client#client.secret
%                   , scope         = Client#client.scope
%                   , expire_time   = ExpireTime
%                   },
%   Result.

retrieve_metadata_project() ->
  URL = "http://metadata.google.internal/computeMetadata/v1/project/project-id",
  Headers = [{
      <<"Metadata-Flavor">>, <<"Google">>
  }],
  Options = [],
  {ok, _, _, ClientRef} = hackney:get(URL, Headers, Options),
  hackney:body(ClientRef).


%%% INTERNAL ===================================================================

claims(Scope) ->
  Value = config:get(<<"client_email">>),
  {MegaSecs, Secs, _} = erlang:timestamp(),
  UnixTime = MegaSecs * 1000000 + Secs,
  #{
    <<"iss">> => Value,
    <<"scope">> => Scope,
    <<"aud">> => <<"https://www.googleapis.com/oauth2/v4/token">>,
    <<"iat">> => UnixTime,
    <<"exp">> => UnixTime + 3600
  }.

jwt(Scope) ->
  Claims = claims(Scope),
  PrivateKey = config:get(<<"private_key">>),
  {ok, Token} = jwt:encode(<<"RS256">>, Claims, PrivateKey),
  Token.

%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
