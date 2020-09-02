-module(token).

-include("egoth.hrl").

-export([for_scope/1]).
-export([for_scope/2]).
-export([refresh/1]).
-export([queue_for_refresh/1]).
-export([from_response_json/2]).

- spec for_scope(Scope) -> Return when
  Scope :: binary(),
  Return :: {ok, #token{}} | {error, _}.
for_scope(Scope) ->
  for_scope(Scope, undefined).

- spec for_scope(Scope, Sub) -> Value when
  %          scope    | {account,           scope}
  Scope   :: binary() | {string() | binary(), binary()},
  Sub   :: string() | undefined,
  Value :: {ok, #token{}} | {error, _}.
for_scope(Scope, Sub) when is_binary(Scope) ->
  Token = token_store:find(#config{account = "default", scope = Scope}, Sub),
  case token_store:find(#config{account = "default", scope = Scope}, Sub)
  of
    {error, _} ->
      retrieve_and_store(#config{account = "default", scope = Scope}, Sub);
    {ok, Token} ->
      {ok, Token}
  end;
for_scope({Account, Scope}, Sub) ->
  case token_store:find(#config{account = Account, scope = Scope}, Sub)
  of
    {ok, Token} ->
      {ok, Token};
    {error, _} ->
      retrieve_and_store(#config{account = Account, scope = Scope}, Sub)
  end.

-spec refresh(#config{}) -> Return when
  Return :: #token{} | {'error', _}.
refresh(Token) ->
  retrieve_and_store(#config{account = Token#token.account, scope = Token#token.scope}, undefined).

-spec retrieve_and_store(Config, Sub) -> Return when
  Config :: #config{},
  Sub   :: string() | undefined,
  Return :: {ok, #token{}} | {error, _}.
retrieve_and_store(#config{account = Account, scope = Scope}, Sub) ->
  case egoth:get_access_token(#config{account = Account, scope = Scope}) of
    {ok, Token} ->
      token_store:store(#config{account = Account, scope = Scope}, Sub, Token),
      {ok, Token};
    {error, Reason} ->
      {error, Reason}
  end.

-spec queue_for_refresh(Token) -> Value when
  Token :: #token{},
  Value :: pid() | term().
queue_for_refresh(#token{expires = Expires} = Token) ->
  Diff = Expires - os:system_time(),
  if Diff < 10 ->
    rpc:async_call(node(), ?MODULE, refresh, [Token]);
  true ->
    {ok, TimerRef } = timer:apply_after((Diff - 10) * 1000, ?MODULE, refresh, [Token]),
    TimerRef
  end.

from_response_json(#config{account = Account, scope = Scope}, ResponseBody) ->
  Json = jiffy:decode(ResponseBody, [ return_maps ]),

  #token{
    token = maps:get(<<"access_token">>, Json),
    type = maps:get(<<"token_type">>, Json),
    scope = Scope,
    account = Account,
    expires = os:system_time() + maps:get(<<"expires_in">>, Json)
  }.

% def queue_for_refresh(%__MODULE__{} = token) do
%   diff = token.expires - :os.system_time(:seconds)

%   if diff < 10 do
%     # just do it immediately
%     Task.async(fn ->
%       __MODULE__.refresh!(token)
%     end)
%   else
%     :timer.apply_after((diff - 10) * 1000, __MODULE__, :refresh!, [token])
%   end
% end
