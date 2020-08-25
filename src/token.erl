-module(token).

% no need for a gen server

-export([for_scope/2]).
-export([refresh/1]).
-export([queue_for_refresh/1]).

-spec for_scope(Map, Map) -> {atom(), Map}.
for_scope(#{account := Account, scope := Scope}, Sub) ->
  case token_store:find(#{account => Account, scope => Scope}, Sub)
  of
    {ok, Token} ->
      {ok, Token};
    {error, _} ->
      {ok, Token} = retrieve_and_store(#{account => Account, scope => Scope}, Sub),
      {ok, Token}
  end.

-spec refresh(Map) -> {atom(), Map}.
refresh({account = Account, scope = Scope}) ->
  retrieve_and_store(#{account => Account, scope => Scope}, undefined).

retrieve_and_store({account = Account, scope = Scope}, Sub) ->
  Token = egoth:get_access_token(#{account => Account, scope => Scope}),
  token_store:store(#{account => Account, scope => Scope}, Sub, Token),
  {ok, Token}.

queue_for_refresh({expires = Expires} = Token) ->
  Diff = Expires - os:system_time(),
  if Diff < 10 ->
    rpc:async_call(node(), ?MODULE, refresh, Token);
  true ->
    io:fwrite("timer"),
    timer:apply_after((Diff - 10) * 1000, ?MODULE, refresh, [Token])
  end.

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
