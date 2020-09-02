-module(token_store).

-behaviour(gen_server).

-include("egoth.hrl").

-export([start/0]).
-export([start_link/0]).

%% gen_server
-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%% API
-export([store/3]).
-export([find/2]).
-export([clear/0]).

-define(SERVER, ?MODULE).

% docs
-spec store(#config{}, _, #token{}) -> Return when
  Return :: ok | {error, _}.
store(#config{account = Account, scope = Scope}, Sub, Token) ->
  Message = #config{account = Account, scope = Scope, sub = Sub},
  gen_server:call(?SERVER, {store, Message, Token}).

-spec find(#config{}, _) -> Return when
  Return :: {ok, #token{}} | {error, _}.
find(#config{account = Account, scope = Scope}, Sub) ->
  Message = #config{account = Account, scope = Scope, sub = Sub},
  gen_server:call(?SERVER, {find, Message}).


%%------------------------------------------------------------------------------
%% @doc resets the token store to empty, clearing all existing keys.
%%
%% @end
%%------------------------------------------------------------------------------
-spec clear() -> Return when
  Return :: ok.
clear() ->
  gen_server:cast(?SERVER, {clear, {}}).

% gen server
-spec start() -> {atom(), pid()}.
start() ->
  gen_server:start({local, ?SERVER}, ?SERVER, {}, []).

-spec start_link() -> {atom(), pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, {}, []).

-spec init(tuple()) -> {'ok', _}.
init(_State) ->
  {ok, #{}}.

- spec handle_cast({atom()}, _) -> {'noreply', _}.
handle_cast({clear}, _State) ->
  {noreply, #{}};
handle_cast(_, State) ->
  {noreply, State}.

% when we store a token, we should refresh it later
handle_call({store, #config{account = Account} = _Config, #token{} = Token}, _From, State) ->
  % this is a race condition when inserting an expired (or about to expire) token...
  PidOrTimer = token:queue_for_refresh(Token),
  % Key = lists:flatten(io_lib:format("~0p", [Config])),
  % NewState = maps:put(Account, Token, State),

  % TODO: map all record values to this map
  % a bit awkward with a record vs a map as state
  NewState2 = #{
    token => Token#token.token
  },

  maps:put(Account, NewState2, State),

  {reply, PidOrTimer, NewState2};

% TODO: check elixir goth
handle_call({find, #config{account = Account, scope = _Scope, sub = _Sub} = _Config}, _From, State) ->
  Token = maps:get(Account, State, #{}),
  Filtered = filter_expired(Token, 100),
  Expres = maps:get("token", Filtered, false),

  if
  Expres == false ->
    {reply, error, State};
  true ->
    {reply, Filtered, State}
  end.



  % reply(Filtered, State, State).

% filter_expired(error, _) ->
%   error;
% filter_expired({ok, #{expires := Expires}}, SystemTime) when Expires < SystemTime ->
%   error;
filter_expired(Value, _) ->
  Value.

% reply(error, State, #config{account = Account, scope = Scope, sub = Sub}) ->
%   NewState = maps:remove(#config{account = Account, scope = Scope, sub = Sub}, State),
%   {reply, error, NewState};
% reply(Value, State, _Key) ->
%   {reply, Value, State}.
