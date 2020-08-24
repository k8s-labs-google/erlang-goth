-module(token_store).

-behaviour(gen_server).

-export([start/0]).
-export([start_link/0]).

%% gen_server
-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%% API
-export([store/3]).
-export([find/2]).

-define(SERVER, ?MODULE).

% API
store({account = Account, scope = Scope}, Sub, Token) ->
  Message = #{account => Account, scope => Scope, sub => Sub},
  gen_server:cast(?SERVER, {store, Message, Token}).

find({Account, Scope}, Sub) ->
  Message = #{account => Account, scope => Scope, sub => Sub},
  gen_server:call(?SERVER, {find, Message}).

% gen server
-spec start() -> {atom(), pid()}.
start() ->
  gen_server:start({local, ?SERVER}, ?SERVER, {}, []).

-spec start_link() -> {atom(), pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, {}, []).

init(State) ->
  {ok, State}.

handle_cast(_, State) ->
  {reply, State}.

% when we store a token, we should refresh it later
handle_call({store, #{account := Account, scope := Scope, sub := Sub}, Token}, _From, State) ->
  % this is a race condition when inserting an expired (or about to expire) token...
  pid_or_timer = token:queue_for_refresh(Token),
  NewState = maps:put(State, {Account, Scope, Sub}, Token),
  {reply, pid_or_timer, NewState};
handle_call({find, #{account := Account, scope := Scope, sub := Sub}}, _From, State) ->
  Token = maps:get(State, {Account, Scope, Sub}),
  Filtered = filter_expired(Token, 100),
  reply(Filtered, State, {Account, Scope, Sub}).

filter_expired(error, _) ->
  error;
filter_expired({ok, #{expires := Expires}}, SystemTime) when Expires < SystemTime ->
  error;
filter_expired(Value, _) ->
  Value.

reply(error, State, #{account := Account, scope := Scope, sub := Sub}) ->
  NewState = maps:remove(State, {Account, Scope, Sub}),
  {reply, error, NewState};
reply(Value, State, _Key) ->
  {reply, Value, State}.
