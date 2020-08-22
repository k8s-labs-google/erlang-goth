-module(config).

-behaviour(gen_server).

-export([start/0]).
-export([start_link/0]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%% API
-export([get/1]).

-define(SERVER, ?MODULE).

-spec start() -> {atom(), pid()}.
start() ->
  gen_server:start({local, ?SERVER}, ?SERVER, {}, []).

-spec start_link() -> {atom(), pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, {}, []).

%%%_ * gen_server callbacks --------------------------------------------

-spec get(Key) -> Value | Error when
    Key :: string(),
    Value :: {ok,  binary()},
    Error :: {error, atom()}.
get(Key) ->
  gen_server:call(?SERVER, {get,
                            Key}).

init(_Args) ->
  CredentialsPath = os:getenv("GOOGLE_CREDENTIALS"),
  {ok, File} = file:read_file(CredentialsPath),
  Data = jiffy:decode(File, [ return_maps ]),
  State = #{"gcp_credentials" => Data},
  {ok, State}.

handle_call({get, Key}, _From, State) ->
  GcpCredentials = maps:get("gcp_credentials", State),
  Value = maps:get(Key, GcpCredentials),
  {ok, Value}.

handle_cast(_, State) -> {noreply, State}.

%%%_ * Private functions -----------------------------------------------


%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
