%%%-------------------------------------------------------------------
%% @doc Top level supervisor of oauth2c.
%% @end
%%%-------------------------------------------------------------------

%%%_* Module declaration ===============================================
-module(egoth_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([start_link/0]).
-export([init/1]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Strategy = #{strategy => one_for_one,
                intensity => 2,
                period => 60},
  ChildSpecs = [#{id => token_store,
                  start => {token_store, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [token_store]
                },#{id => config,
                  start => {config, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [config]
                }],
  {ok, {Strategy, ChildSpecs}}.
