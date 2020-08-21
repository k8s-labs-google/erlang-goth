%%%-------------------------------------------------------------------
%% @doc goth application callback
%% @end
%%%-------------------------------------------------------------------

-module(egoth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  egoth_sup:start_link().

stop(_State) ->
  ok.
