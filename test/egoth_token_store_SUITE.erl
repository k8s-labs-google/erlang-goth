-module(egoth_token_store_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("egoth.hrl").

all() -> [ store_token
].

init_per_suite(Config) ->
  {ok, Pid} = token_store:start(),
  [{pid, Pid}|Config].

end_per_suite(Config) ->
  {pid, Pid} = proplists:lookup(pid, Config),
  exit(Pid, shutdown),
  ok.

init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({init, Config}).

end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({'end', Config}).

store_token({init, Config}) ->
  Config;
store_token({'end', Config}) ->
  token_store:clear(),
  Config;
store_token(_Config) ->
  Config = #config{account = "defualt", scope = "scope"},

  Token = #token{
    token = "aaaddnn",
    account = "default",
    scope = "scope_token",
    expires = os:system_time() + 10
  },
  Sub = undefined,
  token_store:store(Config, Sub, Token),
  ?assertMatch(Token, token_store:find(Config, Sub)).
