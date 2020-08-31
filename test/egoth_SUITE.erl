-module(egoth_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("egoth.hrl").

-define(AUTH_URL, <<"https://authurl.com">>).
-define(INVALID_TOKEN_AUTH_URL, <<"https://invalidauthurl.com">>).
-define(REQUEST_URL, <<"https://requesturl.com">>).
-define(REQUEST_URL_401, <<"https://requesturl401.com">>).
-define(CLIENT_CREDENTIALS_GRANT, <<"client_credentials">>).
-define(VALID_TOKEN, <<"iamanaccesstoken">>).
-define(HEADERS(AccessToken),
        [{<<"Authorization">>, <<"bearer ", AccessToken/binary>>}]).

-define(GET_BODY, [{<<"a">>, <<"b">>}]).

groups() -> [].

all() -> [
    get_access_token_metadata
  ].

% https://github.com/heroku/esputnik/blob/master/test/esputnik_first_SUITE.erl
init_per_suite(Config) ->
  {ok, _MeckApps} = application:ensure_all_started(meck),
  % meck:new(hackney, [unstick]),
  % meck:expect(hackney, request, fun(_) -> {ok, 200, 0, ok} end),
  % meck:expect(hackney, get, fun(_) -> {ok, 200, 0, ok} end),
  % meck:expect(hackney, post, fun(_) -> {ok, 200, 0, ok} end),

  {ok, Pid} = config:start(),
  {ok, _Pid2} = token_store:start(),
  [{config_pid, Pid}|Config].
end_per_suite(Config) ->
  meck:unload(hackney),
  {config_pid, Pid} = proplists:lookup(config_pid, Config),
  % {token_pid, Pid2} = proplists:lookup(token_pid, Config),
  exit(Pid, shutdown),
  % exit(Pid2, shutdown),
  ok.

init_per_testcase(TestCase, Config) ->
  meck:new(hackney, [unstick, passthrough]),
  meck:expect(hackney, request,
              fun(get, <<"http://metadata.google.internal/computeMetadata/v1/project/project-id">>, [], {}, _) ->
                      {ok, 200, ["foobar"], connection1}
              end),
  meck:expect(hackney, send_request,
              fun(connection2, _) ->
                      {error, closed}
              end),
  meck:expect(hackney, body,
              fun(connection1) ->
                      {ok, <<"{\"request_id\":\"random\"}">>}
              end),
  meck:expect(hackney, close,
              fun(active_client) ->
                      ok
              end),
  ?MODULE:TestCase({init, Config}).

end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({'end', Config}).

% init_per_testcase(retrieve_cached_token_on_401_burst, Config) ->
%   mock_http_request_401_burst(),
%   Config;
% init_per_testcase(retrieve_cached_token_on_401, Config) ->
%   mock_http_request_401(),
%   Config;
% init_per_testcase(_TestCase, Config) ->
%   mock_http_requests(),
%   Config.
% end_per_testcase(_TestCase, Config) ->
%   meck:unload([restc]),
%   oauth2c_token_cache:clear(),
%   Config.

get_access_token_metadata() ->
  Scope = #config{account = "default"},
  {ok, Token} = egoth:for_scope(Scope),
  % Token = #token{
  %   account = "foobar"
  % },
  ExpectedToken = #token{
    account = "default"
  },
  ?assertEqual(ExpectedToken, Token).
