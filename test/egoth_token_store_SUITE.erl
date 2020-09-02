-module(egoth_token_store_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("egoth.hrl").

all() -> [ ].

% init_per_suite(Config) ->
%   {ok, _MeckApps} = application:ensure_all_started(meck),
%   meck:new(hackney, [unstick, passthrough]),
%   meck:expect(hackney, request,
%               fun(get, "http://metadata.google.internal/computeMetadata/v1/project/project-id", [{
%                 <<"Metadata-Flavor">>, <<"Google">>
%               }], <<>>, []) ->
%                       {ok, 200, ["foobar"], connection1}
%               end),
%   meck:expect(hackney_manager, get_state,
%     fun(_, _) ->
%       {ok, "foo", "foo", "project-id"}
%     end),
%   meck:expect(hackney, body,
%     fun(_) ->
%       {ok, "project-id"}
%     end),
%   [Config].
% %   % meck:new(hackney, [unstick]),
% %   % meck:expect(hackney, request, fun(_) -> {ok, 200, 0, ok} end),
% %   % meck:expect(hackney, get, fun(_) -> {ok, 200, 0, ok} end),
% %   % meck:expect(hackney, post, fun(_) -> {ok, 200, 0, ok} end),
% %   {ok, Pid} = token_store:start(),
% %   [{pid, Pid}|Config].

% % end_per_suite(Config) ->
% %   {pid, Pid} = proplists:lookup(pid, Config),
% %   exit(Pid, shutdown),
% %   ok.

% init_per_testcase(TestCase, Config) ->
%   ?MODULE:TestCase({init, Config}).

% end_per_testcase(TestCase, Config) ->
%   ?MODULE:TestCase({'end', Config}).

% store_token({init, Config}) ->
%   Config;
% store_token({'end', Config}) ->
%   token_store:clear(),
%   Config;
% store_token(_Config) ->
%   Config = #config{account = "defualt", scope = "scope"},

%   Token = #token{
%     token = "aaaddnn",
%     account = "default",
%     scope = "scope_token",
%     expires = os:system_time() + 10
%   },
%   Sub = undefined,
%   token_store:store(Config, Sub, Token),
%   ?assertMatch(Token, token_store:find(Config, Sub)).
