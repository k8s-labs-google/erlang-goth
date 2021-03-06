-module(config).

-behaviour(gen_server).

-include("egoth.hrl").

-export([start/0]).
-export([start_link/0]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%% API
-export([get/0]).

-define(SERVER, ?MODULE).

-spec start() -> {atom(), pid()}.
start() ->
  gen_server:start({local, ?SERVER}, ?SERVER, {}, []).

-spec start_link() -> {atom(), pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, {}, []).

-spec get() -> Value | Error when
    Value :: {ok,  #config{}},
    Error :: {error, atom()}.
get() ->
  gen_server:call(?SERVER, {get}).

init(State) when is_tuple(State) ->
  % {ok, DynamicConfig} = config_mod_init(State),
  % {ok, Config} = load_and_init(DynamicConfig),
  % {ok, Config};
  init(#{});
init(#{}=State) ->
  % TODO: check for passed in args, or the ability to specify a config file
  {ok, DynamicConfig} = config_mod_init(State),
  {ok, Config} = load_and_init(DynamicConfig),
  {ok, Config}.

-spec load_and_init(Config) -> Return when
  Config :: #config{},
  Return :: {ok, #config{}}.
load_and_init(#config{}=AppConfig) ->
  ConfigTuple = {from_json(AppConfig), from_config(AppConfig), from_creds_file(AppConfig), from_gcloud_adc(AppConfig), from_metadata(AppConfig)},
  Config = lists:filter(fun(Elem) ->
      is_map(Elem)
    end
    , tuple_to_list(ConfigTuple)),
  {Config2, _} = lists:split(1, Config),
  {ok, Config3} = map_config(lists:last(Config2)),
  ActorEmail = AppConfig#config.actor_email,
  ProjectId = determine_project_id(Config3, AppConfig),
  Config4 = Config3#config{project_id = ProjectId, actor_email = ActorEmail},
  {ok, Config4}.

% maps vs records, oh my!
handle_call({get}, _From, State) ->
  % GcpCredentials = maps:get("gcp_credentials", State),
  % Value = maps:get(Key, State, undefined),
  {reply, State, State}.

handle_cast(_, State) -> {noreply, State}.
%%%_ * Private functions -----------------------------------------------

% TODO: rewrite or statement into tuple like above...
determine_project_id(_Config, _DynamicConfig) ->
  ConfigTuple = {
    % maps:get(<<"project_id">>, DynamicConfig, false),
    os:getenv("GOOGLE_CLOUD_PROJECT"),
    os:getenv("GCLOUD_PROJECT"),
    os:getenv("DEVSHELL_PROJECT_ID")
    % maps:get(<<"project_id">>, Config, false)
  },
  % io:fwrite(tuple_to_list(ConfigTuple)),

  Fig = lists:filter(fun(Elem) ->
    % change test
    % is_map(Elem)
    Elem
  end
  , tuple_to_list(ConfigTuple)),
  io:fwrite(Fig),

  Config2 = case length(tuple_to_list(ConfigTuple)) > 0 of
    true ->
      lists:filter(fun(Elem) ->
        % change test, will this be true for strings?
        % is_map(Elem)
        Elem
      end
      , tuple_to_list(ConfigTuple));
    false ->
      {false}
  end,
  Config3 = case length(Config2) == 0 of
  true ->
    false;
  false ->
    {Config4, _} = lists:split(1, Config2),
    Config4
  end,
  if Config3 == false ->
    try egoth:retrieve_metadata_project() of
      ProjectId -> ProjectId
    catch
      error -> erlang:error("Failed to retrieve project data from GCE internal metadata service.
            Either you haven't configured your GCP credentials, you aren't running on GCE, or both.
            Please see README.md for instructions on configuring your credentials.")
    end;
  true -> Config3
  end.

- spec map_config(Config) -> Return when
  Config :: map(),
  Return :: {ok, #config{}}.
map_config(Config) when is_map(Config) ->
  Fields = record_info(fields, config),
  [Tag| Values] = tuple_to_list(#config{}),
  Defaults = lists:zip(Fields, Values),
  L = lists:map(fun ({_K, _V}) -> {_K, maps:get(list_to_binary(atom_to_list(_K)), Config, undefined)} end, Defaults),
  Tupes = list_to_tuple([Tag|L]),
  {ok, Tupes};
map_config(#config{client_email = Account} = Config) ->
  Config2 = #config{account = Account, config = Config},
  {ok, Config2}.

% TODO: implement
config_mod_init(_Config) ->
  % case Keyword.get(config, :config_module) do
  %     nil ->
  %       {:ok, config}

  %     mod ->
  %       if Code.ensure_loaded?(mod) and function_exported?(mod, :init, 1) do
  %         mod.init(config)
  %       else
  %         {:ok, config}
  %       end
  %   end
  {ok, #config{}}.

% will never return json atm (config is always undefined)
- spec from_json(Config) -> Return when
  Config :: #config{},
  Return :: boolean() | map().
from_json(Config) ->
  case Config#config.json of
    'undefined' -> false;
    % {:system, var} -> decode_json(System.get_env(var))
    Json -> decode_json(Json)
  end.

% will never return json atm (config is always undefined)
from_config(Config) ->
  case Config#config.config of
    undefined -> false;
    % {:system, var} -> decode_json(System.get_env(var))
    Json -> decode_json(Json)
  end.

%%----------------------------
%% @doc
%% @end
%%----------------------------
from_creds_file(_Config) ->
  case os:getenv("GOOGLE_APPLICATION_CREDENTIALS") of
    false -> false;
    CredentialsPath ->
      {ok, File} = file:read_file(CredentialsPath),
      decode_json(File)
  end.

% Search the well-known path for application default credentials provided
% by the gcloud sdk. Note there are different paths for unix and windows.
from_gcloud_adc(_Config) ->
  % # config_root_dir = Application.get_env(:goth, :config_root_dir)
  % config_root_dir = Keyword.get(config, :config_root_dir)
  % ConfigRootDir = undefined,
  PathRoot = case os:type() of
    {win32, _} ->
      os:getenv("APPDATA", "");
    {unix, _} ->
      HomeDir = os:getenv("HOME", ""),
      filename:join([HomeDir, ".config"])
  end,
  Path = filename:join([PathRoot, "gcloud", "application_default_credentials.json"]),
  % creats a map #{}, {} reads a map
  case file:read_file_info(Path) of
    {error, _} ->
      false;
    {ok, {_, _Size, Type, _Access, _, _, _CTime, _, _, _, _, _, _, _}} ->
      if Type == regular ->
        {ok, File} = file:read_file(Path),
        decode_json(File);
      true ->
        false
      end
  end.

from_metadata(_) ->
  #{<<"token_source">> => metadata}.

decode_json(Json) ->
  Config = jiffy:decode(Json, [return_maps]),
  set_token_source(Config).

set_token_source(#{<<"refresh_token">> := _, <<"client_id">> := _, <<"client_secret">> := _} = Map) ->
  maps:put(<<"token_source">>, oauth_refresh, Map);
set_token_source(#{<<"private_key">> := _} = Map)->
  maps:put(<<"token_source">>, oauth_jwt, Map).

%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
