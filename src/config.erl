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
    Value :: {ok,  term()},
    Error :: {error, atom()}.
get(Key) ->
  gen_server:call(?SERVER, {get,
                            Key}).

init(State) ->
  {ok, DynamicConfig} = config_mod_init(State),
  {ok, Config} = load_and_init(DynamicConfig),
  {ok, Config}.

load_and_init(AppConfig) ->
  % TODO: figure out how this looks in erlang
  % Config = from_json(AppConfig) or from_config(AppConfig) or from_creds_file(AppConfig) or
  %   from_gcloud_adc(AppConfig) or from_metadata(AppConfig),

  ConfigTuple = {from_json(AppConfig), from_config(AppConfig), from_creds_file(AppConfig), from_gcloud_adc(AppConfig), from_metadata(AppConfig)},
  Config = lists:filter(fun(Elem) ->
      is_map(Elem)
    end
    , tuple_to_list(ConfigTuple)),
  {ok, Config2} = map_config(lists:last(Config)),
  ActorEmail = maps:get(<<"actor_email">>, AppConfig, undefined),
  ProjectId = determine_project_id(Config2, AppConfig),
  Config3 = maps:put(<<"project_id">>, ProjectId, Config2),
  Config4 = maps:put(<<"actor_email">>, ActorEmail, Config3),

  {ok, Config4}.

handle_call({get, Key}, _From, State) ->
  GcpCredentials = maps:get("gcp_credentials", State),
  Value = maps:get(Key, GcpCredentials),
  {reply, Value, State}.

handle_cast(_, State) -> {noreply, State}.

%%%_ * Private functions -----------------------------------------------

% TODO: rewrite or statement into tuple like above...
determine_project_id(_Config, _DynamicConfig) ->
  os:getenv("GOOGLE_CLOUD_PROJECT").
  % case maps:get(<<"project_id">>, DynamicConfig, false) or
  %   os:getenv("GOOGLE_CLOUD_PROJECT") or
  %   os:getenv("GCLOUD_PROJECT") or
  %   os:getenv("DEVSHELL_PROJECT_ID") or
  %   maps:get(<<"project_id">>, Config, false) of
  %   false ->
  %     try egoth:retrieve_metadata_project() of
  %       ProjectId -> ProjectId
  %     catch
  %     % TODO: catch on specific error
  %         error -> erlang:error("Failed to retrieve project data from GCE internal metadata service.
  %           Either you haven't configured your GCP credentials, you aren't running on GCE, or both.
  %           Please see README.md for instructions on configuring your credentials.")
  %     end;
  %   ProjectId -> ProjectId
  % end.


map_config(Config) when is_map(Config) ->
  {ok, Config};
map_config({client_email = Account} = Config) ->
  Config2 = #{<<"account">> => Account, <<"config">> => Config},
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
  {ok, #{}}.

% TODO: implement
from_json(Config) ->
  io:fwrite("from_json"),
  case maps:get(<<"json">>, Config, false) of
    false -> false;
    % {:system, var} -> decode_json(System.get_env(var))
    Json -> decode_json(Json)
  end.

% TODO: implement
from_config(Config) ->
  io:fwrite("from_config"),
  maps:get(<<"config">>, Config, false).

from_creds_file(_Config) ->
  io:fwrite("from_creds_file~n"),
  io:fwrite(os:getenv("GOOGLE_APPLICATION_CREDENTIALS")),
  case os:getenv("GOOGLE_APPLICATION_CREDENTIALS") of
    false -> false;
    CredentialsPath ->
      {ok, File} = file:read_file(CredentialsPath),
      decode_json(File)
  end.

% TODO: implement
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
  {ok, {_, _Size, Type, _Access, _, _, _CTime, _, _, _, _, _, _, _}} = file:read_file_info(Path),
  if Type == regular ->
    {ok, File} = file:read_file(Path),
    io:fwrite("reading file~n"),
    decode_json(File);
  true ->
    false
  end.

from_metadata(_) ->
  #{<<"token_source">> => metadata}.

decode_json(Json) ->
  Config = jiffy:decode(Json, [return_maps]),
  io:fwrite(maps:get(<<"refresh_token">>, Config)),
  set_token_source(Config).

set_token_source(#{<<"refresh_token">> := _, <<"client_id">> := _, <<"client_secret">> := _} = Map) ->
  io:fwrite(maps:get(<<"refresh_token">>, Map)),
  maps:put(<<"token_source">>, oauth_refresh, Map);
set_token_source(#{<<"private_key">> := _} = Map)->
  maps:put(<<"token_source">>, oauth_jwt, Map).

%%%_ * Tests -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
