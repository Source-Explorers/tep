%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2020 6:51 PM
%%%-------------------------------------------------------------------
-module(configuration_server_SUITE).

-include("configuration_server.hrl").

%% API
-export([
  all/0,
  start_config_server/1,
  get_default_locations/1,
  get_config_file/1
]).

all() ->
  [
    start_config_server,
    get_default_locations,
    get_config_file
  ].

start_config_server(_) ->
  {ok, Pid} = configuration_server:start_link(),
  Pid =:= whereis(configuration_server).

get_default_locations(_) ->
  DefaultDirs = ?CONFIG_PATHS,

  {ok, _Pid} = configuration_server:start_link(),
  {ok, Dirs} = configuration_server:get_default_directories(),

  lists:sort(DefaultDirs) =:= lists:sort(Dirs).

get_config_file(_) ->
  {ok, _Pid} = configuration_server:start_link(),
  {error, {no_file, _Desc}} = configuration_server:get_config_file().