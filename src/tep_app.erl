%%%-------------------------------------------------------------------
%% @doc tep public API
%% @end
%%%-------------------------------------------------------------------
-module(tep_app).

-behaviour(application).

%%% Export these internal functions if EUnit is testing this module
-ifdef(EUNIT).

-export([get_config_argument/1]).

-endif.

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> tep_sup:start_link(get_config_file_path()).

stop(_State) -> ok.

%% internal functions
%% @private
%% @doc Reads the arguments from the erlang startup and checks for config_file argument
-spec get_config_file_path() ->
    string() | no_custom_config_path.
get_config_file_path() ->
    ArgumentList = init:get_argument(tep_config_file),
    get_config_argument(ArgumentList).

%% @private
%% @doc Parses list of config_file arguments read from the erlang startup
-spec get_config_argument({ok, Args :: [Values :: [string()]]} | error) ->
    string() | no_custom_config_path.
get_config_argument({ok, [[Value | _] | _]}) -> Value;
get_config_argument(_) -> no_custom_config_path.
