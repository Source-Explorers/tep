%%%-------------------------------------------------------------------
%% @doc tep public API
%% @end
%%%-------------------------------------------------------------------
-module(tep_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> tep_sup:start_link(get_config_file_path(config_file)).

stop(_State) -> ok.

%% internal functions
-spec get_config_file_path(Name :: atom()) ->
    string().
get_config_file_path(Name) ->
    case init:get_argument(Name) of
        {ok, [[Value | _] | _]} -> Value;
        _ -> no_path
    end.