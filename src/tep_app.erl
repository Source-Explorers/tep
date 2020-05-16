%%%-------------------------------------------------------------------
%% @doc tep public API
%% @end
%%%-------------------------------------------------------------------
-module(tep_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> tep_sup:start_link(get_argument_string(config_file)).

stop(_State) -> ok.

%% internal functions
-spec get_argument_string(Name :: atom()) ->
    string().
get_argument_string(Name) ->
    case init:get_argument(Name) of
        {ok, [[Value] | _]} -> Value;
        _ -> no_path
    end.