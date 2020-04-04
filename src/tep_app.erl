%%%-------------------------------------------------------------------
%% @doc tep public API
%% @end
%%%-------------------------------------------------------------------

-module(tep_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tep_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
