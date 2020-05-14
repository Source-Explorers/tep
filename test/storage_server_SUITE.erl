%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_server_SUITE).

%% API
-export([start_storage_server/1, all/0]).

start_storage_server(_) ->
    {ok, Pid} = storage_server:start_link("."),
    Pid = whereis(storage_server).

all() ->
    [start_storage_server].
