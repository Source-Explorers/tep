-module(id_server_SUITE).

-export([get_id_test/1, shutdown_test/1, all/0]).

get_id_test(_) ->
    {ok, _Pid} = id_server:start_link(),
    {ok, "nonode@nohost/id_server"} = id_server:get_id(node()).

shutdown_test(_) ->
    {ok, _Pid} = id_server:start_link(),
    {ok, stopped} = id_server:shutdown(node()).

all() -> [get_id_test, shutdown_test].
