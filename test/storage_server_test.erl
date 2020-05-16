%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("storage_server.hrl").

storage_test_() ->
    TempDir = string:strip(os:cmd("mktemp -d"), right, $\n),
    {
        setup,
        fun () -> TempDir end,
        fun (Dir) -> os:cmd("rm -r " ++ Dir) end,
        {with, TempDir, [fun storage_init/1]}
    }.

storage_init(TempDir) ->
    FileName = filename:absname_join(TempDir, "config.dets"),
    {ok, #storage_server_state{path = TempDir, config = config}} =
        storage_server:init(TempDir),
    ?assert(dets:is_dets_file(FileName)).
