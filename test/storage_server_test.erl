%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("storage_server.hrl").

-define(DETS_DIR, "./tmp_test").
-define(CONFIG_DETS, "./tmp_test/config.dets").

storage_init_test() ->
    {ok,
     #storage_server_state{path = ?DETS_DIR,
                           config = config}} =
        storage_server:init(?DETS_DIR),
    ?assert(dets:is_dets_file(?CONFIG_DETS)),
    file:delete(?CONFIG_DETS).
