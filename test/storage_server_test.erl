%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/storage/storage_server.hrl").

storage_test_() ->
  [
    ?_assertEqual({ok, #storage_server_state{}}, storage_server:init(["."]))
  ].
