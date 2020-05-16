%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2020 8:04 PM
%%%-------------------------------------------------------------------
-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

configuration_server_test_() ->
    [
        ?_assertEqual(?CONFIG_PATHS, configuration_server:default_directories()),
        ?_assertEqual(
            {error, {no_file, "No config file found"}},
            configuration_server:config_file_name()
        ),
        ?_assertEqual(
            {stop, {no_file, "No config file found"}},
            configuration_server:init([])
        )
    ].
