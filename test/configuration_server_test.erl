-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

configuration_server_test_() ->
    [
        ?_assertEqual(?CONFIG_PATHS, configuration_server:default_directories()),
        ?_assertEqual(
            {stop, {no_file, "No config file found"}},
            configuration_server:init([])
        )
    ].

config_file_name_test() ->
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun(_Path) -> true end),
    ?assertEqual(
        {ok, filename:absname("config.ini")},
        configuration_server:config_file_name()
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().