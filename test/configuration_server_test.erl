-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

default_directories_test() ->
    ?assertEqual(?CONFIG_PATHS, configuration_server:default_directories()).

config_file_name_test() ->
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun (_Path) -> true end),
    ?assertEqual(
        {ok, filename:absname("config.ini")},
        configuration_server:config_file_name()
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

init_test() ->
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun (_Path) -> true end),
    ?assertEqual(
        {ok, {configuration, filename:absname("config.ini")}},
        configuration_server:init([])
    ),
    ?assert(meck:validate(filelib)),
    meck:unload(),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init([])
    ).
