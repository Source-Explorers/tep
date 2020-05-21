-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

default_directories_test() ->
    ?assertEqual(?CONFIG_PATHS, configuration_server:default_directories()).

default_filenames_test() ->
    ?assertEqual(?FILE_NAMES, configuration_server:default_filenames()).

choose_file_name_test() ->
    Paths = ["/etc/tep/", "./"],
    FileNames = ["config.ini", "tep.ini"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun
        (Path) when Path == "./tep.ini" -> true;
        (_) -> false
    end),
    ?assertEqual(
        {ok, "./tep.ini"},
        configuration_server:choose_config_file(Paths, FileNames)
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
