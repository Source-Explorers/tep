-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

existing_config_file_test() ->
    Paths = ["/etc/tep/", "./"],
    FileNames = ["config.ini", "tep.ini"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun
        (Path) when Path == "./tep.ini" -> true;
        (_) -> false
    end),
    ?assertEqual(
        {ok, "./tep.ini"},
        configuration_server:search_for_config_file_path(Paths, FileNames)
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

multiple_config_file_test() ->
    Paths = ["/etc/tep/", "./"],
    FileNames = ["config.ini", "tep.ini"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun
        (Path) when Path == "./tep.ini" -> true;
        (Path) when Path == "./config.ini" -> true;
        (_) -> false
    end),
    ?assertEqual(
        {ok, "./config.ini"},
        configuration_server:search_for_config_file_path(Paths, FileNames)
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

missing_config_file_test() ->
    Paths = ["/etc/tep/", "./"],
    FileNames = ["config.ini", "tep.ini"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun (_) -> false end),
    ?assertEqual(
        {error, {no_file, "No config file found"}},
        configuration_server:search_for_config_file_path(Paths, FileNames)
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
    meck:expect(filelib, is_regular, fun (_Path) -> false end),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init([])
    ).
