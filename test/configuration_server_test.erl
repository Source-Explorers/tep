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
        configuration_server:init([no_path])
    ),
    ?assert(meck:validate(filelib)),
    meck:expect(filelib, is_regular, fun (_Path) -> false end),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init([no_path])
    ),
    meck:unload(filelib).

init_with_valid_config_flag_test() ->
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(filelib, is_regular, fun
        ("./tep_test.ini") -> true;
        (Path) -> meck:passthrough([Path])
    end),
    ?assertEqual(
        {ok, {configuration, "./tep_test.ini"}},
        configuration_server:init(["./tep_test.ini"])
    ),
    ?assert(meck:validate(filelib)),
    meck:unload(filelib).

init_with_invalid_config_flag_test() ->
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(filelib, is_regular, fun
        ("./tep_test.ini") -> false;
        (Path) -> meck:passthrough([Path])
    end),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init(["./tep_test.ini"])
    ),
    ?assert(meck:validate(filelib)),
    meck:unload(filelib).
