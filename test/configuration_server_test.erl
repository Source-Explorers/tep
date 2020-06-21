-module(configuration_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("configuration_server.hrl").

list_config_file_path_candidates_test() ->
    Paths = ["/etc/tep/", "./"],
    FileNames = ["config.toml", "tep.toml"],
    CorrectPathList =
        ["/etc/tep/config.toml", "/etc/tep/tep.toml", "./config.toml", "./tep.toml"],
    ?assertEqual(
        CorrectPathList,
        configuration_server:list_config_file_path_candidates(Paths, FileNames)
    ).

existing_config_file_test() ->
    PathList = ["/etc/tep/config.toml", "/etc/tep/tep.toml", "./config.toml", "./tep.toml"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun
        (Path) when Path == "./tep.toml" -> true;
        (_) -> false
    end),
    ?assertEqual(
        {ok, "./tep.toml"},
        configuration_server:search_for_config_file_path(PathList)
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

multiple_config_file_test() ->
    PathList = ["/etc/tep/config.toml", "/etc/tep/tep.toml", "./config.toml", "./tep.toml"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun
        (Path) when Path == "./tep.toml" -> true;
        (Path) when Path == "./config.toml" -> true;
        (_) -> false
    end),
    ?assertEqual(
        {ok, "./config.toml"},
        configuration_server:search_for_config_file_path(PathList)
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

missing_config_file_test() ->
    PathList = ["/etc/tep/config.toml", "/etc/tep/tep.toml", "./config.toml", "./tep.toml"],
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun (_) -> false end),
    ?assertEqual(
        {error, {no_file, "No config file found"}},
        configuration_server:search_for_config_file_path(PathList)
    ),
    ?assert(meck:validate(filelib)),
    meck:unload().

init_test() ->
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_regular, fun (_Path) -> true end),
    ?assertEqual(
        {ok, {configuration, filename:absname_join(filename:absname(""), "config.toml")}},
        configuration_server:init(no_custom_config_path)
    ),
    ?assert(meck:validate(filelib)),
    meck:expect(filelib, is_regular, fun (_Path) -> false end),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init(no_custom_config_path)
    ),
    meck:unload(filelib).

init_with_valid_config_flag_test() ->
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(filelib, is_regular, fun
        ("./tep_test.toml") -> true;
        (Path) -> meck:passthrough([Path])
    end),
    ?assertEqual(
        {ok, {configuration, "./tep_test.toml"}},
        configuration_server:init(["./tep_test.toml"])
    ),
    ?assert(meck:validate(filelib)),
    meck:unload(filelib).

init_with_invalid_config_flag_test() ->
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(filelib, is_regular, fun
        ("./tep_test.toml") -> false;
        (Path) -> meck:passthrough([Path])
    end),
    ?assertEqual(
        {stop, {no_file, "No config file found"}},
        configuration_server:init(["./tep_test.toml"])
    ),
    ?assert(meck:validate(filelib)),
    meck:unload(filelib).
