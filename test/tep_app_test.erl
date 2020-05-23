-module(tep_app_test).

-include_lib("eunit/include/eunit.hrl").

get_config_argument_valid_test() ->
    Valid = {ok, [["test.ini", "config.ini"], ["tep.ini"]]},
    ?assertEqual("test.ini", tep_app:get_config_argument(Valid)).

get_config_argument_invalid_test() ->
    ?assertEqual(no_path, tep_app:get_config_argument(error)).
