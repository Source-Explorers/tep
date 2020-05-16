-module(id_server_test).

-include_lib("eunit/include/eunit.hrl").

my_id_test_() ->
    [?_assertEqual((id_server:my_id()), (id_server:my_id()))].
