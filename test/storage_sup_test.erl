%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_sup_test).

-include_lib("eunit/include/eunit.hrl").

init_test_() ->
  [
    ?_assertNotEqual(storage_sup:init([]), ignore),
    ?_assertEqual(
      storage_sup:init([]),
      {ok, {#{strategy => 'one_for_one',
        intensity => 1000,
        period => 3600},
        [
          #{id => main_storage,
            start => {storage_server, start_link, ["./tmp_test"]}
          }]
      }}
    )
  ].
