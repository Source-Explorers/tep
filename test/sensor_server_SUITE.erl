-module(sensor_server_SUITE).

-export([get_sensor_data_test/1, all/0]).

get_sensor_data_test(_) ->
    {ok, _Pid} = sensor_server:start_link(#{foo => fun () -> 42 end}),
    {ok, 42} = gen_server:call({sensor_server, node()}, {get_sensor_data, foo}, 2000).

all() -> [get_sensor_data_test].
