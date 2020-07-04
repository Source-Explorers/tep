#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname sensor

main([Arg|_]) ->
  Node = list_to_atom("tep@" ++ net_adm:localhost()),
  net_kernel:connect_node(Node),
  {ok, SensorState} = gen_server:call({sensor_server, Node}, {get_sensor_data, list_to_atom(Arg)}, 2000),
  io:fwrite("~w~n", [SensorState]).
