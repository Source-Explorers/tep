#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname sensor
connect() ->
  Node = list_to_atom("tep@" ++ net_adm:localhost()),
  net_kernel:connect_node(Node),
  Node.

main([]) ->
  Node = connect(),
  {ok, Sensors} = gen_server:call({capability_server, Node}, list_sensors),
  io:fwrite("~w~n", [Sensors]);
main([Arg|_]) ->
  Node = connect(),
  {ok, SensorState} = gen_server:call({list_to_atom(Arg), Node}, get_sensor_data),
  io:fwrite("~w~n", [SensorState]).
