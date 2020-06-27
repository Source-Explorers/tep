#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name date

main(_) ->
  Node = list_to_atom("tep@" ++ net_adm:localhost()),
  net_kernel:connect_node(Node),
  {Date, _} = gen_server:call({date_server, Node}, get_date, 2000),
  io:write(Date).
