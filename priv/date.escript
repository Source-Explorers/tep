#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname date

main(_) ->
  {Date, _} = gen_server:call({date_server, tep@localhost}, get_date, 10000),
  io:write(Date).