%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%% Created : 04. Apr 2020 3:29 PM
%%%-------------------------------------------------------------------
-module(storage_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
-spec(init(Args :: term()) ->
  { ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} |
  ignore).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = #{strategy => 'one_for_one',
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  StorageServer = #{id => main_storage,
    start => {storage_server, start_link, ["./tmp_test"]}},

  {ok, {SupFlags, [StorageServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
