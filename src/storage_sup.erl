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
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  Storage = #{
    id => 'main_storage',
    start => {storage, start_link, []}
  },

%%  AChild = #{id => 'AName',
%%    start => {'AModule', start_link, []},
%%    restart => permanent,
%%    shutdown => 2000,
%%    type => worker,
%%    modules => ['AModule']},

  {ok, {SupFlags, [Storage]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%src/storage_sup.erl
%%36: The return type {'error',_} | {'ok',{{'one_for_all',non_neg_integer(),0} | {'one_for_one',non_neg_integer(),0} | {'rest_for_one',non_neg_integer(),0} | {'simple_one_for_one',non_neg_integer(),0},[{_,{atom(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom()]} | #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}} in the specification of init/1 is not a subtype of 'ignore' | {'ok',{{'one_for_all',non_neg_integer(),pos_integer()} | {'one_for_one',non_neg_integer(),pos_integer()} | {'rest_for_one',non_neg_integer(),pos_integer()} | {'simple_one_for_one',non_neg_integer(),pos_integer()} | #{'intensity'=>non_neg_integer(), 'period'=>pos_integer(), 'strategy'=>'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one'},[{_,{atom(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom()]} | #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}}, which is the expected return type for the callback of supervisor behaviour
%%36: Invalid type specification for function storage_sup:init/1. The success typing is ([]) -> {'ok',{#{'intensity':=1000, 'period':=3600, 'strategy':='one_for_one'},[map(),...]}}

%%src/storage_sup.erl
%%36: The return type {'error',_} | {'ok',{{'one_for_all',non_neg_integer(),0} | {'one_for_one',non_neg_integer(),0} | {'rest_for_one',non_neg_integer(),0} | {'simple_one_for_one',non_neg_integer(),0},[{_,{atom(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom()]} | #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}} in the specification of init/1 is not a subtype of 'ignore' | {'ok',{{'one_for_all',non_neg_integer(),pos_integer()} | {'one_for_one',non_neg_integer(),pos_integer()} | {'rest_for_one',non_neg_integer(),pos_integer()} | {'simple_one_for_one',non_neg_integer(),pos_integer()} | #{'intensity'=>non_neg_integer(), 'period'=>pos_integer(), 'strategy'=>'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one'},[{_,{atom(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom()]} | #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}}, which is the expected return type for the callback of supervisor behaviour
%%36: Invalid type specification for function storage_sup:init/1. The success typing is ([]) -> {'ok',{#{'intensity':=1000, 'period':=3600, 'strategy':='one_for_one'},[{_,_},...]}}
%%41: The inferred return type of init/1 ({'ok',{#{'intensity':=1000, 'period':=3600, 'strategy':='one_for_one'},[{_,_},...]}}) has nothing in common with 'ignore' | {'ok',{{'one_for_all',non_neg_integer(),pos_integer()} | {'one_for_one',non_neg_integer(),pos_integer()} | {'rest_for_one',non_neg_integer(),pos_integer()} | {'simple_one_for_one',non_neg_integer(),pos_integer()} | #{'intensity'=>non_neg_integer(), 'period'=>pos_integer(), 'strategy'=>'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one'},[{_,{atom(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom()]} | #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}}, which is the expected return type for the callback of supervisor behaviour
