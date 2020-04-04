%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%% Created : 04. Apr 2020 3:47 PM
%%%-------------------------------------------------------------------
-module(storage).

-behaviour(gen_server).

%% API
-export([start_link/0, get_devices_by_class/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

% device -> #{id, class-name, class-type, location, node-name}
-record(device_info, {id, class_name, class_type, location, node_name}).
-type device_info() :: #device_info{}.

-record(storage_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(get_devices_by_class(Class :: atom()) ->
  list(device_info()) | {error, Reason :: term()}).
get_devices_by_class(_Class) ->
  {error, "not implemented"}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #storage_state{}} | {ok, State :: #storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #storage_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #storage_state{}) ->
  {reply, Reply :: term(), NewState :: #storage_state{}} |
  {reply, Reply :: term(), NewState :: #storage_state{}, timeout() | hibernate} |
  {noreply, NewState :: #storage_state{}} |
  {noreply, NewState :: #storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #storage_state{}} |
  {stop, Reason :: term(), NewState :: #storage_state{}}).
handle_call(_Request, _From, State = #storage_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #storage_state{}) ->
  {noreply, NewState :: #storage_state{}} |
  {noreply, NewState :: #storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #storage_state{}}).
handle_cast(_Request, State = #storage_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #storage_state{}) ->
  {noreply, NewState :: #storage_state{}} |
  {noreply, NewState :: #storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #storage_state{}}).
handle_info(_Info, State = #storage_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #storage_state{}) -> term()).
terminate(_Reason, _State = #storage_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #storage_state{},
    Extra :: term()) ->
  {ok, NewState :: #storage_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #storage_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
