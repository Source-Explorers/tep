-module(capability_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_sensor_server/2, stop_sensor_server/1, list_sensor_servers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(capability_server_state, {supervisor_name}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(SupervisorName :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupervisorName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SupervisorName], []).

-spec start_sensor_server(SensorName :: atom(), SensorFunction :: fun()) ->
    {ok, Ref :: pid()} | {error, Reason :: term()}.
start_sensor_server(SensorName, SensorFunction) ->
    gen_server:call(?SERVER, {start_sensor, SensorName, SensorFunction}).

-spec stop_sensor_server(SensorName :: atom()) ->
    ok | {error, Error :: term()}.
stop_sensor_server(SensorName) ->
    gen_server:call(?SERVER, {stop_server, SensorName}).

-spec list_sensor_servers() ->
    [{term(), pid()}].
list_sensor_servers() ->
    gen_server:call(?SERVER, list_sensors).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #capability_server_state{}} |
    {ok, State :: #capability_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([SupervisorName]) ->
    {ok, #capability_server_state{supervisor_name = SupervisorName}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #capability_server_state{}
) ->
    {reply, Reply :: term(), NewState :: #capability_server_state{}} |
    {reply, Reply :: term(), NewState :: #capability_server_state{},
        timeout() | hibernate} |
    {noreply, NewState :: #capability_server_state{}} |
    {noreply, NewState :: #capability_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #capability_server_state{}} |
    {stop, Reason :: term(), NewState :: #capability_server_state{}}.
handle_call(list_sensors, _From, State = #capability_server_state{}) ->
    {reply, {ok, list_sensors(State#capability_server_state.supervisor_name)}, State};
handle_call({stop_sensor, SensorName}, _From, State = #capability_server_state{}) ->
    {reply, stop_sensor(SensorName), State};
handle_call(
    {start_sensor, SensorName, SensorFunction},
    _From,
    State = #capability_server_state{}
) ->
    {reply, start_sensor(SensorName, SensorFunction), State};
handle_call(_Request, _From, State = #capability_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #capability_server_state{}) ->
    {noreply, NewState :: #capability_server_state{}} |
    {noreply, NewState :: #capability_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #capability_server_state{}}.
handle_cast(_Request, State = #capability_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #capability_server_state{}) ->
    {noreply, NewState :: #capability_server_state{}} |
    {noreply, NewState :: #capability_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #capability_server_state{}}.
handle_info(_Info, State = #capability_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #capability_server_state{}
) ->
    term().
terminate(_Reason, _State = #capability_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #capability_server_state{},
    Extra :: term()
) ->
    {ok, NewState :: #capability_server_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #capability_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec start_sensor(SensorName :: atom(), SensorFunction :: fun()) ->
    {ok, Ref :: pid()} | {error, Reason :: term()}.
start_sensor(SensorName, SensorFunction) ->
    SensorSpec = #{
        id => SensorName,
        start => {sensor_server, start_link, [SensorName, SensorFunction, 0]},
        restart => transient,
        shutdown => 1000
    },
    supervisor:start_child(capability_sup, SensorSpec).

-spec stop_sensor(SensorName :: atom()) ->
    ok | {error, Error :: term()}.
stop_sensor(SensorName) ->
    supervisor:terminate_child(capability_sup, SensorName),
    supervisor:delete_child(capability_sup, SensorName).

-spec list_sensors(SupervisorName :: atom()) ->
    [{term(), pid()}].
list_sensors(SupervisorName) ->
    [{Id, Pid} || {Id, Pid, worker, [sensor_server]} <- supervisor:which_children(SupervisorName)].

