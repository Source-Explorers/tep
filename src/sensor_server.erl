-module(sensor_server).

-behaviour(gen_server).

%% API
-export([start_link/3, call_sensor/2, call_sensor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sensor_server_state, {capabilityfunction :: fun(), arity :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(
    ServerName :: atom(),
    CapabilityFunction :: map(),
    Arity :: non_neg_integer()
) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(ServerName, CapabilityFunction, Arity) ->
    gen_server:start_link({local, ServerName}, ?MODULE, {CapabilityFunction, Arity}, []).

-spec call_sensor(ServerName :: atom(), Arguments :: [atom()]) ->
    {ok, term()} | {error, badarg}.
call_sensor(ServerName, []) ->
    gen_server:call(ServerName, {get_sensor_data});
call_sensor(ServerName, Arguments) ->
    gen_server:call(ServerName, {get_sensor_data, Arguments}).

-spec call_sensor(ServerName :: atom()) ->
    {ok, term()} | {error, badarg}.
call_sensor(ServerName) ->
    gen_server:call(ServerName, {get_sensor_data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init({CapabilityFunction :: fun(), Arity :: non_neg_integer()}) ->
    {ok, State :: #sensor_server_state{}} |
    {ok, State :: #sensor_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init({CapabilityFunction, Arity}) ->
    {ok, #sensor_server_state{capabilityfunction = CapabilityFunction, arity = Arity}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #sensor_server_state{}
) ->
    {reply, Reply :: term(), NewState :: #sensor_server_state{}} |
    {reply, Reply :: term(), NewState :: #sensor_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #sensor_server_state{}} |
    {noreply, NewState :: #sensor_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #sensor_server_state{}} |
    {stop, Reason :: term(), NewState :: #sensor_server_state{}}.
handle_call(get_sensor_data, _From, State = #sensor_server_state{})
        when State#sensor_server_state.arity =:= 0 ->
    SensorFunction = State#sensor_server_state.capabilityfunction,
    {reply, {ok, SensorFunction()}, State};
handle_call({get_sensor_data, Arguments}, _From, State = #sensor_server_state{})
        when length(Arguments) =:= State#sensor_server_state.arity ->
    SensorFunction = State#sensor_server_state.capabilityfunction,
    {reply, {ok, SensorFunction(Arguments)}, State};
handle_call({get_sensor_data, _Arguments}, _From, State = #sensor_server_state{}) ->
    {reply, {error, badarg}, State};
handle_call(_Request, _From, State = #sensor_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #sensor_server_state{}) ->
    {noreply, NewState :: #sensor_server_state{}} |
    {noreply, NewState :: #sensor_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #sensor_server_state{}}.
handle_cast(_Request, State = #sensor_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #sensor_server_state{}) ->
    {noreply, NewState :: #sensor_server_state{}} |
    {noreply, NewState :: #sensor_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #sensor_server_state{}}.
handle_info(_Info, State = #sensor_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #sensor_server_state{}
) ->
    term().
terminate(_Reason, _State = #sensor_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #sensor_server_state{},
    Extra :: term()
) ->
    {ok, NewState :: #sensor_server_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #sensor_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
