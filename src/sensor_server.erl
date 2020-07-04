-module(sensor_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sensor_server_state, {capabilities :: map()}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(Capabilities :: map()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Capabilities) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Capabilities, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init(Capabilities :: map()) ->
    {ok, State :: #sensor_server_state{}} |
    {ok, State :: #sensor_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(Capabilities) ->
    {ok, #sensor_server_state{capabilities = Capabilities}}.

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
handle_call({get_sensor_data, Capability}, _From, State = #sensor_server_state{}) ->
    SensorFunction = maps:get(Capability, State#sensor_server_state.capabilities),
    {reply, {ok, SensorFunction()}, State};
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
