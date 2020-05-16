-module(id_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% business logic API
-export([get_id/1, shutdown/1]).

%%%
-ifdef(EUNIT).

-export([my_id/0]).

-endif.

-define(SERVER, ?MODULE).

-record(id_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link() ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Returns unique identifier for this process =!= PID and universally comparable
-spec get_id(Node :: node()) -> {ok, Id :: term()}.
get_id(Node) -> gen_server:call({id_server, Node}, id).

%% @doc Shuts down id_server
-spec shutdown(Node :: node()) -> {ok, stopped}.
shutdown(Node) ->
    gen_server:call({id_server, Node}, shutdown).

%%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #id_server_state{}} |
    {ok, State :: #id_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) -> {ok, #id_server_state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #id_server_state{}
) ->
    {reply, Reply :: term(), NewState :: #id_server_state{}} |
    {reply, Reply :: term(), NewState :: #id_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #id_server_state{}} |
    {noreply, NewState :: #id_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #id_server_state{}} |
    {stop, Reason :: term(), NewState :: #id_server_state{}}.
handle_call(id, _From, State = #id_server_state{}) ->
    {reply, {ok, my_id()}, State};
handle_call(shutdown, _From, State = #id_server_state{}) ->
    {stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State = #id_server_state{}) ->
    {reply, {error, bad_request}, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #id_server_state{}) ->
    {noreply, NewState :: #id_server_state{}} |
    {noreply, NewState :: #id_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #id_server_state{}}.
handle_cast(_Request, State = #id_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #id_server_state{}) ->
    {noreply, NewState :: #id_server_state{}} |
    {noreply, NewState :: #id_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #id_server_state{}}.
handle_info(_Info, State = #id_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason ::
        normal |
        shutdown |
        {shutdown, term()} | term(),
    State :: #id_server_state{}
) ->
    term().
terminate(_Reason, _State = #id_server_state{}) -> ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #id_server_state{},
    Extra :: term()
) ->
    {ok, NewState :: #id_server_state{}} |
    {error, Reason :: term()}.
code_change(_OldVsn, State = #id_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private (except in tests)
%% @doc Generates an identifier for the current host
-spec my_id() -> Id :: string().
my_id() ->
    erl_types:atom_to_string(node()) ++ "/id_server".
