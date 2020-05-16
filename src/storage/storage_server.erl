%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(storage_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("storage_server.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(Path :: file:name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Path) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Path, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init(Path :: file:name()) ->
    {ok, State :: #storage_server_state{}} |
    {ok, State :: #storage_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(Path) ->
    {ok, Config} = dets:open_file(config, [{file, Path ++ "/config.dets"}]),
    {ok, #storage_server_state{path = Path, config = Config}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #storage_server_state{}
) ->
    {reply, Reply :: term(), NewState :: #storage_server_state{}} |
    {reply, Reply :: term(), NewState :: #storage_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #storage_server_state{}} |
    {noreply, NewState :: #storage_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #storage_server_state{}} |
    {stop, Reason :: term(), NewState :: #storage_server_state{}}.
handle_call(_Request, _From, State = #storage_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #storage_server_state{}) ->
    {noreply, NewState :: #storage_server_state{}} |
    {noreply, NewState :: #storage_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #storage_server_state{}}.
handle_cast(_Request, State = #storage_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #storage_server_state{}) ->
    {noreply, NewState :: #storage_server_state{}} |
    {noreply, NewState :: #storage_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #storage_server_state{}}.
handle_info(_Info, State = #storage_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #storage_server_state{}
) ->
    term().
terminate(_Reason, State = #storage_server_state{}) ->
    dets:close(State#storage_server_state.config).

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #storage_server_state{},
    Extra :: term()
) ->
    {ok, NewState :: #storage_server_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #storage_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
