%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2020 6:49 PM
%%%-------------------------------------------------------------------
-module(configuration_server).

-behaviour(gen_server).

-include("configuration_server.hrl").

%% API
-export([
    start_link/0,
    get_default_directories/0,
    get_config_file/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(configuration_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Returns a list with the default configuration file locations
-spec get_default_directories() ->
    {ok, [file:filename_all()]}.
get_default_directories() ->
    gen_server:call(configuration_server, default_directories).

%% @doc Returns the used configuration file
-spec get_config_file() ->
    {ok, file:filename_all()} | {error, Reason :: term()}.
get_config_file() ->
    gen_server:call(configuration_server, config_file).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #configuration_server_state{}} |
    {ok, State :: #configuration_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    {ok, #configuration_server_state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #configuration_server_state{}
) ->
    {reply, Reply :: term(), NewState :: #configuration_server_state{}} |
    {reply, Reply :: term(), NewState :: #configuration_server_state{},
        timeout() | hibernate} |
    {noreply, NewState :: #configuration_server_state{}} |
    {noreply, NewState :: #configuration_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #configuration_server_state{}} |
    {stop, Reason :: term(), NewState :: #configuration_server_state{}}.
handle_call(config_file, _From, State = #configuration_server_state{}) ->
    {reply, config_file_name(), State};
handle_call(default_directories, _From, State = #configuration_server_state{}) ->
    {reply, {ok, default_directories()}, State};
handle_call(_Request, _From, State = #configuration_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #configuration_server_state{}) ->
    {noreply, NewState :: #configuration_server_state{}} |
    {noreply, NewState :: #configuration_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #configuration_server_state{}}.
handle_cast(_Request, State = #configuration_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #configuration_server_state{}) ->
    {noreply, NewState :: #configuration_server_state{}} |
    {noreply, NewState :: #configuration_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #configuration_server_state{}}.
handle_info(_Info, State = #configuration_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #configuration_server_state{}
) ->
    term().
terminate(_Reason, _State = #configuration_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #configuration_server_state{},
    Extra :: term()
) ->
    {ok, NewState :: #configuration_server_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #configuration_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc This function returns the predefined configuration directories.
-spec default_directories() ->
    [file:filename_all()].
default_directories() ->
    ?CONFIG_PATHS.

%% @private
%% @doc This function searches for a configuration file in the default
%% directories and uses the first found file.
-spec config_file_name() ->
    {ok, FileName :: file:filename_all()} | {error, Reason :: term()}.
config_file_name() ->
    PossibleFileNames =
        [
            filename:absname_join(Path, File)
            || Path <- default_directories(), File <- ?FILE_NAMES
        ],
    ConfigFiles = [File || File <- PossibleFileNames, filelib:is_regular(File)],
    Result =
        if
            length(ConfigFiles) >= 1 ->
                [ConfigFile | _Rest] = ConfigFiles,
                ConfigFile;
            true ->
                {error, {no_file, "No config file found"}}
        end,
    Result.
