%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements the server, that reads and stores the
%%% configuration for TEP. It presents an interface for other
%%% processes to access the configuration variables.
%%% @end
%%%-------------------------------------------------------------------
-module(configuration_server).

-behaviour(gen_server).

-include("configuration_server.hrl").

%%% Export these internal functions if eunit is testing this module
-ifdef(EUNIT).

-export([default_directories/0, default_filenames/0, choose_config_file/0]).

-endif.

%% API
-export([start_link/0, get_default_directories/0, get_config_file/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(configuration, {config_file_path :: file:filename_all()}).

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
    {ok, State :: #configuration{}} |
    {ok, State :: #configuration{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    case choose_config_file() of
        {ok, ConfigFilePath} -> {ok, #configuration{config_file_path = ConfigFilePath}};
        {error, Reason} -> {stop, Reason}
    end.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #configuration{}
) ->
    {reply, Reply :: term(), NewState :: #configuration{}} |
    {reply, Reply :: term(), NewState :: #configuration{}, timeout() | hibernate} |
    {noreply, NewState :: #configuration{}} |
    {noreply, NewState :: #configuration{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #configuration{}} |
    {stop, Reason :: term(), NewState :: #configuration{}}.
handle_call(config_file, _From, State = #configuration{}) ->
    {reply, State#configuration.config_file_path, State};
handle_call(default_directories, _From, State = #configuration{}) ->
    {reply, {ok, default_directories()}, State};
handle_call(_Request, _From, State = #configuration{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #configuration{}) ->
    {noreply, NewState :: #configuration{}} |
    {noreply, NewState :: #configuration{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #configuration{}}.
handle_cast(_Request, State = #configuration{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #configuration{}) ->
    {noreply, NewState :: #configuration{}} |
    {noreply, NewState :: #configuration{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #configuration{}}.
handle_info(_Info, State = #configuration{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #configuration{}
) ->
    term().
terminate(_Reason, _State = #configuration{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #configuration{},
    Extra :: term()
) ->
    {ok, NewState :: #configuration{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #configuration{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc This function returns the predefined configuration directories.
-spec default_directories() ->
    [file:filename_all()].
default_directories() ->
    ?CONFIG_PATHS.

%% @doc This function returns the predefined filenames for config files.
-spec default_filenames() ->
    [file:filename_all()].
default_filenames() ->
    ?FILE_NAMES.

%% @private
%% @doc This function searches for a configuration file in the default
%% directories and uses the first found file.
-spec choose_config_file() ->
    {ok, FileName :: file:filename_all()} | {error, Reason :: term()}.
choose_config_file() ->
    CandidateNames =
        [
            filename:absname_join(Path, File)
            || Path <- default_directories(), File <- default_filenames()
        ],
    case [File || File <- CandidateNames, filelib:is_regular(File)] of
        ConfigFiles when length(ConfigFiles) >= 1 -> {ok, lists:nth(1, ConfigFiles)};
        _ -> {error, {no_file, "No config file found"}}
    end.
