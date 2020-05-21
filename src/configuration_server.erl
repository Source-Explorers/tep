%%%-------------------------------------------------------------------
%%% @doc
%%% The configuration_server reads and stores the configuration  for
%%% TEP. It presents an interface for other processes to access
%%% configuration variables.
%%% @end
%%%-------------------------------------------------------------------
-module(configuration_server).

-behaviour(gen_server).

-include("configuration_server.hrl").

%%% Export these internal functions if EUnit is testing this module
-ifdef(EUNIT).

-export([
    prepare_default_directories/0,
    prepare_default_filenames/0,
    search_config_file/2,
    select_config_file/1,
    compile_file_paths/2,
    filter_existing_files/1,
    create_configuration/1
]).

-endif.

%% API
-export([
    start_link/0,
    get_default_directories/0,
    get_default_filenames/0,
    get_config_file_path/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
    gen_server:call(configuration_server, get_default_directories).

%% @doc Returns a list with the default configuration file locations
-spec get_default_filenames() ->
    {ok, [file:filename_all()]}.
get_default_filenames() ->
    gen_server:call(configuration_server, get_default_filenames).

%% @doc Returns the used configuration file
-spec get_config_file_path() ->
    {ok, file:filename_all()} | {error, Reason :: term()}.
get_config_file_path() ->
    gen_server:call(configuration_server, get_config_file_path).

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
    Directories = prepare_default_directories(),
    FileNames = prepare_default_filenames(),
    ConfigFilePath = search_config_file(Directories, FileNames),
    create_configuration(ConfigFilePath).

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
handle_call(get_config_file_path, _From, State = #configuration{}) ->
    {reply, State#configuration.config_file_path, State};
handle_call(get_default_directories, _From, State = #configuration{}) ->
    {reply, {ok, prepare_default_directories()}, State};
handle_call(get_default_filenames, _From, State = #configuration{}) ->
    {reply, {ok, prepare_default_filenames()}, State};
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
-spec prepare_default_directories() ->
    [file:filename_all()].
prepare_default_directories() ->
    ?CONFIG_PATHS.

%% @doc This function returns the predefined filenames for config files.
-spec prepare_default_filenames() ->
    [file:filename_all()].
prepare_default_filenames() ->
    ?FILE_NAMES.

%% @private
%% @doc This function searches for a configuration file in the default
%% directories and uses the first found file.
-spec search_config_file(
    Directories :: [file:filename_all()],
    FileNames :: [file:filename_all()]
) ->
    {ok, FileName :: file:filename_all()} | {error, Reason :: term()}.
search_config_file(Directories, FileNames) ->
    CandidateNames = compile_file_paths(Directories, FileNames),
    Candidates = filter_existing_files(CandidateNames),
    select_config_file(Candidates).

%% @private
%% @doc Compile paths for configuration files from directories nd filenames
-spec compile_file_paths(
    Directories :: [file:filename_all()],
    FileNames :: [file:filename_all()]
) ->
    [file:filename_all()].
compile_file_paths(Directories, FileNames) ->
    [filename:absname_join(Path, File) || Path <- Directories, File <- FileNames].

%% @private
%% @doc Filter non existing configuration files from the list of candidates
-spec filter_existing_files(CandidateNames :: [file:filename_all()]) ->
    [file:filename_all()].
filter_existing_files(CandidateNames) ->
    [CandidateFile || CandidateFile <- CandidateNames, filelib:is_regular(CandidateFile)].

%% @private
%% @doc Choose the config file from a list of candidates
-spec select_config_file([file:filename_all()]) ->
    {ok, FileName :: file:filename_all()} | {error, Reason :: term()}.
select_config_file([ConfigFile | _]) -> {ok, ConfigFile};
select_config_file([]) -> {error, {no_file, "No config file found"}}.

%% @private
%% @doc Create a configuration from a path to a config file
-spec create_configuration(
    {ok, ConfigFile :: file:filename_all()} | {error, Reason :: term()}
) ->
    {ok, #configuration{}} | {stop, Reason :: term()}.
create_configuration({ok, ConfigFilePath}) ->
    {ok, #configuration{config_file_path = ConfigFilePath}};
create_configuration({error, Reason}) ->
    {stop, Reason}.
