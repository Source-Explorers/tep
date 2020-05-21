%%%-------------------------------------------------------------------
%%% @doc
%%% Configuration_server finds, reads and stores the configuration for TEP. It presents an interface
%%% for other processes to access configuration variables.
%%% @end
%%%-------------------------------------------------------------------
-module(configuration_server).

-behaviour(gen_server).

-include("configuration_server.hrl").

%%% Export these internal functions if EUnit is testing this module
-ifdef(EUNIT).

-export([
    search_for_config_file_path/2
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

%% @doc Returns a list with the default names for configuration files
-spec get_default_filenames() ->
    {ok, [file:filename_all()]}.
get_default_filenames() ->
    gen_server:call(configuration_server, get_default_filenames).

%% @doc Returns the currently used configuration file
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
    ConfigFilePathSearchResult = search_for_config_file_path(
        ?TEP_DEFAULT_CONFIG_LOCATIONS,
        ?TEP_DEFAULT_CONFIG_FILE_NAMES
    ),
    StoreResult = create_configuration(ConfigFilePathSearchResult),
    return_from_init(StoreResult).

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
    {reply, {ok, ?TEP_DEFAULT_CONFIG_LOCATIONS}, State};
handle_call(get_default_filenames, _From, State = #configuration{}) ->
    {reply, {ok, ?TEP_DEFAULT_CONFIG_FILE_NAMES}, State};
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
%% @private
%% @doc Searches for a configuration file in the default directories and uses the first found file.
-spec search_for_config_file_path(
    Directories ::
        [file:filename_all()],
    FileNames :: [file:filename_all()]
) ->
    {ok,
        FileName ::
            file:filename_all()} |
    {error, Reason :: term()}.
search_for_config_file_path(Directories, FileNames) ->
    ConfigFileCandidates = list_config_file_path_candidates(Directories, FileNames),
    RegularFileCandidates = filter_for_regular_files(ConfigFileCandidates),
    select_candidate(RegularFileCandidates).

%% @private
%% @doc Lists all possible paths in which we want to look for configuration files.
-spec list_config_file_path_candidates(
    Directories :: [file:filename_all()],
    FileNames :: [file:filename_all()]
) ->
    [file:filename_all()].
list_config_file_path_candidates(Directories, FileNames) ->
    [filename:absname_join(Path, File) || Path <- Directories, File <- FileNames].

%% @private
%% @doc Filters non-existent configuration files from the list of candidates.
-spec filter_for_regular_files(CandidateNames :: [file:filename_all()]) ->
    [file:filename_all()].
filter_for_regular_files(CandidateNames) ->
    [CandidateFile || CandidateFile <- CandidateNames, filelib:is_regular(CandidateFile)].

%% @private
%% @doc Chooses the config file from a list of candidates if any are found; otherwise errors.
-spec select_candidate([file:filename_all()]) ->
    {ok, FileName :: file:filename_all()} |
    {error, Reason :: term()}.
select_candidate([ConfigFile | _]) -> {ok, ConfigFile};
select_candidate([]) -> {error, {no_file, "No config file found"}}.

%% @private
%% @doc Creates a configuration state from a path to a config file.
-spec create_configuration(
    {ok, ConfigFile :: file:filename_all()} | {error, Reason :: term()}
) ->
    {ok, #configuration{}} | {error, Reason :: term()}.
create_configuration({ok, ConfigFilePath}) ->
    {ok, #configuration{config_file_path = ConfigFilePath}};
create_configuration({error, Reason}) ->
    {error, Reason}.

%% @private
%% @doc Generate return from init depending on results of preparatory functions.
-spec return_from_init(
    {ok, State :: #configuration{}} | {error, Reason :: term()}
) ->
    {ok, State :: #configuration{}} | {stop, Reason :: term()}.
return_from_init({ok, State}) ->
    {ok, State};
return_from_init({error, Reason}) ->
    {stop, Reason}.
