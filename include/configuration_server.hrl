%%%-------------------------------------------------------------------
%%% @doc
%%% Commonly used macros for filenames and the record for the configuration_server
%%% @end
%%%-------------------------------------------------------------------
-record(configuration, {config_file_path :: file:filename_all()}).

%% @doc This macro defines the accepted file names for the configuration files.
-define(TEP_DEFAULT_CONFIG_FILE_NAMES, [
    "config.ini",
    "tep.ini"
]).

%% @doc This macro defines where tep will search for its config files.
-define(TEP_DEFAULT_CONFIG_LOCATIONS, [
    filename:absname(""),
    filename:basedir(user_config, "tep/"),
    "/etc/tep/"
]).
