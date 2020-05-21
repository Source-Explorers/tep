%%%-------------------------------------------------------------------
%%% @doc
%%% Commonly used macros for filenames and the record for the configuration_server
%%% @end
%%%-------------------------------------------------------------------
-record(configuration, {config_file_path :: file:filename_all()}).

%% @doc This macro defines the accepted file names for the configuration files.
-define(FILE_NAMES, [
    "config.ini",
    "tep.ini"
]).

%% @doc This macro defines where tep will search for its config files.
-define(CONFIG_PATHS, [
    filename:absname(""),
    filename:basedir(user_config, "tep/"),
    "/etc/tep/"
]).
