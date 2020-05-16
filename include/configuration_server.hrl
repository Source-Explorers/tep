%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2020 9:54 PM
%%%-------------------------------------------------------------------
%% @doc This macro defines the possible file names for the configuration files.
-define(FILE_NAMES, [
    "config.ini",
    "tep.ini"
]).

%% @doc This macro defines the directories in which the config file is searched.
-define(CONFIG_PATHS, [
    filename:absname(""),
    filename:basedir(user_config, "tep/"),
    "/etc/tep/"
]).
