%%%-------------------------------------------------------------------
%%% @doc
%%% This header file defines a few macros, that TEP needs to find and
%%% access a configuration file.
%%% @end
%%%-------------------------------------------------------------------
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
