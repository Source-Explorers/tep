%%%-------------------------------------------------------------------
%% @doc tep top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(tep_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(string() | no_custom_config_path) ->
    {ok, pid()} |
    ignore |
    {error,
        {already_started, pid()} |
        {shutdown, term()} |
        term()}.
start_link(ConfigPath) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ConfigPath]).

-spec init([string()] | [no_custom_config_path]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([ConfigPath]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},

    Configuration = #{
        id => configuration,
        start => {configuration_server, start_link, [ConfigPath]}
    },

    Sensor = #{
        id => sensor,
        start =>
            {sensor_server, start_link, [#{date => fun () -> calendar:local_time() end}]}
    },

    Storage = #{
        id => storage_sup,
        start => {storage_sup, start_link, []},
        shutdown => infinity,
        type => supervisor
    },
    {ok, {SupFlags, [Configuration, Storage, Sensor]}}.

%% internal functions
