%%%-------------------------------------------------------------------
%% @doc tep top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(tep_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(string() | no_path) ->
    {ok, pid()} |
    ignore |
    {error,
        {already_started, pid()} |
        {shutdown, term()} |
        term()}.
start_link(ConfigPath) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ConfigPath]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([ConfigPath]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},

    Configuration = #{
        id => configuration,
        start => {configuration_server, start_link, [ConfigPath]}
    },

    Storage = #{
        id => storage_sup,
        start => {storage_sup, start_link, []},
        shutdown => infinity,
        type => supervisor
    },
    {ok, {SupFlags, [Configuration, Storage]}}.

%% internal functions
