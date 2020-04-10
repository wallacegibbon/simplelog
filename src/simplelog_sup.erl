%%--------------------------------------------------------------------
%% @doc simplelog top level supervisor.
%% @end
%%--------------------------------------------------------------------

-module(simplelog_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_specs(Configs) ->
    [#{id => simplelog, start => {simplelog, start_link, [Configs]},
       restart => permanent, shutdown => 10000, type => worker,
       modules => [simplelog]}].

init(Configs) ->
    SupFlags = #{strategy => one_for_all, intensity => 3, period => 3},
    {ok, {SupFlags, child_specs(Configs)}}.

start_link(Configs) ->
    CommonTab = ets:new(simplelog, [set, public]),
    true = ets:insert(CommonTab, {top_state, #{}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE,
			  Configs#{commontab => CommonTab}).

