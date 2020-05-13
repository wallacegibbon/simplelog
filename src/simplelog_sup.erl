%%--------------------------------------------------------------------
%% @doc simplelog top level supervisor.
%% @end
%%--------------------------------------------------------------------

-module(simplelog_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_list(Configs) ->
    [{simplelog, {simplelog, start_link, [Configs]}, permanent, 10000,
      worker, [simplelog]}].

init(Configs) ->
    {ok, {{one_for_one, 3, 3}, child_list(Configs)}}.


start_link(Configs) ->
    CommonTab = ets:new(simplelog, [set, public]),
    true = ets:insert(CommonTab, {top_state, #{}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE,
			  Configs#{commontab => CommonTab}).

