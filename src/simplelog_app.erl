%%--------------------------------------------------------------------
%% @doc simplelog public API
%% @end
%%--------------------------------------------------------------------

-module(simplelog_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    simplelog_sup:start_link(#{}).

stop(_State) ->
    ok.

