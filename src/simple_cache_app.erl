%%%-------------------------------------------------------------------
%% @doc simple_cache public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    simple_cache_store:init(),
    case simple_cache_sup:start_link() of
        {ok, Pid} ->
            simple_cache_event_logger:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%% internal functions
