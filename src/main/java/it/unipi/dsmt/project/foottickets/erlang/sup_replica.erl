-module(sup_replica).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 1},
    ChildSpecs = [
	#{id => replica,
	  start => {replica, start_link, []},
      restart => permanent,
      type => worker,
      module => [replica]
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
