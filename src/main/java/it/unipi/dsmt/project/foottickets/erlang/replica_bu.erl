-module(replica_bu).
-behaviour(gen_server).
%% Export API Functions
-export([start_link/0]).
%% Required gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tbl = none}).


start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    Table = ets:new(test, []),
    Return = {ok, #state{tbl = Table}},
    io:format("init: ~n"),
    Return.

handle_call(Request, From, State = #state{tbl = Table}) ->
	%io:format("We are in the handle_call, ~w~n", [Request]),
	case Request of		
		{RepPID, getTableInfo} ->
            io:format("~w ~n", [ets:member(Table, map)]),
            MapExist = ets:member(Table, map),
            
            if MapExist == false ->
                RepPID ! {tableDontExist};              
            true ->
                RepPID ! {ets:lookup_element(Table, map, 2), ets:lookup_element(Table, hash, 2), ets:lookup_element(Table, nRows, 2), ets:lookup_element(Table, nCols, 2), ets:lookup_element(Table, price, 2)}
            end,
            Result = {noreply, State};
        {createTable, Map, Hash, NRows, NCols, Price} ->
            ets:insert(Table, [{map, Map}, {hash, Hash}, {nRows, NRows}, {nCols, NCols}, {price, Price}]),
            Result = {noreply, State};
        _ ->
            Result = {noreply, State},
            io:format("error")
    end,    
    Result.

handle_cast(Request, State = #state{tbl = Table}) ->
    case Request of		
		{RepPID, getTableInfo} ->
            io:format("~w ~n", [ets:member(Table, map)]),
            MapExist = ets:member(Table, map),
            
            if MapExist == false ->
                RepPID ! {tableDontExist};              
            true ->
                RepPID ! {ets:lookup_element(Table, map, 2), ets:lookup_element(Table, hash, 2), ets:lookup_element(Table, nRows, 2), ets:lookup_element(Table, nCols, 2), ets:lookup_element(Table, price, 2)}
            end,
            Result = {noreply, State};
        {createTable, Map, Hash, NRows, NCols, Price} ->
            ets:insert(Table, [{map, Map}, {hash, Hash}, {nRows, NRows}, {nCols, NCols}, {price, Price}]),
            Result = {noreply, State};
        _ ->
            Result = {noreply, State},
            io:format("error")
    end,    
    Result.

handle_info(Info, State) ->
	io:format("Info: ~w  State: ~w~n", [Info, State]),
    {noreply, State}.
	

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
