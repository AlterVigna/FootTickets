-module(replica).
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
		%%v2

		{Disp_PID, Java_PID, create, NRows, NCols, Price, Map} when is_atom(create) ->
			AlreadyExist = ets:member(Table, Map),
			io:format("Creating a map in the replica node~w~n", [AlreadyExist]),
			if AlreadyExist == false ->
				Hash = 0,
				ets:insert(Table, [{map, Map}, {hash, Hash}, {nRows, NRows}, {nCols, NCols}, {price, Price}]),
				io:format("~w~n", [ets:lookup(Table, map)]),
				Result = {noreply, State},		
				Disp_PID ! {Java_PID, created, Hash};
				
			true ->
				Result = {noreply, State},
				Disp_PID ! {Java_PID, alreadyExist}
			end;

		{Disp_PID, Java_PID, select, Seat} when is_atom(select) ->
			io:format("Buying a seat in the replica node~n"),
			CurrMap = ets:lookup_element(Table, map, 2),
			%io:format("~w~n", [CurrMap]),
			Check = maps:find({Seat}, CurrMap),
			io:format("Checking if a seat is available in the replica node  ~w~n", [Check]),
			
			if  Check == {ok, {not_used}} ->
				Map = maps:update({Seat}, {used},CurrMap),
				Hash = ets:lookup_element(Table, hash, 2),
				ets:insert(Table, [{map, Map}, {hash, Hash + 1}]),
				Result = {noreply, State},
				Disp_PID ! {Java_PID, ets:lookup_element(Table, hash, 2), selected, Map};
				
			true ->
				Result = {noreply, State},	
				Disp_PID ! {Java_PID, ets:lookup_element(Table, hash, 2), notSelected, ets:lookup_element(Table, map, 2)}
								
			end;
	
		{Disp_PID, Java_PID, unselect, Seat} when is_atom(unselect) ->
			io:format("Buying a seat in the replica node~n"),
			Check = maps:find({Seat}, ets:lookup_element(Table, map, 2)),
			io:format("Checking if a seat is available in the replica node  ~w~n", [Check]),
			
			if  Check == {ok, {used}} ->
				Map = maps:update({Seat}, {not_used}, ets:lookup_element(Table, map, 2)),
				Hash = ets:lookup_element(Table, hash, 2),
				ets:insert(Table, [{map, Map}, {hash, Hash + 1}]),
				Result = {noreply, State},
				Disp_PID ! {Java_PID, ets:lookup_element(Table, hash, 2), unselected, Map};
				
			true ->
				Result = {noreply, State},	
				Disp_PID ! {Java_PID, ets:lookup_element(Table, hash, 2), notUnselected, ets:lookup_element(Table, map, 2)}
								
			end;


		{Disp_PID, Java_PID, show, Hash} when is_atom(show) ->
			
			io:format("View the map in the replica node~n"),
			NodeHash = ets:lookup_element(Table, hash, 2),
			if Hash == NodeHash ->
				Result = {noreply, State},
				Disp_PID ! {Java_PID, ok_hash};
			
			true ->
				Result = {noreply, State},
				Disp_PID ! {Java_PID, show, ets:lookup_element(Table, map, 2), Hash, ets:lookup_element(Table, nRows, 2), ets:lookup_element(Table, nCols, 2), ets:lookup_element(Table, price, 2)}

			end;
					
			
		
		
		{Disp_PID, Java_PID, _} ->
			Disp_PID ! {Java_PID, errorRequest},
			Result = {noreply, State};

		_->
			io:format("error call~n"),
			Result = {noreply, State}

		
	end,
    %io:format("handle_call: ~p~n", [Reply]),
    Result.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(Info, State) ->
	io:format("Info: ~w  State: ~w~n", [Info, State]),
	case Info of 
		{create} when is_atom(create) ->
			io:format("This is a test~n"),
			catch gen_server:call(dispatcher, Info, 1),
			io:format("This is a test 2~n"),
			{noreply, State};
			
			
		{Local_PID, {User_PID, test}} ->
			io:format("Test Reply~n"),
			User_PID ! {User_PID, test},
			{noreply, State};
			
		_->
			io:format("error info~n"),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
