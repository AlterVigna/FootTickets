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
		{Disp_PID, Java_PID, create, Map} when is_atom(create) ->
			io:format("Creating a map in the replica node~n"),
			%if ets:match(Table, Map) == False ->
			
				ets:insert(Table, {Map}),
				io:format("~w~n", ets:lookup(Table, Map)),
				Result = {noreply, State},		
				Disp_PID ! {Java_PID, created};
				
			%true ->
				%Disp_PID ! {Java_PID, alreadyExist}
			%end;
			
		{Disp_PID, Java_PID, isAvailable, Seat} when is_atom(isAvailable) ->
			Check = maps:find({Seat}, ets:first(Table)),
			io:format("Checking if a seat is available in the replica node  ~w~n", [Check]),
			
			if  Check == {ok, {not_used}} ->
				Result = {noreply, State},	
				Disp_PID ! {Java_PID, isAvailable};
						
			true ->
				Result = {noreply, State},	
				Disp_PID ! {Java_PID, notAvailable}
				
			end;
			
		{Disp_PID, Java_PID, buy, Seat} when is_atom(buy) ->
			io:format("Buying a seat in the replica node~n"),
			Check = maps:find({Seat}, ets:first(Table)),
			io:format("Checking if a seat is available in the replica node  ~w~n", [Check]),
			
			if  Check == {ok, {not_used}} ->
				Map = maps:update({Seat}, {used}, ets:first(Table)),
				%io:format("~n~w~n", [Map]),
				ets:insert(Table, {Map}),
				Result = {noreply, State},
				Disp_PID ! {Java_PID, bought};
				
			true ->
				Result = {noreply, State},	
				Disp_PID ! {Java_PID, notAvailable}
								
			end;
	
		{Disp_PID, Java_PID, view} when is_atom(view) ->
			io:format("View the map in the replica node~n"),
			Result = {noreply, State},		
			Disp_PID ! {Java_PID, ets:first(Table)};
			
		_->
			io:format("error call~n"),
			Result = {noreply, State}
	end,
    Result = {noreply, State},
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
