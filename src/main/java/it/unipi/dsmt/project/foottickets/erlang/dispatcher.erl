-module(dispatcher).
-behaviour(gen_server).
%% Export API Functions
-export([start_link/0]).
%% Required gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    State = [],
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    Return.

handle_call(Request, From, State) ->
	io:format("We are in the handle_call~n"),
	case Request of
		{User_PID, test} ->
			io:format("I received a test~n"),
			Reply = Request,
    		NewState = State,
			Result = {reply, Reply, NewState};
			
		{User_PID, buy} ->
			io:format("Handle_call Buy~n"),
			Reply = Request,
    		NewState = State,
			Result = {reply, Reply, NewState};
			
		{User_PID, create} ->
			io:format("Handle_call Create~n"),
			gen_server:call(replica, {create}, 1),
			Reply = Request,
    		NewState = State,
			Result = {reply, Reply, NewState};	
			
					
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
	%io:format("Info: ~w  State: ~w~n", [Info, State]),
	case Info of 

		%%requests from java
		%%v1
		{Java_PID, create, Map} when is_atom(create) ->
			io:format("You are creating a map~n"),
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, create, Map}, 1),
			{noreply, State};
			
		{Java_PID, isAvailable, Seat} when is_atom(isAvailable) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, isAvailable, Seat}, 1),
			{noreply, State};
			
		{Java_PID, buy, Seat} when is_atom(buy) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, buy, Seat}, 1),
			{noreply, State};
			
		{Java_PID, view} when is_atom(view) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, view}, 1),
			{noreply, State};
		
		%%v2
		{Java_PID, create, NRows, NCols, Price, Map} when is_atom(create) ->
			io:format("You are creating a map~n"),
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, create, NRows, NCols, Price, Map}, 1),
			{noreply, State};

		{Java_PID, select, Seat} when is_atom(select) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, select, Seat}, 1),
			{noreply, State};

		{Java_PID, unselect, Seat} when is_atom(unselect) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, unselect, Seat}, 1),
			{noreply, State};

		{Java_PID, show, Hash} when is_atom(show) ->
			catch gen_server:call({replica,replica@localhost}, {self(), Java_PID, show, Hash}, 1),
			{noreply, State};

		%%Answers to java
		%%v1
		{Java_PID, created} when is_atom(created) ->
			io:format("Created Reply~n"),
			Java_PID ! {"Map created"},
			{noreply, State};
			
		{Java_PID, isAvailable} when is_atom(isAvailable) ->
			io:format("isAvailable Reply~n"),
			Java_PID ! {"The seat is available"},
			{noreply, State};
			
		{Java_PID, notAvailable} when is_atom(notAvailable) ->
			io:format("notAvailable Reply~n"),
			Java_PID ! {"The seat is not available"},
			{noreply, State};
			
		{Java_PID, bought} when is_atom(bought) ->
			io:format("bought Reply~n"),
			Java_PID ! {"The seat has been bought"},
			{noreply, State};
		
		{Java_PID, Map} when is_atom(bought) ->
			io:format("view Reply~n"),
			Java_PID ! {Map},
			{noreply, State};

		%%v2
		{Java_PID, created, Hash} when is_atom(created) ->
			io:format("Created Reply~n"),
			Java_PID ! {ok, Hash, "Map created"},
			{noreply, State};

		{Java_PID, alreadyExist} when is_atom(alreadyExist) ->
			io:format("alreadyExist Reply~n"),
			Java_PID ! {no_ok, "Map already exist"},
			{noreply, State};

		{Java_PID, selected, Map} when is_atom(selected) ->
			io:format("selected Reply~n"),
			Java_PID ! {ok, "The seat has been selected", Map},
			{noreply, State};

		{Java_PID, notSelected, Map} when is_atom(notSelected) ->
			io:format("notSelected Reply~n"),
			Java_PID ! {no_ok, "The seat has not been selected", Map},
			{noreply, State};
		
		{Java_PID, unselected, Map} when is_atom(selected) ->
			io:format("unselected Reply~n"),
			Java_PID ! {ok, "The seat has been unselected", Map},
			{noreply, State};
		
		{Java_PID, notUnselected, Map} when is_atom(notUnselected) ->
			io:format("notUnselected Reply~n"),
			Java_PID ! {no_ok, "The seat has not been unselected", Map},
			{noreply, State};

		{Java_PID, _} ->
			Java_PID ! {no_ok, "Error from request"};

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
