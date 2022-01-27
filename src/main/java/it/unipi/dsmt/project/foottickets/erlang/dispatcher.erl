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
    Return = {ok, []},
    io:format("init:~n"),
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
		
		%%v2
		{Java_PID, create, NRows, NCols, Price, Map} when is_atom(create) ->
			io:format("You are creating a map~n"),
			catch gen_server:call({replica, replica@localhost}, {self(), Java_PID, create, NRows, NCols, Price, Map}, 1),
			{noreply, State};

		{Java_PID, select, Seat} when is_atom(select) ->
			catch gen_server:call({replica, replica@localhost}, {self(), Java_PID, select, Seat}, 1),
			{noreply, State};

		{Java_PID, unselect, Seat} when is_atom(unselect) ->
			catch gen_server:call({replica, replica@localhost}, {self(), Java_PID, unselect, Seat}, 1),
			{noreply, State};

		{Java_PID, show, Hash} when is_atom(show) ->
			catch gen_server:call({replica, replica@localhost}, {self(), Java_PID, show, Hash}, 1),
			{noreply, State};

		%%Answers to java
		%%v2
		{Java_PID, created, Hash} when is_atom(created) ->
			io:format("Created Reply~n"),
			Java_PID ! {ok, Hash, "Map created"},
			{noreply, State};

		{Java_PID, alreadyExist} when is_atom(alreadyExist) ->
			io:format("alreadyExist Reply~n"),
			Java_PID ! {no_ok, "Map already exist"},
			{noreply, State};

		{Java_PID, show, Map, Hash, NRows, NCols, Price} when is_atom(show) ->
			io:format("showMap Reply~n"),
			Java_PID ! {ok, Hash, NRows, NCols, Price, Map},
			{noreply, State};
	
		{Java_PID, ok_hash} when is_atom(ok_hash) ->
			io:format("okhash Reply~n"),
			Java_PID ! {ok_hash},
			{noreply, State};

		{Java_PID, Hash, selected, Map} when is_atom(selected) ->
			io:format("selected Reply~n"),
			Java_PID ! {ok, Hash, "The seat has been selected", Map},
			{noreply, State};

		{Java_PID, Hash, notSelected, Map} when is_atom(notSelected) ->
			io:format("notSelected Reply~n"),
			Java_PID ! {no_ok, Hash, "The seat has not been selected", Map},
			{noreply, State};
		
		{Java_PID, Hash, unselected, Map} when is_atom(selected) ->
			io:format("unselected Reply~n"),
			Java_PID ! {ok, Hash, "The seat has been unselected", Map},
			{noreply, State};
		
		{Java_PID, Hash, notUnselected, Map} when is_atom(notUnselected) ->
			io:format("notUnselected Reply~n"),
			Java_PID ! {no_ok, Hash, "The seat has not been unselected", Map},
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
