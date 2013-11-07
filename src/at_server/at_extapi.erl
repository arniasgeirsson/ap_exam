%%%-------------------------------------------------------------------
%%% Student name: Arni Asgeirsson
%%% Student KU-id: lwf986
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

%% COM are all these functions blocking or non-blocking?

abort(AT, Ref) ->
    %% TODO how can I be sure that this ensures an abortion?
    at_server:query_t(AT,Ref,fun(_) -> error(force_abort) end).

tryUpdate(AT, Fun) ->
    {ok,Ref} = at_server:begin_t(AT),
    %% COM by querying the transaction first, we can be sure the function returns an error
    %% to stick to the api, and if we do that then we do not need to recalculate that again
    %% although adding some overhead of transporting the data back and forth
    case at_server:query_t(AT,Ref,Fun) of
	{ok,State} ->
	    %% COM No reason to evaluate the result again
	    ok = at_server:update_t(AT,Ref,fun(_) -> State end),
	    %% COM by now we either get a succesfull commit or got aborted
	    %% because someone else made a commit before us.
	    at_server:commit_t(AT,Ref);
	aborted ->
	    error
    end.


ensureUpdate(AT, Fun) ->
    {ok,Ref} = at_server:begin_t(AT),
    case at_server:query_t(AT,Ref,Fun) of
	{ok,State} ->
	    %% COM ensureLoop begins a new transaction, making R obsolete
	    %% but it will be cleaned up with the next commit.
	    ensureLoop(AT,fun(_) -> State end);
	aborted ->
	    %% COM there is a slight change that someone made a commit before
	    %% we were able to call query (not after we begun querying)
	    %% and after we begun the transaction. This is accepted.
	    error
    end.

ensureLoop(AT,Fun) ->
    {ok,R} = at_server:begin_t(AT),
    ok = at_server:update_t(AT,R,Fun),
    case at_server:commit_t(AT,R) of
	ok ->
	    ok;
	aborted ->
	    %% Ugh, we try again
	    ensureLoop(AT,Fun)
    end.

%% COM note that this does not 100% that the first is the one to
%% get through, (though it does locally) as the message queue is not guerenteed.
%% It is assumed that Val_list is indeed a list
choiceUpdate(AT, Fun, Val_list) ->
    AllTrans = lists:map(fun(E) -> {at_server:begin_t(AT),E} end, Val_list),
    Me = self(),
    URef = make_ref(),
    lists:foreach(fun({{ok,R},E}) ->
			  ok = at_server:update_t(
				 AT,
				 R,
				 fun(State) ->
					 
					 try Fun(State,E) of
					     Res ->
						 info(Me,{URef,R,done}),
						 Res
					 catch
					     _:_ ->
						 info(Me,{URef,R,error}),
						 %% Remember to fail so its
						 %% state is updated properly
						 Fun(State,E)
					 end
				 end)
		  end,
		  AllTrans),
    choiceLoop(AT,AllTrans,URef).

%% Used by choiceUpdate
%% COM is moved into its own function to remove garbage messages if any,
%% as a precaution.
choiceLoop(_,[],_) ->
    error;
choiceLoop(AT,AllTrans,URef) ->
    %% COM note that the messages are not guarenteed to arrive in the same order
    %% they are sent, therefore it could be that R is not the one who finished first
    %% but then again if R would sent the commit message himself, we still are not
    %% sure the someone won't skip in front of him.
    %% COM if we let the transaction to themself commit, we still need to let them
    %% send us a message to protect against the case of where all functions fail.
    receive
	{URef,R,done} ->
	    at_server:commit_t(AT,R);
	{URef,R,error} ->
	    RestTrans = lists:keydelete({ok,R},1,AllTrans),
	    choiceLoop(AT,RestTrans,URef);
	_ -> 
	    choiceLoop(AT,AllTrans,URef)
    end.
    

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.
