%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name:
%%% Student KU-id:
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

%% COM are all these functions blocking or non-blocking?

%% COM The ass. text does not say anything about the return value
%% So I let it be the return valid of query_t.
abort(AT, Ref) ->
    %% COM/TODO how can I be sure that this ensures an abortion?
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

%% COM As while we try to update the state of AT someone else could do it,
%% meaning that either we have to do a rollback of that commit, lying to the other
%% client, thinking that he got a commit through, even though we are just going to
%% neglect that, as we MUST ensure our update of the state data, unless the Fun
%% raises some error, and no rollback will occour.
%% Otherwise we could check that if the state of AT has changed before we commit,
%% but after we have updated, but this could happen forever, leading to that we
%% starve ourselves. And what if the third time around threw an error, and it was
%% just a matter of chance (underlying process scheduling, cpu-bussiness, randomness, and-so-on)
%% if we could have gotten the second update through. Blaaw, lots of trouble, I will stick
%% to the rollback, even though this could mean that we rollback possible many commits.
ensureUpdate(AT, Fun) ->
    %% ? Just keep trying until we succeed?
    %% How can I ensure that it will be on the current state?
    %% First doquery the current state out?
    {ok,R} = at_server:begin_t(AT),
    %% COM why do I query first? What if the function takes a long time?
    %% -> will not change the outcome of the function, but will increase running time
    %% and might result in more commits being rolled back.
    %% COM if you somehow could stop and start the server again and be sure that it
    %% received the exact same Pid again, then you could do that, or no, not even
    %% someone could still sneak in a commit, if our function has a very long running time.
    %% COM As we are not sure that we get our commit through the first time (or the next, or ever)
    %% we need to keep trying until we get it through.

    %% TODO might aswell take the answer from query_t ????? Instead of
    %% calling update all the time, since the answer would be exactly the same
    case at_server:query_t(AT,R,Fun) of
	{ok,State} ->
	    %% COM ensureLoop begins a new transaction, making R obsolete
	    %% but it will be cleaned up with the next commit.
	    %% This will speed up the update function, so we hopefully doesn't
	    %% have to try to many times before succeding.
	    %% COM and this method ensures that it is the initial state that is used.
	    ensureLoop(AT,fun(_) -> State end);
	aborted ->
	    %% COM there is a slight change that someone made a commit before
	    %% we were able to call query (not after we begun querying)
	    %% and after we begun the transaction.
	    %% TODO how to protect us from this? By letting us try out the function
	    %% instead of utilizing the transaction, but I don't think that is 
	    %% a good idea.
	    error
    end.

ensureLoop(AT,Fun) ->
    {ok,R} = at_server:begin_t(AT),
    ok = at_server:update_t(AT,R,Fun),
    case at_server:commit_t(AT,R) of
	ok ->
	    ok;
	aborted ->
	    %% COM ugh, we try again
	    ensureLoop(AT,Fun)
    end.

%% COM What if all fails?
%% -> then the function will hang in the loop/0 function as it expects someone
%% to succeed
%% As if an update fails then it will never send the message to us
%% Meaning that when we receive a message we can be certain that the helper succeeded.
%% COM Or someone else commits first?
%% -> then wrong_ref is returned when trying to commit
%% COM note that this does not 100% that the first is the one to
%% get through, (though it does locally) as the message queue is not guerenteed.
choiceUpdate(AT, Fun, Val_list) ->
    %% COM We must initiate all the transactions before updating and commiting
    %% otherwise we risk that an early one commits before another has even
    %% been started and possible commiting again.
    %% And zip in the mean time
    AllTrans = lists:map(fun(E) -> {at_server:begin_t(AT),E} end, Val_list),
    Me = self(),
    lists:foreach(fun({{ok,R},E}) ->
			  ok = at_server:update_t(AT,R,
						  fun(State) -> Res = Fun(State,E),
								info(Me,{R,done}),
								Res
						  end)
		  end,
		  AllTrans),
    {R,done} = choiceLoop(),
    at_server:commit_t(AT,R).

%% Used by choiceUpdate
%% COM is moved into its own function to remove garbage messages if any,
%% as a precaution.
choiceLoop() ->
    receive
	{R,done} ->
	    {R,done};
	E -> io:format("~p received expected message: ~p~n"
		       ++"Throwing it away as garbage.~n",[self(),E]),
	     choiceLoop()
    end.
    

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.
