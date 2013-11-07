%%%-------------------------------------------------------------------
%%% Student name: Arni Asgeirsson
%%% Student KU-id: lwf986
%%%-------------------------------------------------------------------

-module(test_at_server).

-export([runTests/0]).

-define(SLEEP_TIME, 40).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

%% Run all tests
runTests() ->
    io:format("------------------------------------------------------------------~n"),
    io:format("------------ Running tests for the at_server module --------------~n"),
    io:format("------------------------------------------------------------------~n~n"),
    io:format("-------------------- Running Unit tests --------------------------~n"),
    io:format("Testing start/1:............."),
    io:format("~p~n",[testStart()]),
    io:format("Testing begin_t/1:..........."),
    io:format("~p~n",[testBegin()]),
    io:format("Testing stop/1:.............."),
    io:format("~p~n",[testStop()]),
    io:format("Testing doquery/2:..........."),
    io:format("~p~n",[testDoquery()]),
    io:format("Testing query_t/3:..........."),
    io:format("~p~n",[testQuery_t()]),
    io:format("Testing update_t/3:.........."),
    io:format("~p~n",[testUpdate_t()]),
    io:format("Testing commit_t/2:.........."),
    io:format("~p~n",[testCommit_t()]),
    io:format("Testing abort/2:............."),
    io:format("~p~n",[testAbort()]),
    io:format("Testing tryUpdate/2:........."),
    io:format("~p~n",[testTryUpdate()]),
    io:format("Testing ensureUpdate/2:......"),
    io:format("~p~n",[testEnsureUpdate()]),
    io:format("Testing choiceUpdate/3:......"),
    io:format("~p~n",[testChoiceUpdate()]).

%% TODO dont sleep after update?
%% TODO test timeouts of the different parts of the server (interface functions)
%% to make sure that it does not crash
%% TODO cannot a transaction commit it self inside an update function?
%% TODO sent unrecognized message
%% TODO let someone else send message to a transaction

%%%-------------------------------------------------------------------
%%% Test Functions
%%%-------------------------------------------------------------------

%% ----------------------------------
%% ------------ ATS API -------------

%% Test start/1
testStart() ->
    %% Init test data
    StateA = [asd,"d",233],

    %% 1. Test that we can start a server with some state

    {ok, Pid1} = at_server:start([]),
    timer:sleep(?SLEEP_TIME),
    Test1 = assertEquals(true,isProcessAlive(Pid1)),
    
    %% 2. Test that we can start multiply servers

    {ok, Pid2} = at_server:start(Pid1),
    timer:sleep(?SLEEP_TIME),
    Test21 = assertEquals(true,isProcessAlive(Pid2)),
    
    {ok, Pid3} = at_server:start(StateA),
    timer:sleep(?SLEEP_TIME),
    Test22 = assertEquals(true,isProcessAlive(Pid3)),
    Test2 = areTrue([Test21,Test22]),

    %% Clean up
    {ok,[]} = at_server:stop(Pid1),
    {ok,Pid1} = at_server:stop(Pid2),
    {ok,StateA} = at_server:stop(Pid3),

    areTrue([Test1,Test2]).

%% Test begin_t/1
%% COM/TODO how do I test that the unique references are indeed unique?
%% -> by looking at make_ref/0 and knowing that is returns a unique reference
%% -> http://www.erlang.org/doc/man/erlang.html#make_ref-0
testBegin() ->
    %% Init test data
    State = some_state,
    {ok, Pid1} = at_server:start(State),
    {ok, Pid2} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that only one process exist on start up

    {ok, AllPids} = at_server:get_pids(Pid1),
    Test1 = assertEquals(1,length(AllPids)),
    
    %% 2. Test that we can start transactions and these spawn a the correct amount
    %% of transactions/processes

    begin_transactions(Pid1,1),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids1} = at_server:get_pids(Pid1),
    Test21 = assertEquals(2, length(AllPids1)),

    begin_transactions(Pid1,5),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids3} = at_server:get_pids(Pid1),
    Test22 = assertEquals(7, length(AllPids3)),
    
    begin_transactions(Pid1,28),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids4} = at_server:get_pids(Pid1),
    Test23 = assertEquals(35, length(AllPids4)),
    
    Test24 = assertEquals(true, areProcessesAlive(AllPids4)),
    Test2 = areTrue([Test21, Test22, Test23, Test24]),

    %% 3. Test that we cannot create a transaction in A and use it in B
    {ok,R} = at_server:begin_t(Pid1),
    Test3 = assertEquals(true, isAborted(Pid2,R)),

    %% 4. Test that the state of a transaction is the same as the ATS
    Test4 = assertEquals(at_server:doquery(Pid1,fun identity/1),
			 at_server:query_t(Pid1,R,fun identity/1)),
    
    %% Clean up
    {ok,State} = at_server:stop(Pid1),
    {ok,State} = at_server:stop(Pid2),

    areTrue([Test1, Test2, Test3, Test4]).

%% Test stop/1
testStop() ->
    %% Init test data
    State = some_state,
    {ok, Pid1} = at_server:start(State),
    {ok, Pid2} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that an at_server dies after stop/1 has been called

    Test11 = assertEquals(true, isProcessAlive(Pid1)),
    Test12 = assertEquals({ok,State}, at_server:stop(Pid1)),
    timer:sleep(?SLEEP_TIME),
    Test13 = assertEquals(true, isProcessDead(Pid1)),
    Test1 = areTrue([Test11, Test12, Test13]),
    
    %% 2. Test that all initiated transactions are also stopped with the at_server

    begin_transactions(Pid2,4),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids} = at_server:get_pids(Pid2),
    Test21 = assertEquals(true, areProcessesAlive(AllPids)),
    {ok, State} = at_server:stop(Pid2),
    timer:sleep(?SLEEP_TIME),
    Test22 = assertEquals(true, areProcessesDead(AllPids)),
    Test2 = areTrue([Test21, Test22]),
    
    %% Clean up
    
    areTrue([Test1, Test2]).


%% Test doquery/2
testDoquery() ->
    %% Init test data
    State = "I am not an A",
    {ok, Pid1} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),
    
    %% 1. Test doquery returns the state with an identity function
    Test1 = assertEquals({ok,State},
			 at_server:doquery(Pid1,fun identity/1)),
    
    %% 2. Test that it returns what is returned by the given function
    Test21 = assertEquals({ok,mapMult2(State)},
			  at_server:doquery(Pid1,fun mapMult2/1)),
    Test22 = assertEquals({ok,mapToA(State)},
			  at_server:doquery(Pid1,fun mapToA/1)),
    Test2 = areTrue([Test21, Test22]),
    
    %% 3. Show that the doquery doesn't update the state data
    Test3 = assertEquals({ok,State},
			 at_server:doquery(Pid1,fun identity/1)),

    %% 4. Test what happens if the function causes some error
    Test41 = assertEquals(error,
			  at_server:doquery(Pid1,fun onlyEmpty/1)),
    Test42 = assertEquals(true, isProcessAlive(Pid1)),
    Test43 = assertEquals({ok,State},
			  at_server:doquery(Pid1,fun identity/1)), 
    Test4 = areTrue([Test41, Test42, Test43]),
    
    %% Clean up
    {ok,State} = at_server:stop(Pid1),
    
    areTrue([Test1, Test2, Test3, Test4]).

%% Test query_t/3
testQuery_t() ->
    %% Init test values
    State = [1,2,3,4,5,6],
    {ok,Pid1} = at_server:start(State),
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that an unaltered trans state returns the initial state when used with identity
    Test1 = assertEquals({ok, State},
			 at_server:query_t(Pid1,R1,fun identity/1)),

    %% 2. Test that it returns the same as when run on the state here
    Test21 = assertEquals({ok,mapMult2(State)},
			  at_server:query_t(Pid1,R1,fun mapMult2/1)),
    Test22 = assertEquals({ok,mapToA(State)},
			  at_server:query_t(Pid1,R1,fun mapToA/1)),
    Test2 = areTrue([Test21, Test22]),

    %% 3. Show that query_t doesnt update its state
    Test3 = assertEquals({ok, State},
			 at_server:query_t(Pid1,R1,fun identity/1)),

    %% 4. Test what happens if the function causes some error
    Test41 = assertEquals(aborted,
			  at_server:query_t(Pid1,R1,fun onlyEmpty/1)),
    Test42 = assertEquals(true, isProcessAlive(Pid1)),
    Test4 = areTrue([Test41, Test42]),
    
    %% 5. Show that aborted is also returned when trying to query it again (even with a valid function)
    Test5 = assertEquals(aborted,
			 at_server:query_t(Pid1,R1,fun identity/1)),

    %% 6. Test that even though R1 is aborted R2 is still good
    Test6 = assertEquals({ok,State},
			 at_server:query_t(Pid1,R2,fun identity/1)),

    %% 7. Test that a wrong ref_id is considered to be an aborted transaction
    WrongRef = make_ref(),
    Test71 = assertEquals(aborted,
			  at_server:query_t(Pid1,WrongRef,fun identity/1)),
    Test72 = assertEquals(aborted,
			  at_server:query_t(Pid1,also_wrong,fun identity/1)),
    Test7 = areTrue([Test71, Test72]),

    %% Clean up
    {ok, State} = at_server:stop(Pid1),

    areTrue([Test1, Test2, Test3, Test4, Test5, Test6, Test7]).

%% Test update_t/3
testUpdate_t() ->
    %% Init test values
    State = [1,2,3,4,5,6],
    {ok,Pid1} = at_server:start(State),
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    ok = at_server:update_t(Pid1,R2,fun removeEven/1),
    {ok,R3} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that if we update it then it contains the new data
    Test11 = assertEquals({ok,State},
			  at_server:query_t(Pid1,R1,fun identity/1)),
    ok = at_server:update_t(Pid1,R1,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test12 = assertEquals({ok,removeEven(State)},
			  at_server:query_t(Pid1,R1,fun identity/1)),
    Test1 = areTrue([Test11, Test12]),
    
    %% 2. Test what happends if the update function fails
    %% I.e. Show that it is aborted
    ok = at_server:update_t(Pid1,R1,fun onlyEmpty/1),
    timer:sleep(?SLEEP_TIME),
    Test2 = assertEquals(true, isAborted(Pid1,R1)),

    %% 3. Test calling update on a aborted transaction
    ok = at_server:update_t(Pid1,R1,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test3 = assertEquals(true, isAborted(Pid1,R1)),

    %% 4. Show that even though it is aborted R2 & R3 still maintain their state and are fully functional
    Test41 = assertEquals({ok,removeEven(State)},
			  at_server:query_t(Pid1,R2,fun identity/1)),
    Test42 = assertEquals({ok,State},
			  at_server:query_t(Pid1,R3,fun identity/1)),

    ok = at_server:update_t(Pid1,R3,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test43 = assertEquals({ok,removeEven(State)},
			  at_server:query_t(Pid1,R3,fun identity/1)),
    Test4 = areTrue([Test41, Test42, Test43]),

    %% 5. Test what happens with a wrong ref_id
    {ok, AllPids} = at_server:get_pids(Pid1),
    Test51 = assertEquals(true, areProcessesAlive(AllPids)),
    ok = at_server:update_t(Pid1,wrong_ref,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test52 = assertEquals(true, areProcessesAlive(AllPids)),
    Test53 = assertEquals({ok,removeEven(State)},
			  at_server:query_t(Pid1,R3,fun identity/1)),
    Test5 = areTrue([Test51, Test52, Test53]),

    %% Clean up
    {ok,State} = at_server:stop(Pid1),
    
    areTrue([Test1, Test2, Test3, Test4, Test5]).

%% Test commit_t/2
testCommit_t() ->
    %% Init test values
    StateA = [1,2,3,4,5,6,7,8,9,10],
    StateB = removeEven(StateA),
    {ok,Pid1} = at_server:start(StateA),
    {ok,R1} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that after a commit without first doing a update the state is still the same
    %% And that it is still treated as a commit, ie the process is aborted
    Test11 = assertEquals({ok,StateA},
			  at_server:doquery(Pid1, fun identity/1)),
    Test12 = assertEquals(ok, at_server:commit_t(Pid1,R1)),
    Test13 = assertEquals({ok,StateA},
			  at_server:doquery(Pid1, fun identity/1)),
    Test1 = areTrue([Test11, Test12, Test13]),
    
    %% 2. Test that after a commit the transactions are all aborted
    %% -test what happends if we try and update the same ref again
    %% -test what happends if we try and commit the same ref again
    Test21 = assertEquals(true, isAborted(Pid1,R1)),
    Test22 = assertEquals(aborted, at_server:commit_t(Pid1,R1)),
    Test23 = assertEquals(true, isAborted(Pid1,R1)),
    Test2 = areTrue([Test21, Test22, Test23]),

    %% 3. Test that the state changes to the correct value after a commit and update
    {ok,R2} = at_server:begin_t(Pid1),
    Test31 = assertEquals({ok,StateA},
			  at_server:doquery(Pid1, fun identity/1)),
    ok = at_server:update_t(Pid1,R2, fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    ok = at_server:commit_t(Pid1,R2),
    Test32 = assertEquals({ok,StateB},
			  at_server:doquery(Pid1, fun identity/1)),
    Test3 = areTrue([Test31, Test32]),

    %% 4. Test if we try to commit after the update function have failed
    {ok,R3} = at_server:begin_t(Pid1),
    ok = at_server:update_t(Pid1,R3, fun onlyEmpty/1),
    Test4 = assertEquals(aborted, at_server:commit_t(Pid1,R3)),

    %% 5. Test that we can have several different transactions going at one time
    %% And that all are aborted when one is committed
    {ok,R4} = at_server:begin_t(Pid1),
    {ok,R5} = at_server:begin_t(Pid1),
    {ok,R6} = at_server:begin_t(Pid1),
    {ok,R7} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),
    
    Mult2 = fun(NS) -> lists:map(fun(N) -> N*2 end,NS) end,
    Mult4 = fun(NS) -> lists:map(fun(N) -> N*4 end,NS) end,
    Mult8 = fun(NS) -> lists:map(fun(N) -> N*8 end,NS) end,
    
    ok = at_server:update_t(Pid1,R4, Mult2),
    ok = at_server:update_t(Pid1,R5, Mult4),
    ok = at_server:update_t(Pid1,R6, fun onlyEmpty/1),
    ok = at_server:update_t(Pid1,R7, Mult8),
    timer:sleep(?SLEEP_TIME),
    
    Test51 = assertEquals({ok,Mult2(StateB)},
			  at_server:query_t(Pid1,R4,fun identity/1)),
    Test52 = assertEquals({ok,Mult4(StateB)},
			  at_server:query_t(Pid1,R5,fun identity/1)),
    Test53 = assertEquals(aborted,
			  at_server:query_t(Pid1,R6,fun identity/1)),
    Test54 = assertEquals({ok,Mult8(StateB)},
			  at_server:query_t(Pid1,R7,fun identity/1)),

    ok = at_server:commit_t(Pid1,R5),
    StateC = Mult4(StateB),
    Test55 = assertEquals({ok,StateC},
			  at_server:doquery(Pid1, fun identity/1)),
    Test56 = assertEquals(true, isAborted(Pid1,R4)),
    Test57 = assertEquals(true, isAborted(Pid1,R5)),
    Test58 = assertEquals(true, isAborted(Pid1,R6)),
    Test59 = assertEquals(true, isAborted(Pid1,R7)),
    Test5 = areTrue([Test51,Test52,Test53,Test54,Test55,Test56,
		     Test57,Test58,Test59]), 

    %% 6. Test with wrong ref
    Test6 = assertEquals(aborted, at_server:commit_t(Pid1,wrong_ref)),

    %% Clean up
    {ok,StateC} = at_server:stop(Pid1),

    areTrue([Test1, Test2, Test3, Test4, Test5, Test6]).

%% ----------------------------------
%% ------------ EXT API -------------

%% Tests abort/2
testAbort() ->
    %% Init test data
    State = abcdef,
    {ok,Pid1} = at_server:start(State),
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    {ok,R3} = at_server:begin_t(Pid1),
    {ok,R4} = at_server:begin_t(Pid1),
    {ok,R5} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that the transaction is aborted
    Test11 = assertEquals(aborted, at_extapi:abort(Pid1,R1)),
    Test12 = assertEquals(true, isAborted(Pid1,R1)),
    Test1 = areTrue([Test11, Test12]),

    %% 2. Test what happens if calling aborted again
    Test2 = assertEquals(aborted, at_extapi:abort(Pid1,R1)),

    %% 3. Test that several can be aborted
    Test31 = assertEquals(aborted, at_extapi:abort(Pid1,R2)),
    Test32 = assertEquals(aborted, at_extapi:abort(Pid1,R3)),
    Test3 = areTrue([Test31, Test32]),

    %% 4. Test that no one else is affacted when one is being aborted
    Test41 = assertEquals({ok,State}, at_server:query_t(Pid1,R4,fun identity/1)),
    ok = at_server:update_t(Pid1,R5,fun atom_to_list/1),
    timer:sleep(?SLEEP_TIME),
    Test42 = assertEquals({ok,atom_to_list(State)}, at_server:query_t(Pid1,R5,fun identity/1)),
    Test4 = areTrue([Test41, Test42]),

    %% 5. Test what happens with a unknown ref id
    WrongRef = make_ref(),
    Test5 = assertEquals(aborted, at_extapi:abort(Pid1,WrongRef)),

    %% Clean up
    {ok,State} = at_server:stop(Pid1),

    areTrue([Test1, Test2, Test3, Test4, Test5]).

%% Tests tryUpdate/2
testTryUpdate() ->
    %% Init test data
    StateA = [1,2,3,4,5,6],
    StateB = removeEven(StateA),
    {ok,Pid1} = at_server:start(StateA),
    timer:sleep(?SLEEP_TIME),
    
    %% 1. Test that if no one else is doing a transaction we get our update through
    Test11 = assertEquals(ok, at_extapi:tryUpdate(Pid1,fun identity/1)),
    Test12 = assertEquals({ok,StateA}, at_server:doquery(Pid1, fun identity/1)),
    Test13 = assertEquals(ok, at_extapi:tryUpdate(Pid1,fun removeEven/1)),
    Test14 = assertEquals({ok,StateB}, at_server:doquery(Pid1, fun identity/1)),
    Test1 = areTrue([Test11, Test12, Test13, Test14]),

    %% 2. Test that if the function fails, no update happens and we get error returned
    Test21 = assertEquals(error, at_extapi:tryUpdate(Pid1,fun onlyEmpty/1)),
    Test22 = assertEquals({ok,StateB}, at_server:doquery(Pid1, fun identity/1)),
    Test2 = areTrue([Test21, Test22]),
    
    %% 3. Test that if others is doing a transaction they get aborted
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    {ok,R3} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),
    ok = at_server:update_t(Pid1,R2,fun onlyEmpty/1),
    ok = at_server:update_t(Pid1,R3,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    
    Test31 = assertEquals(ok, at_extapi:tryUpdate(Pid1,fun identity/1)),
    Test32 = assertEquals(true, isAborted(Pid1,R1)),
    Test33 = assertEquals(true, isAborted(Pid1,R2)),
    Test34 = assertEquals(true, isAborted(Pid1,R3)),
    Test3 = areTrue([Test31, Test32, Test33, Test34]),

    %% 4. Test that if someone commits while we are trying to update we get aborted
    %% -> Really hard to do due to the implementation of tryUpdate/2
    {ok,R} = at_server:begin_t(Pid1),
    ok = at_server:update_t(Pid1,R,fun(_) ->
					   T = expensive1(500),
					   at_server:commit_t(Pid1,R),
					   T end),
    Test4 = assertNotEquals(aborted,
			    at_extapi:tryUpdate(Pid1,fun(_) -> expensive1(3500) end)),

    %% Clean up
    {ok,3500} = at_server:stop(Pid1),

    areTrue([Test1, Test2, Test3,Test4]).

%% Tests ensureUpdate/2
testEnsureUpdate() ->
    %% Init test data
    StateA = [1,2,3,4,5,6],
    StateB = removeEven(StateA),
    {ok,Pid} = at_server:start(StateA),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that if we are the only one here we get our update through
    Test11 = assertEquals(ok, at_extapi:ensureUpdate(Pid,fun identity/1)),
    Test12 = assertEquals({ok,StateA}, at_server:doquery(Pid, fun identity/1)),
    Test13 = assertEquals(ok, at_extapi:ensureUpdate(Pid,fun removeEven/1)),
    Test14 = assertEquals({ok,StateB}, at_server:doquery(Pid, fun identity/1)),
    Test1 = areTrue([Test11, Test12, Test13, Test14]),

    %% 2. Test that if the function fails we get error and nothing is updated
    Test21 = assertEquals(error, at_extapi:ensureUpdate(Pid,fun onlyEmpty/1)),
    Test22 = assertEquals({ok,StateB}, at_server:doquery(Pid,fun identity/1)),
    Test2 = areTrue([Test21, Test22]),

    %% 3. Test that if someone commits while we are trying to update we still get our
    %% commit through on the original state and the other is rolled backed
    %% -- HOW? TODO/COM
    {ok,R} = at_server:begin_t(Pid),
    ok = at_server:update_t(Pid,R,fun(_) ->
					   expensive1(500),
					   at_server:commit_t(Pid,R) end),
    Test3 = assertEquals(ok,
			 at_extapi:ensureUpdate(Pid,fun(_) -> expensive1(3500) end)),

    %% Clean up
    {ok,3500} = at_server:stop(Pid),

    areTrue([Test1, Test2,Test3]).

%% Tests choiceUpdate/3
testChoiceUpdate() ->
    %% Init test data
    Val_listA = [1],
    Val_listB = [a,2,c],
    %% TODO timeout error happens if 500 is set to 5000+
    Val_listC = [500,1],
    Val_listD = [a,b,c],

    Add = fun(State,E) -> lists:map(fun(N) -> N+E end,State) end,
    StateA = [1,2,3,4,5],
    StateB = Add(StateA,lists:nth(1,Val_listA)),
    StateC = Add(StateB,lists:nth(2,Val_listB)),
    StateD = lists:nth(2,Val_listC),

    {ok,Pid} = at_server:start(StateA),
    timer:sleep(?SLEEP_TIME),

    %% 1. Test that if only one then that gets through
    Test11 = assertEquals(ok, at_extapi:choiceUpdate(Pid,Add,Val_listA)),
    Test12 = assertEquals({ok,StateB}, at_server:doquery(Pid,fun identity/1)),
    Test1 = areTrue([Test11, Test12]),

    %% 2. Test that if all fail except one, then that gets through
    Test21 = assertEquals(ok, at_extapi:choiceUpdate(Pid,Add,Val_listB)),
    Test22 = assertEquals({ok,StateC}, at_server:doquery(Pid,fun identity/1)),
    Test2 = areTrue([Test21, Test22]),
    

    %% 3. Test that a shorter function will be the one to come through rather than a long function
    %% Although this cannot be guerenteed!
    Test31 = assertEquals(ok, at_extapi:choiceUpdate(Pid, fun expensive/2, Val_listC)),
    Test32 = assertEquals({ok,StateD}, at_server:doquery(Pid,fun identity/1)),
    Test3 = areTrue([Test31, Test32]),

    %% 4. Test that if someoneelse commits before any of us, we get aborted when trying to commit
    {ok,R} = at_server:begin_t(Pid),
    Test41 = assertEquals(aborted,
			  at_extapi:choiceUpdate(Pid,fun(_,_) -> 
							     at_server:commit_t(Pid,R)
						     end,Val_listA)),
    Test42 = assertEquals({ok,StateD}, at_server:doquery(Pid, fun identity/1)),
    Test4 = areTrue([Test41,Test42]),
    
    %% 5. Test that if all fail then error is returned
    Test5 = assertEquals(error, at_extapi:choiceUpdate(Pid,Add,Val_listD)),

    %% 6. Test that if the list is empty
    Test6 = assertEquals(error, at_extapi:choiceUpdate(Pid,Add,[])),

    %% Clean up
    {ok,StateD} = at_server:stop(Pid),

    areTrue([Test1, Test2, Test3, Test4, Test5, Test6]).

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

areTrue([]) ->
    false;
areTrue(List) ->
    lists:foldl(fun(A,B) -> A andalso B end,true,List).

assertEquals(A,B) ->
    A == B.

assertNotEquals(A,B) ->
    A /= B.

isAborted(Pid,R) ->
    A1 = aborted == at_server:query_t(Pid,R, fun identity/1),
    A2 = aborted == at_server:commit_t(Pid,R),
    A1 andalso A2.

% Returns true if the given Pid is a running process
% otherwise false.
isProcessAlive(Pid) ->
    case process_info(Pid) of
	undefined ->
	    false;
	_ -> true
    end.

isProcessDead(Pid) ->
    not(isProcessAlive(Pid)).

areProcessesAlive(Pids) ->
    lists:foldl(fun(P,B) -> isProcessAlive(P) andalso B end,true,Pids).

areProcessesDead(Pids) ->
    lists:foldl(fun(P,B) -> (isProcessDead(P)) andalso B end,true,Pids).

begin_transactions(A,N) ->
    case N > 0 of
	true ->
	    {ok, R} = at_server:begin_t(A),
	    [R|begin_transactions(A,N-1)];
	false -> []
    end.

%%%-------------------------------------------------------------------
%%% Update Functions
%%%-------------------------------------------------------------------

expensive(_,Time) ->
    receive
    after
	Time -> Time
    end.

expensive1(Time) ->
    expensive(Time,Time).

identity(X) ->
    X.

onlyEmpty([]) ->
    [].

mapToA(_) ->
    "A".

mapMult2(Ns) ->
    lists:map(fun(X) -> X*2 end,Ns).

removeEven(X) ->
    lists:filter(fun(N) -> N rem 2 /= 0 end, X).
