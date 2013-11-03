-module(test_at_server).

-export([runTests/0]).

-define(SLEEP_TIME, 40).

%% Run all tests
runTests() ->
    io:format("Test start: ~p~n",[testStart()]),
    io:format("Test begin: ~p~n",[testBegin()]),
    io:format("Test stop:  ~p~n",[testStop()]),
    io:format("Test doquery: ~p~n",[testDoquery()]),
    io:format("Test query_t: ~p~n",[testQuery_t()]),
    io:format("Test update_t: ~p~n",[testUpdate_t()]),
    io:format("Test commit_t: ~p~n",[testCommit_t()]).

%% Test start/1
testStart() ->
    %% TODO can start/1 go wrong in any way?

    %% Test that we can start a server with some state

    {ok, Pid1} = at_server:start([]),
    timer:sleep(?SLEEP_TIME),
    Test1 = isProcessAlive(Pid1),
    
    %% Test that we can start multiply servers with 

    {ok, Pid2} = at_server:start(Pid1),
    timer:sleep(?SLEEP_TIME),
    Test2 = isProcessAlive(Pid2),
    
    {ok, Pid3} = at_server:start([asd,"d", 233]),
    timer:sleep(?SLEEP_TIME),
    Test3 = isProcessAlive(Pid3),

    %% Clean up
    {ok,[]} = at_server:stop(Pid1),
    {ok,Pid1} = at_server:stop(Pid2),
    {ok,[asd,"d",233]} = at_server:stop(Pid3),

    Test1 andalso Test2 andalso Test3.

%% Test begin_t/1
%% COM/TODO how do I test that the unique references are indeed unique?
%% -> by looking at make_ref/0 and knowing that is returns a unique reference
%% -> http://www.erlang.org/doc/man/erlang.html#make_ref-0
testBegin() ->
    %% TODO test that they also contains the same state as their parent

    %% Init test data
    State = some_state,
    {ok, Pid1} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),

    %% Test that only one process exist on start up

    {ok, AllPids} = at_server:get_pids(Pid1),
    Test11 = 1 == length(AllPids),
    
    %% Test that we can start transactions and these spawn a the correct amount
    %% of transactions/processes

    begin_trans(Pid1,1),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids1} = at_server:get_pids(Pid1),
    Test12 = 2 == length(AllPids1),

    begin_trans(Pid1,5),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids3} = at_server:get_pids(Pid1),
    Test13 = 7 == length(AllPids3),
    
    begin_trans(Pid1,28),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids4} = at_server:get_pids(Pid1),
    Test14 = 35 == length(AllPids4),
    
    Test15 = areProcessAlive(AllPids4),
    
    %% Clean up
    {ok,State} = at_server:stop(Pid1),

    Test1 = Test11 andalso Test12 andalso Test13 andalso Test14 andalso Test15,
    Test1.

%% Test stop/1
%% Assumes that begin_t/1 works
testStop() ->
    %% Init test data
    State = some_state,
    {ok, Pid1} = at_server:start(State),
    {ok, Pid2} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),

    %% Test that an at_server dies after stop/1 has been called

    Test11 = isProcessAlive(Pid1),
    Test12 = {ok,State} == at_server:stop(Pid1),
    timer:sleep(?SLEEP_TIME),
    Test13 = isProcessDead(Pid1),
    Test1 = Test11 andalso Test12 andalso Test13,
    
    %% Test that all initiated transactions are also stopped with the at_server

    %% NOTE we don't care about the unique ref ie the return value
    {ok, _} = at_server:begin_t(Pid2),
    {ok, _} = at_server:begin_t(Pid2),
    {ok, _} = at_server:begin_t(Pid2),
    {ok, _} = at_server:begin_t(Pid2),
    timer:sleep(?SLEEP_TIME),
    {ok, AllPids} = at_server:get_pids(Pid2),
    Test21 = areProcessAlive(AllPids),
    {ok, State} = at_server:stop(Pid2),
    timer:sleep(?SLEEP_TIME),
    Test22 = areProcessDead(AllPids),
    Test2 = Test21 andalso Test22,
    
    %% Clean up
    
    Test1 andalso Test2.


%% Test doquery/2
testDoquery() ->
    %% Init test data
    State = "I am not an A",
    {ok, Pid1} = at_server:start(State),
    timer:sleep(?SLEEP_TIME),
    
    %% Test doquery returns the state with an identity function
    Test1 = {ok,State} == at_server:doquery(Pid1,fun identity/1),
    
    %% Test that it returns what is returned by the function
    Test21 = {ok,mapMult2(State)} == at_server:doquery(Pid1,fun mapMult2/1),
    Test22 = {ok,mapToA(State)} == at_server:doquery(Pid1,fun mapToA/1),
    Test2 = Test21 andalso Test22,
    
    %% Show that the doquery doesn't update the state data
    Test3 = {ok,State} == at_server:doquery(Pid1,fun identity/1),

    %% Test what happens if the function causes some error
    Test41 = error == at_server:doquery(Pid1,fun onlyEmpty/1),
    Test42 = isProcessAlive(Pid1),
    Test4 = Test41 andalso Test42,
    
    %% Clean up
    {ok,State} = at_server:stop(Pid1),

    Test1 andalso Test2 andalso Test3 andalso Test4.

%% TODO Place them inside the test functions them self: Name = fun .. ?
identity(X) ->
    X.

onlyEmpty([]) ->
    [].

mapToA(_) ->
    "A".

mapMult2(Ns) ->
    lists:map(fun(X) -> X*2 end,Ns).

%% Test query_t/3
testQuery_t() ->
    %% Init test values
    State = [1,2,3,4,5,6],
    {ok,Pid1} = at_server:start(State),
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% Test a transaction has the same data as its parent
%%    {ok, R} = at_server:begin_t.. Already tested above?

    %% Test that an unaltered trans state returns the initial state when used with identity
    Test1 = {ok, State} == at_server:query_t(Pid1,R1,fun identity/1),

    %% Test that it returns the same as when run on the state here
    Test21 = {ok,mapMult2(State)} == at_server:query_t(Pid1,R1,fun mapMult2/1),
    Test22 = {ok,mapToA(State)} == at_server:query_t(Pid1,R1,fun mapToA/1),
    Test2 = Test21 andalso Test22,

    %% Show that query_t doesnt update its state
    Test3 = {ok, State} == at_server:query_t(Pid1,R1,fun identity/1),

    %% Test what happens if the function causes some error
    Test41 = aborted == at_server:query_t(Pid1,R1,fun onlyEmpty/1),
    Test42 = isProcessAlive(Pid1),
    Test4 = Test41 andalso Test42,
 
    %% Show that the transaction is indeed now arborted (dead)
    %% How do I do that? By doing the test below?

    %% Show that aborted is also returned when trying to query it again (even with a valid function)
    Test5 = aborted == at_server:query_t(Pid1,R1,fun identity/1),

    %% Test that even though R1 is aborted R2 is still good
    Test6 = {ok,State} == at_server:query_t(Pid1,R2,fun identity/1),

    %% Test what happens with a wrong ref_id
    WrongRef = make_ref(),
    Test71 = wrong_ref == at_server:query_t(Pid1,WrongRef,fun identity/1),
    Test72 = wrong_ref == at_server:query_t(Pid1,something_else,fun identity/1),
    Test7 = Test71 andalso Test72,

    %% Clean up
    {ok, State} = at_server:stop(Pid1),

    Test1 andalso Test2 andalso Test3 andalso Test4 andalso Test5 andalso Test6
	andalso Test7.

removeEven(X) ->
    lists:filter(fun(N) -> N rem 2 /= 0 end, X).

%% Test update_t/3
testUpdate_t() ->
    %% Init test values
    State = [1,2,3,4,5,6],
    {ok,Pid1} = at_server:start(State),
    {ok,R1} = at_server:begin_t(Pid1),
    {ok,R2} = at_server:begin_t(Pid1),
    {ok,R3} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% Test that if we update it, and then it contains the new data
    Test11 = {ok,State} == at_server:query_t(Pid1,R1,fun identity/1),
    ok = at_server:update_t(Pid1,R1,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test12 = {ok,removeEven(State)} == at_server:query_t(Pid1,R1,fun identity/1),
    Test1 = Test11 andalso Test12,
    
    %% To be used below
    ok = at_server:update_t(Pid1,R2,fun removeEven/1),

    %% Test what happends if the update function fails
    %% I.e. Show that it is aborted
    ok = at_server:update_t(Pid1,R1,fun onlyEmpty/1),
    timer:sleep(?SLEEP_TIME),
    Test2 = isAborted(Pid1,R1),

    %% Test calling update on a aborted transaction
    ok = at_server:update_t(Pid1,R1,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test3 = isAborted(Pid1,R1),

    %% Show that even though it is aborted R2 still maintains its state and is fully functional
    Test41 = {ok,removeEven(State)} == at_server:query_t(Pid1,R2,fun identity/1),
    Test42 = {ok,State} == at_server:query_t(Pid1,R3,fun identity/1),

    ok = at_server:update_t(Pid1,R3,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test43 = {ok,removeEven(State)} == at_server:query_t(Pid1,R3,fun identity/1),
    Test4 = Test41 andalso Test42 andalso Test43,

    %% Test what happens with a wrong ref_id
    {ok, AllPids} = at_server:get_pids(Pid1),
    Test51 = areProcessAlive(AllPids),
    ok = at_server:update_t(Pid1,something,fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    Test52 = areProcessAlive(AllPids),
    Test53 = {ok,removeEven(State)} == at_server:query_t(Pid1,R3,fun identity/1),
    Test5 = Test51 andalso Test52 andalso Test53,

    %% Clean up
    {ok,State} = at_server:stop(Pid1),
    
    Test1 andalso Test2 andalso Test3 andalso Test4 andalso Test5.

%% Test commit_t/2
testCommit_t() ->
    %% Init test values
    StateA = [1,2,3,4,5,6,7,8,9,10],
    StateB = removeEven(StateA),
    {ok,Pid1} = at_server:start(StateA),
    {ok,R1} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),

    %% Test that after a commit without first doing a update the state is still the same
    %% And that it is still treated as a commit, ie the process is aborted
    Test11 = {ok,StateA} == at_server:doquery(Pid1, fun identity/1),
    ok = at_server:commit_t(Pid1,R1),
    Test12 = {ok,StateA} == at_server:doquery(Pid1, fun identity/1),
    Test1 = Test11 andalso Test12,
    
    %% Test that after a commit the transactions are all aborted
    %% -test what happends if we try and update the same ref again
    %% -test what happends if we try and commit the same ref again
    Test2 = isWrongRef(Pid1,R1),

    %% Test that the state changes to the correct value after a commit and update
    {ok,R2} = at_server:begin_t(Pid1),
    Test31 = {ok,StateA} == at_server:doquery(Pid1, fun identity/1),
    ok = at_server:update_t(Pid1,R2, fun removeEven/1),
    timer:sleep(?SLEEP_TIME),
    ok = at_server:commit_t(Pid1,R2),
    Test32 = {ok,StateB} == at_server:doquery(Pid1, fun identity/1),
    Test3 = Test31 andalso Test32,

    %% Test if we try to commit after the update function have failed
    {ok,R3} = at_server:begin_t(Pid1),
    ok = at_server:update_t(Pid1,R3, fun onlyEmpty/1),
    Test4 = aborted == at_server:commit_t(Pid1,R3),

    %% Test that we can have several different transactions going at one time
    {ok,R4} = at_server:begin_t(Pid1),
    {ok,R5} = at_server:begin_t(Pid1),
    {ok,R6} = at_server:begin_t(Pid1),
    {ok,R7} = at_server:begin_t(Pid1),
    timer:sleep(?SLEEP_TIME),
    
    %% Used later
    {ok,AllPids} = at_server:get_pids(Pid1),

    Mult2 = fun(NS) -> lists:map(fun(N) -> N*2 end,NS) end,
    Mult4 = fun(NS) -> lists:map(fun(N) -> N*4 end,NS) end,
    Mult8 = fun(NS) -> lists:map(fun(N) -> N*8 end,NS) end,
    
    ok = at_server:update_t(Pid1,R4, Mult2),
    ok = at_server:update_t(Pid1,R5, Mult4),
    ok = at_server:update_t(Pid1,R6, fun onlyEmpty/1),
    ok = at_server:update_t(Pid1,R7, Mult8),
    timer:sleep(?SLEEP_TIME),
    
    Test51 = {ok,Mult2(StateB)} == at_server:query_t(Pid1,R4,fun identity/1),
    Test52 = {ok,Mult4(StateB)} == at_server:query_t(Pid1,R5,fun identity/1),
    Test53 = aborted == at_server:query_t(Pid1,R6,fun identity/1),
    Test54 = {ok,Mult8(StateB)} == at_server:query_t(Pid1,R7,fun identity/1),
    Test5 = Test51 andalso Test52 andalso Test53 andalso Test54,

    %% Test that all are aborted when one is commited
    ok = at_server:commit_t(Pid1,R5),
    StateC = Mult4(StateB),
    Test61 = {ok,StateC} == at_server:doquery(Pid1, fun identity/1),
    Test62 = isWrongRef(Pid1,R4),
    Test63 = isWrongRef(Pid1,R5),
    Test64 = isWrongRef(Pid1,R6),
    Test65 = isWrongRef(Pid1,R7),
    Test6 = Test61 andalso Test62 andalso Test63 andalso Test64 andalso Test65,

    %% Show that all the proccess remain alive after having been aborted
    Test7 = areProcessAlive(AllPids),

    %% Clean up
    {ok,StateC} = at_server:stop(Pid1),

    Test1 andalso Test2 andalso Test3 andalso Test4 andalso Test5 andalso Test6 andalso Test7.

%% Helpers

isAborted(Pid,R) ->
    A1 = aborted == at_server:query_t(Pid,R, fun identity/1),
    A2 = aborted == at_server:commit_t(Pid,R),
    A1 andalso A2.

isWrongRef(Pid,R) ->
    A1 = wrong_ref == at_server:query_t(Pid,R, fun identity/1),
    A2 = wrong_ref == at_server:commit_t(Pid,R),
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

areProcessAlive(Pids) ->
    lists:foldl(fun(P,B) -> isProcessAlive(P) andalso B end,true,Pids).

areProcessDead(Pids) ->
    lists:foldl(fun(P,B) -> (isProcessDead(P)) andalso B end,true,Pids).

begin_trans(A,N) ->
    case N > 0 of
	true ->
	    {ok, R} = at_server:begin_t(A),
	    [R|begin_trans(A,N-1)];
	false -> []
    end.
