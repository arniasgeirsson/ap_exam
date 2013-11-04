%%%-------------------------------------------------------------------
%%% Student name: Arni Asgeirsson
%%% Student KU-id: lwf986
%%%-------------------------------------------------------------------

-module(at_server).

-behaviour(gen_server).

% Interface functions
-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).
% Extra interface functions
-export([get_pids/1]).
% gen_server callback functions
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-define(MIN_POOL,false).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% xCOM I always assume that AT is a valid at_server process id, this is never
%% checked and if called with invalid value may result in unexpected error, behaviour or 
%% and endless waiting for a never responding process.

%% TODO only one can be started at a time?
%% -> COM fix is to not give it a name? What is the wanted behaviour? {local, some_name}
%% xCOM I make no assumptions on the input State
start(State) ->
    %% TODO use start or start_link?
    %% TODO handle error cases here? ie the ignore and {error,erro} respons?
    gen_server:start(at_server, {server, State}, []).

%% Default timeout value is 5000 ms
%% call/2 is a synchronous call
stop(AT) ->
    gen_server:call(AT,stop_at_server).

doquery(AT, Fun) ->
    gen_server:call(AT,{doquery,Fun}).

% Returns a reference
begin_t(AT) ->
    gen_server:call(AT, begin_t).

query_t(AT, Ref, Fun) ->
    gen_server:call(AT, {doquery_t, {Ref, Fun}}).

%% Cast is the async requests
update_t(AT, Ref, Fun) ->
    gen_server:cast(AT, {update_t, {Ref, Fun}}).

commit_t(AT, Ref) ->
    gen_server:call(AT,{commit_t, Ref}).

%%% Extra API
%% COM the extra functionality and why
%% COM I assume that it works as intended
%% Returns {ok, ListOfPids}
get_pids(AT) ->
    gen_server:call(AT,get_pids).

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

%% rpc(Pid, Request) ->
%%     Pid ! {self(), Request},
%%     receive
%%         {Pid, Response} -> Response
%%     end.

%% reply(From,  Msg) ->
%%     From ! {self(), Msg}.

%% reply_ok(From) ->
%%     reply(From, ok).

%% reply_ok(From, Msg) ->
%%     reply(From, {ok, Msg}).

%% reply_error(From, Msg) ->
%%     reply(From, error).

%% reply_abort(From) ->
%%     reply(From, aborted).

%% %% asynchronous communication

%% info(Pid, Msg) ->
%%     Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Your implementation of the atomic transaction server.

%%%-------------------------------------------------------------------
%%% State functions
%%%-------------------------------------------------------------------

%%%----------------------------------
%% Module:init(Args) -> Result
%% ----Types:
%% Args = term()
%% Result = {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
%%     | {stop,Reason} | ignore
%% State = term()
%% Timeout = int()>=0 | infinity
%% Reason = term()
%%%----------------------------------

init({server, Args}) ->
    %% TODO Init pool size, or something similar? 
    {ok,{Args,[]}};
init({transaction, Args}) ->
    {ok,{Args,ready}}.

%%%----------------------------------
%% Module:handle_call(Request, From, State) -> Result
%% ----Types:
%% Request = term()
%% From = {pid(),Tag}
%% State = term()
%% Result = {reply,Reply,NewState} | {reply,Reply,NewState,Timeout}
%%     | {reply,Reply,NewState,hibernate}
%%     | {noreply,NewState} | {noreply,NewState,Timeout}
%%     | {noreply,NewState,hibernate}
%%     | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%% Reply = term()
%% NewState = term()
%% Timeout = int()>=0 | infinity
%% Reason = term()
%%%----------------------------------

%% COM/TODO what should happen if calling update on a ref that has been aborted
%% COM/TODO what should happen if calling update on a ref that does not exist
%% COM/TODO when do we clean up the aborted processes?
%% -> After each commit? Otherwise they will stack only upwards
%% COM when doing (Ref,Pid,Status) ->
%% Only the master (at_server) knows the state of the transactions
%% They don't know it themselves.
%% -> Why not let them know? What are the advanteges and disadvanteges of both ways?
%% This way we do not need to talk to them when aborting them after a commit
%% or if the user requests it
%% But they cannot abort them selves if they find some unexpected error or terminates
%% for some unexpected reason.
%% What about perfomance? This way we need to do several O(N) key searches,
%% -> becomes a problem with a large N
%% The other way we need to do some sync communication, which could be a problem
%% with a worker than is running some very heavy computations in a update call
%% and the master becomes unresponsive as the change has to be a sync call,
%% (otherwise a commit call can occour later and skip ahead in the mailbox,
%% as the mail box order is not guarenteed when moving out of local space communication
%% (find reference)).

%% COM One could make it so that workers do not get killed after a commit, but merely
%% set to waiting/empty/init and are treated as non-existing when trying to perfom
%% operations on them, but when begin_t is called the master server looks up in its pool
%% and only spawns a new worker if no waiting worker is to be found.
%% Only that this makes the reference not unique as they may be reused after each commit
%% Unless they are updated by the master when changing their state! As their
%% pid is what is used to communicate with the workers.
%% -> I will do this! (don't kill and create new ref each time) 

%% COM from ass. text "Note, that your implementation must stop all unused 
%% processes and should not keep transaction-helper processes much longer 
%% than necessary, especially at commits. Also, processes waiting for answers 
%% from aborted transactions must be answered with aborted as quickly as possible. 
%% Explain in your report how you solve this problem."
%% -> Does this mean that I should kill the processes? But how should I then
%% let the user know that the process has been aborted? And a busy server will
%% have a lot of overhead spawning and killing processes, of course a very non-busy
%% server will not, and might benefit from it.
%% But my method also allows for a-process-waiting-for-answer-from-aborted-transaction
%% to recieve the aborted messages very very quickly, (O(n) fast..)


%% COM Do I maintain this property?
%% "Your API must be robust against erroneous updating and querying functions.
%% All erroneous behaviour outside a transaction must return error,
%% and all erroneous behaviour inside a transaction must return aborted."

%% COM the "Transactions" variable is an extra parameter that can be used beside the state
%% The master uses it to keep information of its transations/pool
%% The transactions uses it to contain some state (aborted or not)
%% This is referred to as Transaction when only the master receives this message
%% Status or the actual status if the message is intended to a transaction
%% And satalite if both can receive the message

%% COM I assume that no one will try and guess the pids of the transactions and send
%% them random messages or try and manipulate with them being going past the api functions

%% COM the status of the transactions to don't entirely relfect the status
%% maintained by the master, as the only thing that is needed is if they
%% are aborted or not.

handle_call(stop_at_server, _, {State,Transactions}) ->
    lists:foreach(fun({_,P,_}) -> {ok,_} = gen_server:call(P,stop_at_trans) end, Transactions),
    {stop,normal,{ok,State},[]}; %% No reason to carry the state anymore
handle_call(stop_at_trans, _, {State, _}) ->
    {stop,normal,{ok,State},[]}; %% No reason to carry the state anymore
handle_call({doquery,_}, _, {State, aborted}) ->
    {reply,error,{State,aborted}};
handle_call({doquery,Fun}, _, {State,Satalite}) ->
    Reply = try Fun(State) of
		Result -> {ok,Result}
	    catch
		%% TODO report the error to the caller?
		_:_ -> error
	    end,
    {reply,Reply,{State,Satalite}};
handle_call({doquery_t, {Ref, Fun}}, _, {State,Transactions}) ->
    {Reply,NewTransactions} =
	case lists:keyfind(Ref,1,Transactions) of
	    {Ref,TrPid,ready} ->
		case gen_server:call(TrPid, {doquery, Fun}) of
		    error ->
			case ?MIN_POOL of
			    false ->
				{aborted, lists:keyreplace(Ref,1,Transactions,{Ref,TrPid,aborted})};
			    true ->
				{ok,_} = gen_server:call(TrPid,stop_at_trans),
				{aborted, lists:keydelete(Ref,1,Transactions)}
			end;
		    Result -> {Result,Transactions}
		end;
	    _ ->
		{aborted,Transactions}
	end,
    {reply, Reply, {State, NewTransactions}};
handle_call(begin_t, _, {State,Transactions}) ->
    %% COM why use key pair instead of just using the returned pid as the 'ref' value
    %% or something else? This allows us to maintain a unique reference to the user but internally
    %% maintain a small pool of processes instead of killing and spawning new each item
    URef = make_ref(),
    NewTransactions =
	case lists:keyfind(waiting,3,Transactions) of
	    false ->
		{ok, TrPid} = gen_server:start(at_server, {transaction, State}, []),
		[{URef,TrPid,ready}|Transactions];
	    {Ref,TrPid,waiting} ->
		%% Make sure to update its state to be of ours
		ok = gen_server:call(TrPid,{initiate,fun(_) -> {State,ready} end}),
		lists:keyreplace(Ref,1,Transactions,{URef,TrPid,ready})
	end,
    {reply,{ok,URef},{State,NewTransactions}};
handle_call({commit_t, Ref}, _, {State, Transactions}) ->
    {Reply, NewState, NewTransactions} =
	case lists:keyfind(Ref,1,Transactions) of
	    {Ref,TrPid,ready} ->
		case gen_server:call(TrPid, {doquery,fun(I) -> I end}) of
		    error ->
			{aborted,State,lists:keyreplace(Ref,1,Transactions,{Ref,TrPid,aborted})};
		    {ok, NS} ->
			%% Abort all transactions now,
			%% ei set their state to waiting
			%% Note that their state does not get 'cleaned up' this is done in begin_t
			case ?MIN_POOL of
			    false ->
				NT = lists:map(fun({R,P,_}) -> {R,P,waiting} end, Transactions),
				{ok,NS,NT};
			    true ->
				lists:foreach(fun({_,P,_}) -> {ok,_} = gen_server:call(P,stop_at_trans) end, Transactions),
				{ok,NS,[]}
			end
		end;
	    _ ->
		{aborted,State,Transactions}
	end,
    {reply,Reply,{NewState,NewTransactions}};
handle_call(get_pids, _, {State, Transactions}) ->
    AllPids = [self()|lists:flatmap(fun({_,P,_}) -> [P] end, Transactions)],
    {reply, {ok, AllPids}, {State, Transactions}};
handle_call({initiate,Fun}, _, State) ->
    InitState = Fun(State),
    {reply, ok, InitState};
handle_call(Msg,_,State) ->
    {reply,{unrecognized_message,Msg},State}.

%%%----------------------------------
%% Module:handle_cast(Request, State) -> Result
%% ----Types:
%% Request = term()
%% State = term()
%% Result = {noreply,NewState} | {noreply,NewState,Timeout}
%%     | {noreply,NewState,hibernate}
%%     | {stop,Reason,NewState}
%% NewState = term()
%% Timeout = int()>=0 | infinity
%% Reason = term()
%%%----------------------------------

handle_cast({update_t, {Ref, Fun}}, {State, Transactions}) ->
    case lists:keyfind(Ref,1,Transactions) of
	{Ref,TrPid,ready} ->
	    gen_server:cast(TrPid,{update, Fun});
	_ ->
	    do_nothing
    end,
    {noreply, {State, Transactions}};
handle_cast({update, _}, {State, aborted}) ->
    %% Do nothing
    {noreply,{State,aborted}};
handle_cast({update, Fun}, {State, Status}) ->
    NewState = try Fun(State) of
		   Result -> {Result,Status}
	       catch
		   %% TODO report the error to the caller?
		   _:_ -> {State,aborted}
	       end,
    {noreply,NewState};
handle_cast(_,State) ->
    {noreply,State}.

%%%----------------------------------
%% Module:handle_info(Info, State) -> Result
%% ----Types:
%% Info = timeout | term()
%% State = term()
%% Result = {noreply,NewState} | {noreply,NewState,Timeout}
%%     | {noreply,NewState,hibernate}
%%     | {stop,Reason,NewState}
%% NewState = term()
%% Timeout = int()>=0 | infinity
%% Reason = normal | term()
%%%----------------------------------

handle_info(_, State) ->
    {noreply,State}.

%%%----------------------------------
%% Module:terminate(Reason, State)
%% ----Types:
%% Reason = normal | shutdown | {shutdown,term()} | term()
%% State = term()
%%%----------------------------------

terminate(normal, _) ->
    ok;
terminate(Error, State) ->
    io:format(
      "#####Error: at_server/transaction with state: ~p~n"
      ++" - Terminating due to some unexpected error: ~p!~n",[State, Error]),
    ok.

%% COM the code_change/3 is not used and therefore not implemented,
%% although present due to the expected callback exports

%%%----------------------------------
%% Module:code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%% ----Types:
%% OldVsn = Vsn | {down, Vsn}
%% Vsn = term()
%% State = NewState = term()
%% Extra = term()
%% Reason = term()
%%%----------------------------------

code_change(_, State, _) ->
    {ok,State}.
