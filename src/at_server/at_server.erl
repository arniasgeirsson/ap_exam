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

-define(MIN_POOL,true).
%% Default timeout value is 5000 ms for call/3
-define(TIME_OUT, 5000).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% xCOM I always assume that AT is a valid at_server process id, this is never
%% checked and if called with invalid value may result in unexpected error, behaviour or 
%% and endless waiting for a never responding process.

%% TODO make sure that the transactions only listens to messages given by their master?

%% TODO only one can be started at a time?
%% -> COM fix is to not give it a name? What is the wanted behaviour? {local, some_name}
%% xCOM I make no assumptions on the input State
start(State) ->
    %% TODO use start or start_link?
    %% TODO handle error cases here? ie the ignore and {error,erro} respons?
    gen_server:start(at_server, {server, State}, []).

%% call/2 is a synchronous call
stop(AT) ->
    tryCall(gen_server:call(AT,stop_at_server,?TIME_OUT)).

doquery(AT, Fun) ->
    tryCall(gen_server:call(AT,{doquery,Fun},?TIME_OUT)).

% Returns a reference
begin_t(AT) ->
    tryCall(gen_server:call(AT, begin_t,?TIME_OUT)).

query_t(AT, Ref, Fun) ->
    tryCall(gen_server:call(AT, {doquery_t, {Ref, Fun}},?TIME_OUT)).

%% Cast is the async requests
update_t(AT, Ref, Fun) ->
    gen_server:cast(AT, {update_t, {Ref, Fun}}).

commit_t(AT, Ref) ->
    tryCall(gen_server:call(AT,{commit_t, Ref},?TIME_OUT)).

%%% Extra API
%% COM I assume that it works as intended
%% Returns {ok, ListOfPids}
get_pids(AT) ->
    tryCall(gen_server:call(AT,get_pids,?TIME_OUT)).

%%%-------------------------------------------------------------------
%%% Callback functions
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

%% ----------------------------------
%% -------------- ATS ---------------
init({server, Args}) ->
    %% TODO Init pool size, or something similar? 
    {ok,{Args,[]}};
%% ----------------------------------
%% ---------- Transaction -----------
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

%% A dictionary could also have been used instead of a key value pair (for the transaction mappings)

%% ----------------------------------
%% -------------- ATS ---------------
handle_call(stop_at_server, _, {State,Transactions}) ->
    stopAllTransactions(Transactions),
    {stop,normal,{ok,State},[]}; %% No reason to carry the state anymore
%% ----------------------------------
%% -------------- ATS ---------------
handle_call({doquery_t, {Ref, Fun}}, _, {State,Transactions}) ->
    {Reply,NewTransactions} =
	case lists:keyfind(Ref,1,Transactions) of
	    {Ref,TrPid,ready} ->
		try gen_server:call(TrPid, {doquery, Fun},?TIME_OUT) of
		    error ->
			case ?MIN_POOL of
			    false ->
				{aborted, lists:keyreplace(Ref,1,Transactions,{Ref,TrPid,aborted})};
			    true ->
				stopTransaction(TrPid),
				{aborted, lists:keydelete(Ref,1,Transactions)}
			end;
		    Result -> {Result,Transactions}
		catch
		    _:_ -> {timeout,Transactions}
		end;
	    _ ->
		{aborted,Transactions}
	end,
    {reply, Reply, {State, NewTransactions}};
%% ----------------------------------
%% -------------- ATS ---------------
handle_call(begin_t, _, {State,Transactions}) ->
    %% COM why use key pair instead of just using the returned pid as the 'ref' value
    %% or something else? This allows us to maintain a unique reference to the user but internally
    %% maintain a small pool of processes instead of killing and spawning new each item
    URef = make_ref(),
    NewTransactions =
	case lists:keyfind(idle,3,Transactions) of
	    false ->
		{ok, TrPid} = gen_server:start(at_server, {transaction, State}, []),
		[{URef,TrPid,ready}|Transactions];
	    {Ref,TrPid,idle} ->
		%% Make sure to update its state to be of ours
		try gen_server:call(TrPid,{initialize,{State,ready}},?TIME_OUT) of
		    ok ->
			lists:keyreplace(Ref,1,Transactions,{URef,TrPid,ready})
		catch
		    _:_ ->
			{ok, TrPid} = gen_server:start(at_server, {transaction, State}, []),
			[{URef,TrPid,ready}|Transactions]
		end
	end,
    {reply,{ok,URef},{State,NewTransactions}};
%% ----------------------------------
%% -------------- ATS ---------------
handle_call({commit_t, Ref}, _, {State, Transactions}) ->
    {Reply, NewState, NewTransactions} =
	case lists:keyfind(Ref,1,Transactions) of
	    {Ref,TrPid,ready} ->
		try gen_server:call(TrPid, {doquery,fun(I) -> I end},?TIME_OUT) of
		    error ->
			{aborted,State,lists:keyreplace(Ref,1,Transactions,{Ref,TrPid,aborted})};
		    {ok, NS} ->
			%% Abort all transactions now,
			%% ei set their state to idle
			%% Note that their state does not get 'cleaned up' this is done in begin_t
			case ?MIN_POOL of
			    false ->
				NT = lists:map(fun({R,P,_}) -> {R,P,idle} end, Transactions),
				{ok,NS,NT};
			    true ->
				stopAllTransactions,
				{ok,NS,[]}
			end
		catch
		    _:_ -> {timeout,State,Transactions}
		end;
	    _ ->
		{aborted,State,Transactions}
	end,
    {reply,Reply,{NewState,NewTransactions}};
%% ----------------------------------
%% -------------- ATS ---------------
handle_call(get_pids, _, {State, Transactions}) ->
    AllPids = [self()|lists:flatmap(fun({_,P,_}) -> [P] end, Transactions)],
    {reply, {ok, AllPids}, {State, Transactions}};
%% ----------------------------------
%% ---------- Transaction -----------
handle_call({initialize,InitState}, _, _) ->
    {reply, ok, InitState};
%% ----------------------------------
%% ---------- Transaction -----------
handle_call(stop_at_trans, _, {State, _}) ->
    {stop,normal,{ok,State},[]}; %% No reason to carry the state anymore
%% ----------------------------------
%% ---------- Transaction -----------
handle_call({doquery,_}, _, {State, aborted}) ->
    {reply,error,{State,aborted}};
%% ----------------------------------
%% -------------- Both --------------
handle_call({doquery,Fun}, _, {State,Satalite}) ->
    Reply = try Fun(State) of
		Result -> {ok,Result}
	    catch
		%% TODO report the error to the caller?
		_:_ -> error
	    end,
    {reply,Reply,{State,Satalite}};
%% ----------------------------------
%% -------------- Both --------------
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

%% ----------------------------------
%% -------------- ATS ---------------
handle_cast({update_t, {Ref, Fun}}, {State, Transactions}) ->
    case lists:keyfind(Ref,1,Transactions) of
	{Ref,TrPid,ready} ->
	    gen_server:cast(TrPid,{update, Fun});
	_ ->
	    do_nothing
    end,
    {noreply, {State, Transactions}};
%% ----------------------------------
%% ---------- Transaction -----------
handle_cast({update, _}, {State, aborted}) ->
    {noreply,{State,aborted}};
handle_cast({update, Fun}, {State, ready}) ->
    NewState = try Fun(State) of
		   Result -> {Result,ready}
	       catch
		   %% TODO report the error to the caller?
		   _:_ -> {State,aborted}
	       end,
    {noreply,NewState};
handle_cast(stop_at_trans, State) ->
    {stop,normal,State};
%% ----------------------------------
%% -------------- Both --------------
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

%% ----------------------------------
%% -------------- Both --------------
%% TODO
handle_info(_, State) ->
    io:format("Someone called info!!!"),
    {noreply,State}.

%%%----------------------------------
%% Module:terminate(Reason, State)
%% ----Types:
%% Reason = normal | shutdown | {shutdown,term()} | term()
%% State = term()
%%%----------------------------------

%% ----------------------------------
%% -------------- Both --------------
terminate(normal, _) ->
    ok;
%% ----------------------------------
%% -------------- ATS ---------------
terminate(Error, {State,[H|T]}) ->
    io:format(
      "#####Error: at_server with state: ~p~n"
      ++"#####Terminating due to some unexpected error: ~p!~n",[State, Error]),

    %% Try to shutdown each living transaction
    stopAllTransactions([H|T]);
%% ----------------------------------
%% ---------- Transaction -----------
terminate(Error, State) ->
    io:format(
      "#####Error: transaction with state: ~p~n"
      ++"#####Terminating due to some unexpected error: ~p!~n",[State, Error]),
    ok.

%%%----------------------------------
%% Module:code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%% ----Types:
%% OldVsn = Vsn | {down, Vsn}
%% Vsn = term()
%% State = NewState = term()
%% Extra = term()
%% Reason = term()
%%%----------------------------------

%% The code_change/3 callback is not used and therefore not really implemented,
%% although present due to the expected callback exports

%% ----------------------------------
%% -------------- Both --------------
code_change(_, State, _) ->
    {ok,State}.


%%%-------------------------------------------------------------------
%%% Helper server-functions
%%%-------------------------------------------------------------------

stopAllTransactions(Transactions) ->
    lists:foreach(fun({_,P,_}) -> stopTransaction(P) end, Transactions).

%% TODO do I need to catch the ok message ?
stopTransaction(Pid) ->
    try gen_server:call(Pid,stop_at_trans,?TIME_OUT)
    catch 
	_:_ -> gen_server:cast(Pid,stop_at_trans)
    end.

tryCall(Call) ->
    try Call of
	Result ->
	    Result
    catch
	timeout:_ ->
	    timeout
    end.
