%%%-------------------------------------------------------------------
%%% Student name:
%%% Student KU-id:
%%%-------------------------------------------------------------------

-module(at_server).

-behaviour(gen_server).

% Interface functions
-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

% gen_server callback functions
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3, format_status/2]).

% Testing
-export([test/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    %% TODO use start or start_link?
    %% TODO handle error cases here? ie the ignore and {error,erro} respons?
    gen_server:start({local,at_server}, at_server, State, []).

%% Default timeout value is 5000 ms
%% call/2 is a synchronous call
%% COM why do I use sync to stop?
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
    %% COM why sync and not async, since ass. text doesnt say anything
    gen_server:call(AT,{commit_t, Ref}).

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

init(Args) ->
    %% TODO Init pool size, or something similar? 
    {ok,{Args,[]}}.

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

handle_call(stop_at_server, From, {State,Transactions}) ->
    %% TODO stop all transactions and pools %% Move that work down to terminate? To stick the the api description
    {stop,normal,{ok,State},[]}; %% No reason to carry the state anymore
handle_call({doquery,Fun}, From, {State,Transactions}) ->
    Result = Fun(State),
    {reply,{ok,Result},{State,Transactions}};
handle_call({doquery_t, {Ref, Fun}}, From, {State,Transactions}) ->
    %% TODO handle case where Ref does not exist
    {Ref,TrPid} = lists:keyfind(Ref,1,Transactions),
    Reply = gen_server:call(TrPid, {doquery, Fun}),
    {reply, Reply, {State, Transactions}};
handle_call(begin_t, From, {State,Transactions}) ->
    %% COM why use key pair instead of just using the returned pid as the 'ref' value
    %% or something else?
    URef = make_ref(),
    {ok, TrPid} = gen_server:start(at_server, State, []),
    {reply,{ok,URef},{State,[{URef,TrPid}|Transactions]}};
handle_call({update, Fun}, From, {State, Transactions}) ->
    NewState = Fun(State),
    {reply,ok,{NewState,Transactions}};
handle_call({commit_t, Ref}, From, {State, Transactions}) ->
    %% TODO abort all other transactions
    {Ref,TrPid} = lists:keyfind(Ref,1,Transactions),
    {ok,NewState} = gen_server:call(TrPid, {doquery,fun(X) -> X end}),
    {reply,ok,{NewState,Transactions}}.


test() ->
    {ok,A} = start([13]),
    {ok,R} = begin_t(A),
    Aa = doquery(A,fun(X) -> X end),
    Ab = query_t(A,R, fun(X) -> multA(X) end),
    ok = update_t(A,R,fun(X) -> multA(X) end),
    Ac = query_t(A,R,fun(X) -> multA(X) end),
    ok = commit_t(A,R),
    Ad = doquery(A,fun(X) -> X end),
    stop(A),
    {Aa,Ab,Ac,Ad}.

multA([X]) ->
    [X*1000].

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
    {Ref,TrPid} = lists:keyfind(Ref,1,Transactions),
    %% COM why sync and not async?
    ok = gen_server:call(TrPid,{update, Fun}),
    {noreply, {State, Transactions}}.

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

handle_info(Info, State) ->
    result.

%%%----------------------------------
%% Module:terminate(Reason, State)
%% ----Types:
%% Reason = normal | shutdown | {shutdown,term()} | term()
%% State = term()
%%%----------------------------------

terminate(normal, State) ->
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

code_change(OldVsn, State, Extra) ->
    result.

%%%----------------------------------
%%% This one is optional!
%% Module:format_status(Opt, [PDict, State]) -> Status
%% ----Types:
%% Opt = normal | terminate
%% PDict = [{Key, Value}]
%% State = term()
%% Status = term()
%%%----------------------------------

%% TODO needed?
format_status(Opt, [PDict, State]) ->
    status.
