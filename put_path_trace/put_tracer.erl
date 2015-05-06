%% Tool to generate a process diagram for a put request.
-module(put_tracer).
-export([start/1, process/2, create_json/1]).

-record(fcall, {start_time, end_time, mfa, sub_calls = []}).
-record(run, {start_time, end_time, initial_mfa, seen_call = false,
              events = []}).
-record(proc, {idx, label, pid, stack = [], runs = [], current = #run{}}).
-record(msg, {from, to, time, payload}).
-record(state, {caller, txt_file, pending_link, filename, start_time, end_time,
                procs = dict:new(), messages = [], spawned = []}).

%% Set up tracing so that starting a PUT request generates a sequential
%% token and trace the activity that it generates.
start(Filename) ->
    % Start a file output tracer and also use it for sequential token
    % tracing.
    PortFun = dbg:trace_port(file, Filename),
    dbg:tracer(port, PortFun),
    {ok, Tracer} = dbg:get_tracer(),
    seq_trace:set_system_tracer(Tracer),
    TFlags = [send, 'receive', running, procs],
    % Enable function call tracing, but start in silent mode
    dbg:p(all, [call, arity, return_to, sos, timestamp, silent]),
    % Make any function call while the process has a seq_token start
    % tracing messages, scheduling and process events, etc.
    % Disable tracing as soon as a function is called but the process
    % does not have an active seq_token.
    dbg:tpl('_', [{'_', [{is_seq_trace}],
                   [{silent, false},
                    {trace, [], TFlags},
                    {exception_trace}
                   ]},
                  {'_', [{'not', {is_seq_trace}}],
                   [{silent, true},
                    {trace, TFlags, []}
                   ]}
                 ]),
    % Earliest known function call in the write path. Should probably
    % replace with an earlier one in the webmachine/mochiweb request
    % handling code. seq_token starts here.
    dbg:tpl(riak_kv_wm_object, malformed_request, 2,
           [{'_', [],
             [{set_seq_token, send, true},
              {set_seq_token, timestamp, true},
              {trace, [], TFlags}
             ]}]),
    dbg:tpl(riak_kv_pb_object, decode, 2,
           [{'_', [],
             [{set_seq_token, label, 3},
              {set_seq_token, send, true},
              {set_seq_token, timestamp, true},
              {trace, [], TFlags}
             ]}]),
    % These are 3 processes created during a write. Unfortunately seq_trace
    % tokens are not passed automatically to the new process, so we are
    % activating the sequential token manually on their first function call.
    dbg:tpl(riak_kv_get_fsm, init, 1,
            [{'_',[],[{set_seq_token, label, 1},
                      {set_seq_token, timestamp, true},
                      {set_seq_token, send, true},
                      {silent, false},
                      {trace, [], TFlags}
                     ]}]),
    dbg:tpl(riak_kv_put_fsm, init, 1,
            [{'_',[],[{set_seq_token, label, 2},
                      {set_seq_token, timestamp, true},
                      {set_seq_token, send, true},
                      {silent, false},
                      {trace, [], TFlags}
                     ]}]),
    dbg:tpl(webmachine_decision_core, do_log, 1,
            [{'_',[],[{set_seq_token, label, 3},
                      {set_seq_token, timestamp, true},
                      {set_seq_token, send, true},
                      {silent, false},
                      {trace, [], TFlags}
                     ]}])
    .

create_json(Filename) ->
    {ok, TxtFile} = file:open(Filename ++ ".txt", [write]), 
    Client = dbg:trace_client(file, Filename,
                              {fun process/2,
                               #state{filename = Filename ++ ".json",
                                      txt_file = TxtFile,
                                      pending_link = self(),
                                      caller = self()}}),
    % Wait until the client is done processing the entire trace output file.
    Ref = monitor(process, Client),
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

% Write a list to the output file and handle internal (but not final)
% commas.
write_list(_OF, _EFun, []) ->
    ok;
write_list(OF, EFun, [E|Rest]) ->
    EFun(OF, E),
    case Rest of
        [] ->
            io:format(OF, "\n", []);
        _ ->
            io:format(OF, ",\n", [])
    end,
    write_list(OF, EFun, Rest).

write_call(OF, #fcall{mfa = {M, F, Args}, start_time = S, end_time = E,
                      sub_calls = Sub}) ->
    A = case is_list(Args) of true -> length(Args); false -> Args end,
    io:format(OF, "{\"name\": \"~p:~p/~p\", \"start\": ~p, \"end\": ~p,"
              " \"calls\":[\n", [M, F, A, S, E]),
    write_list(OF, fun write_call/2, Sub),
    io:format(OF, "]}\n", []),
    ok.

write_proc(OF, #proc{label = Label, pid = Pid, runs = Runs}) ->
    io:format(OF, "\t\t{\"label\": \"~s\", \"pid\": \"~p\", \"runs\":\n",
              [Label, Pid]),
    io:format(OF, "\t\t[\n", []),
    write_list(OF,
               fun(F, #run{start_time = RS, end_time = RE, events = Events}) ->
                       io:format(F, "\t\t\t{\"start\": ~p, \"end\": ~p,"
                                 " \"calls\":[\n", [RS, RE]),
                       write_list(OF, fun write_call/2, Events), 
                       io:format(F, "]}", [])
               end, Runs),
    io:format(OF, "\t\t]}", []),
    ok.

% Poor man's JSON string escape.
escape_str(Term) ->
    S = io_lib:format("~p", [Term]),
    S1 = re:replace(S, "\n", "\\\\n", [global]),
    S2 = re:replace(S1, "\r", "\\\\r", [global]),
    re:replace(S2, "\"", "\\\\\"", [global]).

write_msg(OF, #msg{from = From, to = To, time = Time, payload = Payload}) ->
    io:format(OF, "\t\t{\"time\": ~p, \"from\": ~p, \"to\": ~p,"
              " \"payload\": \"~s\"}",
              [Time, From, To, escape_str(Payload)]),
    ok.

write_spawn(OF, Pid) ->
    io:format(OF, "\"~p\"", [Pid]),
    ok.

%% Write current results as a JSON file.
write_result(#state{filename = Fname,
                    start_time = Start,
                    spawned = Spawned,
                    end_time = End,
                    procs = Procs,
                    messages = Msgs}) ->
    {ok, OF} = file:open(Fname, [write]),
    io:format(OF, "{\n\t\"start\": ~p,\n\t\"end\": ~p,\n", [Start, End]),
    io:format(OF, "\t\"spawned\": [", []),
    write_list(OF, fun write_spawn/2, Spawned),
    io:format(OF, "],\n", []),
    io:format(OF, "\t\"processes\": [\n", []),
    write_list(OF, fun write_proc/2, Procs), 
    io:format(OF, "\t],\n", []),
    io:format(OF, "\t\"messages\": [\n", []),
    write_list(OF, fun write_msg/2, lists:reverse(Msgs)),
    io:format(OF, "\t]\n", []),
    io:format(OF, "}\n", []),
    file:close(OF),
    ok.

update_time(Time, State = #state{start_time = undefined}) ->
    {M, S, U} = Time,
    Start = M * 1000000000 + S * 1000 + U / 1000.0,
    State#state{start_time = Start,
                end_time = Time};
update_time(Time, State) ->
    State#state{end_time = Time}.

relative_time({MS, S, US}, Start) ->
    (MS * 1000000000 + S * 1000 + US / 1000.0) - Start.

get_proc(Pid, State = #state{procs = Procs}) ->
    case dict:find(Pid, Procs) of
        {ok, Proc} ->
            {Proc, State};
        error ->
            N = dict:size(Procs),
            Proc = #proc{idx = N, pid = Pid},
            Procs1 = dict:store(Pid, Proc, Procs),
            {Proc, State#state{procs = Procs1}}
    end.

update_proc(Proc = #proc{pid = Pid}, State = #state{procs = Procs}) ->
    Procs1 = dict:store(Pid, Proc, Procs),
    State#state{procs = Procs1}.

add_msg(FromPid, Msg, ToPid, Time, State = #state{messages = Msgs}) ->
    {FromProc, State1} = get_proc(FromPid, State),
    {ToProc, State2} = get_proc(ToPid, State1),
    State2#state{messages = [#msg{from = FromProc#proc.idx,
                                to = ToProc#proc.idx,
                                time = Time,
                                payload = Msg} | Msgs]}.

%%ensure_pid(Pid, State = #state{procs = Procs}) ->
%%   case dict:is_key(Pid, Procs)

%% Convert procs dict to list and close any pending runs.
prepare_results(State = #state{procs = Procs,
                               start_time = StartTime,
                               end_time = EndTime}) ->
    L1 = dict:fold(fun(_, P, Acc) ->
                           P1 = do_proc_out(P, undefined),
                           [P1|Acc]
                   end, [], Procs),
    IdxCmp = fun(#proc{idx=LI}, #proc{idx=RI}) -> LI =< RI end,
    L2 = lists:sort(IdxCmp, L1),
    State#state{procs=L2, start_time = 0.0,
                end_time = relative_time(EndTime, StartTime)}.

do_proc_out(Proc = #proc{current = #run{start_time = undefined}}, _) ->
    Proc#proc{current = #run{}};
%% If only a process in event, ignore. It means process started
%% lost its seq_token before doing any real work.
do_proc_out(Proc = #proc{current = #run{seen_call = false}}, _) ->
    Proc#proc{current = #run{}};
do_proc_out(Proc = #proc{current = Current,
                         stack = Stack, runs = Runs}, Time) ->
    #run{events = Events, initial_mfa = InitMFA, end_time = REnd} = Current,
    Events1 =
    case {Stack, InitMFA, Events} of
        {[], undefined, _} ->
            Events;
        {[], _, [#fcall{end_time=PrevTime}|_]} ->
            FinalCall = #fcall{mfa = InitMFA, start_time = PrevTime,
                               end_time = Time},
            [FinalCall|Events];
        _ ->
            FinalCall = collapse_calls(Time, Stack),
            [FinalCall | Events]
    end,
    Current1 = Current#run{end_time = choose_time(REnd, Time),
                           events = lists:reverse(Events1)},
    Proc#proc{current = #run{}, runs = [Current1 | Runs]}.

do_proc_out(Proc = #proc{}, Time, State1) ->
    Proc1 = do_proc_out(Proc, Time),
    update_proc(Proc1, State1);
do_proc_out(Pid, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    do_proc_out(Proc, Time, State1).

do_proc_in(Pid, MFA, Time, State) ->
    {Proc = #proc{stack = Stack}, State1} = get_proc(Pid, State),
    % Remove calls from previous run, leave unfinished calls with new time.
    Stack1 = [Call#fcall{sub_calls=[], start_time = Time} || Call <- Stack],
    Proc1 = Proc#proc{stack = Stack1,
                      current = #run{start_time = Time,
                                     initial_mfa = MFA}},
    update_proc(Proc1, State1).

remove_no_returns([#fcall{mfa={ets, select_reverse, _}} | Stack]) ->
    Stack;
remove_no_returns([#fcall{mfa={riak_kv_pb_object, decode, _}} | Stack]) ->
    Stack;
remove_no_returns(Stack) ->
    Stack.

do_call(Pid, MFArgs, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    Proc1 =
    case Proc of
        #proc{current = Current = #run{start_time = undefined}} ->
            Proc#proc{current = Current#run{start_time = Time}};
        _ ->
            Proc
    end,
    Fcall = #fcall{start_time = Time, end_time = Time, mfa = MFArgs},
    Stack1 = remove_no_returns(Proc1#proc.stack),
    Proc2 = Proc1#proc{stack = [Fcall | Stack1],
                       current = Proc1#proc.current#run{seen_call = true,
                                                        end_time = Time}},
    Proc3 = maybe_label_proc(Proc2, MFArgs),
    update_proc(Proc3, State1).

do_spawn(Pid, State) ->
    State#state{spawned = [Pid|State#state.spawned]}.

update_latest_time(Pid, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    #proc{stack = Stack} = Proc,
    case Stack of
        [] ->
            State1;
        [TopCall | OtherCalls] ->
            Stack1 = [ TopCall#fcall{end_time = Time} | OtherCalls],
            Proc2 = Proc#proc{stack = Stack1},
            update_proc(Proc2, State1)
    end.

maybe_label_proc(Proc, {riak_kv_w1c_worker, handle_cast, _}) ->
    Proc#proc{label="W1 Worker"};
maybe_label_proc(Proc, {riak_kv_put_fsm, init, _}) ->
    Proc#proc{label="Put FSM"};
maybe_label_proc(Proc, {riak_kv_get_fsm, init, _}) ->
    Proc#proc{label="Get FSM"};
maybe_label_proc(Proc, {riak_kv_wm_object, malformed_request, _}) ->
    Proc#proc{label="WM Worker"};
maybe_label_proc(Proc, {riak_kv_index_hashtree, handle_cast, _}) ->
    Proc#proc{label="Vnode AAE process"};
maybe_label_proc(Proc, {riak_kv_vnode, handle_command, _}) ->
    Proc#proc{label="KV Vnode"};
maybe_label_proc(Proc, {riak_core_vnode_proxy, loop, _}) ->
    Proc#proc{label="Vnode Proxy"};
maybe_label_proc(Proc, {riak_kv_stat_worker, handle_cast, _}) ->
    Proc#proc{label="KV Stat Worker"};
maybe_label_proc(Proc, {sidejob_supervisor, handle_call, _}) ->
    Proc#proc{label="Sidejob Sup"};
maybe_label_proc(Proc, {exometer_probe, handle_msg, _}) ->
    Proc#proc{label="Exometer Probe"};
maybe_label_proc(Proc, {exometer_report, handle_cast, _}) ->
    Proc#proc{label="Exometer Report"};
maybe_label_proc(Proc, {exometer_admin, handle_call, _}) ->
    Proc#proc{label="Exometer Admin"};
maybe_label_proc(Proc, {mochiweb_socket_server, handle_info, _}) ->
    Proc#proc{label="Mochi Socket"};
maybe_label_proc(Proc, {webmachine_decision_core, do_log, _}) ->
    Proc#proc{label="WM Log Worker"};
maybe_label_proc(Proc, {riak_pb_codec, decode, _}) ->
    Proc#proc{label="PB worker"};
maybe_label_proc(Proc, _) ->
    Proc.

do_return(Pid, {M, F, A}, Time, State) ->
    {Proc = #proc{stack = Stack,
                  current = Current = #run{events = Events}},
     State1} = get_proc(Pid, State),
    %% Assert return matches top function name and arity.
    [Fcall = #fcall{mfa = {_, _, Args}, sub_calls = Fsub} | Stack1] =
        remove_no_returns(Stack),
    case Fcall#fcall.mfa of
        {M, F, _} ->
            ok;
        _ ->
            throw({mismatched_return, {M, F, A}, Stack})
    end,
    A = case is_list(Args) of true -> length(Args); false -> Args end,
    Fcall2 = Fcall#fcall{end_time = Time,
                         sub_calls = lists:reverse(Fsub)},
    Proc1 =
    case Stack1 of
        [] ->
            Proc#proc{stack = [],
                      current = Current#run{events = [Fcall2|Events],
                                            end_time = Time}};
        [Pcall = #fcall{sub_calls = SubCalls} | Stack2 ] ->
            Pcall2 = Pcall#fcall{sub_calls = [Fcall2 | SubCalls],
                                 end_time = Time},
            Proc#proc{stack = [Pcall2 | Stack2]}
    end,
    update_proc(Proc1, State1).

choose_time(OldTime, undefined) ->
    OldTime;
choose_time(undefined, NewTime) ->
    NewTime;
choose_time(OldTime, NewTime) ->
    max(OldTime, NewTime).

collapse_calls(Time, [#fcall{end_time = EndTime, sub_calls = Sub} = Fcall]) ->
    Fcall#fcall{end_time = choose_time(Time, EndTime),
                sub_calls = lists:reverse(Sub)};
collapse_calls(Time, [#fcall{sub_calls = Sub1, end_time = EndTime} = Fcall1,
                      #fcall{sub_calls = Sub2} = Fcall2 | Rest]) ->
    collapse_calls(Time,
                   [Fcall2#fcall{sub_calls =
                                 [Fcall1#fcall{end_time = choose_time(Time, EndTime),
                                               sub_calls = lists:reverse(Sub1)}
                                  | Sub2]} | Rest]).

%% { start, end, processes: [{label, pid, runs:[{start, end}]}], messages:
%% [{time, from, to, payload}]
%% Produce JSON data for visualization from trace

% Link to caller for easier debugging on crash before processing data.
process(T, State = #state{pending_link = Pid}) when is_pid(Pid) ->
    link(Pid),
    process(T, State#state{pending_link = undefined});
%% End of input trace file reached.
process(end_of_trace, State) ->
    io:format("Finished reading trace, writing results\n", []),
    State1 = prepare_results(State),
    file:close(State#state.txt_file),
    write_result(State1);

%% Message sent.
process(T = {seq_trace, _, {send, _, Pid, To, Msg}, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    add_msg(Pid, Msg, To, Time1, State1);

% Erlang process being scheduled.
process(T = {trace_ts, Pid, in, MFA, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    Time1 = relative_time(Time, State#state.start_time),
    do_proc_in(Pid, MFA, Time1, State);

%% Erlang process being de-scheduled or exiting.
process(T = {trace_ts, Pid, OutExit, _, Time}, State)
  when OutExit == out; OutExit == exit ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    do_proc_out(Pid, Time1, State1);

%% Entering function call.
process(T = {trace_ts, Pid, call, MFArgs, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    do_call(Pid, MFArgs, Time1, State1);

%% Returning from a function call.
process(T = {trace_ts, Pid, ReturnToken, MFA, _, Time}, State)
  when ReturnToken == return_from; ReturnToken == exception_from ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    do_return(Pid, MFA, Time1, State1);

process(T = {trace_ts, Pid, 'receive', _Msg, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    Time1 = relative_time(Time, State#state.start_time),
    update_latest_time(Pid, Time1, State);
    
process(T = {trace_ts, Pid, spawn, _Args, _Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    do_spawn(Pid, State);
process(T, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State.

