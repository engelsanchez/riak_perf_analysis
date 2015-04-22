%% Tool to generate a process diagram for a put request.
-module(put_tracer).
-export([start/1, process/2, create_json/1]).

-record(run, {start_time, end_time, events = []}).
-record(proc, {idx, label, pid, runs = [], current = #run{}}).
-record(msg, {from, to, time, payload}).
-record(state, {
          caller,
          txt_file,
          filename,
          start_time, end_time,
          procs = dict:new(),
          messages = []
        }).

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
    dbg:p(all, [call, return_to, sos, timestamp, silent]),
    dbg:tpl('_', [{'_', [{is_seq_trace}],
                   [{silent, false},
                    {trace, [], TFlags}
                   ]},
                  {'_', [{'not', {is_seq_trace}}],
                   [{silent, true},
                    {trace, TFlags, []}
                   ]}
                 ]),
    dbg:tpl(riak_kv_wm_object, malformed_request, 2,
           [{'_', [],
             [{set_seq_token, send, true},
              {set_seq_token, timestamp, true},
              {trace, [], TFlags}
             ]}]),
    % These are 3 processes created during tracing. Unfortunately seq_trace
    % tokens are not passed automatically to the new process, so we are
    % activating the sequential token manually on the first function call
    % for those processes.
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
                                      caller = self()}}),
    % Wait until the client is done processing the entire trace output file.
    Ref = monitor(process, Client),
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

write_proc(OF, #proc{label = Label, pid = Pid, runs = Runs}) ->
    io:format(OF, "\t\t{\"label\": \"~s\", \"pid\": \"~p\", \"runs\":\n", [Label, Pid]),
    io:format(OF, "\t\t[\n",[]),
    [begin
         io:format(OF, "\t\t\t{\"start\": ~p, \"end\": ~p },\n", [RS, RE])
     end || #run{start_time = RS, end_time = RE} <- Runs],
    io:format(OF, "\t\t]},\n", []),
    ok.

escape_str(Term) ->
    S = io_lib:format("~p", [Term]),
    S1 = re:replace(S, "\n", "\\\\n", [global]),
    S2 = re:replace(S1, "\r", "\\\\r", [global]),
    S3 = re:replace(S2, "\"", "\\\\\"", [global]),
    S3
    .

write_msg(OF, #msg{from = From, to = To, time = Time, payload = Payload}) ->
    io:format(OF, "\t\t{\"time\": ~p, \"from\": ~p, \"to\": ~p, \"payload\": \"~s\"},\n",
              [Time, From, To, escape_str(Payload)]),
    ok.

write_result(#state{filename = Fname,
                    start_time = Start,
                    end_time = End,
                    procs = Procs,
                    messages = Msgs}) ->
    {ok, OF} = file:open(Fname, [write]),
    io:format(OF, "{\n\t\"start\": ~p,\n\t\"end\": ~p,\n", [Start, End]),
    io:format(OF, "\t\"processes\": [\n", []),
    [write_proc(OF, Proc) || Proc <- Procs],
    io:format(OF, "\t],\n", []),
    io:format(OF, "\t\"messages\": [\n", []),
    [write_msg(OF, Msg) || Msg <- Msgs],
    io:format(OF, "\t],\n", []),
    io:format(OF, "};\n", []),
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
                           P1 = close_run(P, relative_time(EndTime, StartTime)),
                           [P1|Acc]
                   end, [], Procs),
    IdxCmp = fun(#proc{idx=LI}, #proc{idx=RI}) -> LI =< RI end,
    L2 = lists:sort(IdxCmp, L1),
    State#state{procs=L2, start_time = 0.0,
                end_time = relative_time(EndTime, StartTime)}.

close_run(Proc = #proc{current = #run{start_time = undefined}}, _) ->
    Proc;
close_run(Proc = #proc{current = Current, runs = Runs}, Time) ->
    Runs1 = [Current#run{end_time = Time}|Runs],
    Proc#proc{current = #run{}, runs = Runs1}.

close_run(Proc = #proc{}, Time, State1) ->
    Proc1 = close_run(Proc, Time),
    update_proc(Proc1, State1);
close_run(Pid, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    close_run(Proc, Time, State1).

open_run(Pid, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    Proc1 = Proc#proc{current = #run{start_time = Time}},
    update_proc(Proc1, State1).

add_call(Pid, _MFArgs, Time, State) ->
    {Proc, State1} = get_proc(Pid, State),
    case Proc of
        #proc{current = #run{start_time = undefined}} ->
            Proc1 = Proc#proc{current = #run{start_time = Time}},
            update_proc(Proc1, State1);
        _ ->
            State1
    end.

%% { start, end, processes: [{label, pid, runs:[{start, end}]}], messages:
%% [{time, from, to, payload}]
%% Produce JSON data for visualization from trace
process(end_of_trace, State) ->
    io:format("Finished reading trace, writing results\n", []),
    State1 = prepare_results(State),
    file:close(State#state.txt_file),
    write_result(State1);
process(T = {seq_trace, _, {send, _, Pid, To, Msg}, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    add_msg(Pid, Msg, To, Time1, State1);
process(T = {trace_ts, Pid, in, _, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    open_run(Pid, Time1, State1);
process(T = {trace_ts, Pid, OutExit, _, Time}, State)
  when OutExit == out; OutExit == exit ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    close_run(Pid, Time1, State1);
process(T = {trace_ts, Pid, call, MFArgs, Time}, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State1 = update_time(Time, State),
    Time1 = relative_time(Time, State1#state.start_time),
    add_call(Pid, MFArgs, Time1, State1);
process(T, State) ->
    io:format(State#state.txt_file, "~p.\n", [T]),
    State.

