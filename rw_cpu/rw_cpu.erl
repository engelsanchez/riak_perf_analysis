-module(rw_cpu).
-export([tag_procs/0, tag_proc/1, tag_proc/2]).

-define(TAG_PROC, 1).

-define(PROC_API, 1).
-define(PROC_FSM, 2).
-define(PROC_VNODE, 3).
-define(PROC_STATS, 4).
-define(PROC_AAE, 5).

tag_procs() ->
    %% Tag vnode processes
    [tag_proc_d(vnode, Pid)
     || {_Idx, Pid} <- riak_core_vnode_manager:all_index_pid(riak_kv_vnode)],
    %% Tag current PB processes
    [case is_pid(Pid) of
         true -> tag_proc(api, Pid);
         false -> ok
     end || {_, Pid, _, _} <- supervisor:which_children(riak_api_pb_sup)],
    %% Tag exometer processes
    ExoAdmin = whereis(exometer_admin),
    tag_proc_d(stats, ExoAdmin),
    %% Probe processes only stored in the exometer_admin dictionary as
    %% {Pid, Ref} pairs.
    {dictionary, EADict} = process_info(ExoAdmin, dictionary),
    [tag_proc_d(stats, Pid) || {Pid, Ref} <- EADict,
                             is_pid(Pid), is_reference(Ref)],
    %% Tag sidejob managed kv stats workers.
    %% I could just pick them up by registered name (riak_kv_stat_sj_N)
    [KVStatsSJSup] = [Pid || {sidejob_worker_sup, Pid, _, _}
                             <- supervisor:which_children(riak_kv_stat_sj)],
    [tag_proc_d(stats, Pid) || {_, Pid, _, _}
                             <- supervisor:which_children(KVStatsSJSup)],
    ok.

tag_proc_d(Type, Pid) ->
    timer:sleep(5),
    tag_proc(Type, Pid).

tag_proc(Tag) ->
    tag_proc(Tag, self()).

tag_proc(api, Pid) ->
    dyntrace:p(?TAG_PROC, ?PROC_API, pid_to_list(Pid));
tag_proc(fsm, Pid) ->
    dyntrace:p(?TAG_PROC, ?PROC_FSM, pid_to_list(Pid));
tag_proc(vnode, Pid) ->
    dyntrace:p(?TAG_PROC, ?PROC_VNODE, pid_to_list(Pid));
tag_proc(stats, Pid) ->
    dyntrace:p(?TAG_PROC, ?PROC_STATS, pid_to_list(Pid));
tag_proc(aae, Pid) ->
    dyntrace:p(?TAG_PROC, ?PROC_AAE, pid_to_list(Pid)).
