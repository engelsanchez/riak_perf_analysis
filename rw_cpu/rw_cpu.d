#pragma D option quiet
#pragma D option dynvarsize=128m

/*
 * Measures CPU usage of read/write operations through the PB interface
 * under load.
 * - CPU spent on API processes.
 * - CPU spent on the FSM process.
 * - CPU spent on vnodes.
 * - CPU spent on stats processes.
 */

/* Operations activated through dyntrace:p() calls. */
inline int TAG_PROC = 1;
inline int COUNT_OP = 2;

/* Numeric erlang process type. */
inline int PROC_API = 1;
inline int PROC_FSM = 2;
inline int PROC_VNODE = 3;
inline int PROC_STATS = 4;
inline int PROC_AAE = 5;

inline int num_cpus = 1;
inline int cpu_dsecs = num_cpus * 1000000;

/* When we start measuring */
uint64_t start_time;
/* Maps an Erlang pid to a process type */
int ptype[string];
self string pid;
self uint64_t start;

sched:::on-cpu
/ pid == $target /
{
    self->on_time = timestamp;
}

sched:::off-cpu
/ self->on_time /
{
    @riak_time = sum(timestamp - self->on_time);
    self->on_time = 0;
}

/* Associate process with a type. This version takes an explicit pid
 * as a string and is used to tag processes other than the one calling it.
 */
erlang$target:::user_trace-i4s4
/ arg2 == TAG_PROC && arg6 && !ptype[copyinstr(arg6)] /
{
    this->pid = copyinstr(arg6);
    ptype[this->pid] = arg3;
    @ptype[arg3] = sum(1);
    /* printf("Tagging process %s as %u\n", self->pid, arg3); */
}

/* Associate process with a type */
erlang$target:::user_trace-i4s4
/ arg2 == TAG_PROC && !arg6 && !ptype[copyinstr(arg0)] /
{
    this->pid = copyinstr(arg0);
    ptype[this->pid] = arg3;
    @ptype[arg3] = sum(1);
    /* printf("Tagging process %s as %u\n", self->pid, arg3); */
}

erlang$target:::process-scheduled
{
    self->proc_start = vtimestamp;
}

erlang$target:::process-unscheduled
/ self->proc_start && ptype[copyinstr(arg0)] == PROC_API /
{
    @api_time = sum(vtimestamp - self->proc_start);
}

erlang$target:::process-unscheduled
/ self->proc_start &&  ptype[copyinstr(arg0)] == PROC_FSM /
{
    @fsm_time = sum(vtimestamp - self->proc_start);
}

erlang$target:::process-unscheduled
/ self->proc_start && ptype[copyinstr(arg0)] == PROC_VNODE /
{
    @vnode_time = sum(vtimestamp - self->proc_start);
}

erlang$target:::process-unscheduled
/ self->proc_start && ptype[copyinstr(arg0)] == PROC_STATS /
{
    @stats_time = sum(vtimestamp - self->proc_start);
}

erlang$target:::process-unscheduled
/ self->proc_start && ptype[copyinstr(arg0)] == PROC_AAE /
{
    @aae_time = sum(vtimestamp - self->proc_start);
}

erlang$target:::process-unscheduled
/ self->proc_start /
{
    self->proc_start = 0;
}

/* Remove process type entry when it dies */
erlang$target:::process-exit
/ ptype[copyinstr(arg0)] /
{
    this->pid = copyinstr(arg0);
    @ptype[ptype[this->pid]] = sum(-1);
    ptype[this->pid] = 0;
}

BEGIN
{
    start_time = walltimestamp;
    printf("time,tot,api,fsm,vnode,stats\n");
   
    @riak_time = sum(0);
    @api_time = sum(0);
    @fsm_time = sum(0);
    @vnode_time = sum(0);
    @stats_time = sum(0);
    @ops = count();
}

profile:::tick-1s
{
    normalize(@riak_time,   cpu_dsecs);
    normalize(@api_time,   cpu_dsecs);
    normalize(@fsm_time,   cpu_dsecs);
    normalize(@vnode_time,   cpu_dsecs);
    normalize(@stats_time,   cpu_dsecs);

    printf("%u", (walltimestamp - start_time) / 1000000);
    printa(",%@u,%@u,%@u,%@u,%@u\n",
           @riak_time, @api_time, @fsm_time, @vnode_time, @stats_time);
    printf("Types:\n");
    printa("Type : %u -> %@u\n", @ptype);

    trunc(@riak_time);
    trunc(@api_time);
    trunc(@fsm_time);
    trunc(@vnode_time);
    trunc(@stats_time);
    trunc(@ops);
}

END
{
    clear(@riak_time);
    clear(@api_time);
    clear(@fsm_time);
    clear(@vnode_time);
    clear(@stats_time);
    clear(@ops);
}

