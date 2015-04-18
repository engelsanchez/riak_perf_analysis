/*
 * Trace a bunch of stuff happening in Riak with the LevelDB backend.
 * Counters:
 *   - Number of reads
 *   - Number of writes
 *   - Number of queued up requests
 *   - Number of tables opened (table cache misses)
 *
 * I/O usage:
 *   - WAL log writes
 *   - SSTable flush writes
 *   - Compaction writes
 *   - Total read I/O
 *   - Total write I/O
 *   - Table open related reads
 *
 * CPU
 *   - Total Riak CPU usage
 *   - Erlang VM CPU usage
 *   - Eleveldb NIF CPU usage
 *   - Eleveldb threads CPU usage
 *   - Hot threads CPU usage
 *   - Compaction work CPU usage
 */
#pragma D option quiet

/* Thread types */
inline int SCHEDULER_THREAD = 1;
inline int ELEVELDB_THREAD = 2;
inline int HOT_THREAD = 3;

inline int num_cpus = 8;
/* To normalize CPU time to tenths of a percentage per second. */
inline int cpu_dsecs = num_cpus * 1000000;

self int thread_type;
self int logging;
self int compacting;
self uint64_t proc_enter_time;
self uint64_t erlang_start_time;
self uint64_t nif_start_time;
self uint64_t compaction_enter_time;
uint64_t start_time;

typedef struct slice {
    char * data;
    size_t size;
} slice_t;

sched:::on-cpu
/ pid == $target /
{
    self->proc_enter_time = vtimestamp;
}

sched:::off-cpu
/ pid == $target && self->proc_enter_time
  && self->thread_type == SCHEDULER_THREAD /
{
    @sched_time = sum(vtimestamp - self->proc_enter_time);
}

sched:::off-cpu
/ pid == $target && self->proc_enter_time 
  && self->thread_type == ELEVELDB_THREAD /
{
    @eleveldb_time = sum(vtimestamp - self->proc_enter_time);
}

sched:::off-cpu
/ pid == $target && self->thread_type == HOT_THREAD
  && self->proc_enter_time /
{
    @hot_thread_time = sum(vtimestamp - self->proc_enter_time);
}

sched:::off-cpu
/ pid == $target && self->proc_enter_time /
{
    @proc_time = sum(vtimestamp - self->proc_enter_time);
    self->proc_enter_time = 0;
}

/* notify_caller is only called from the eleveldb async pool, so mark
 * thread as an eleveldb thread if you see it. */
pid$target:eleveldb.so:eleveldb??eleveldb_thread_pool??notify_caller*:entry
/ self->thread_type != ELEVELDB_THREAD /
{
    self->thread_type = ELEVELDB_THREAD;
    @num_eleveldb = count();
}

/* This function was added for this script. */
pid$target:eleveldb.so:eleveldb??eleveldb_thread_pool??enqueue_work*:entry
/ self->thread_type != ELEVELDB_THREAD /
{
    @num_queued = count();
}

pid$target:eleveldb.so:leveldb??Table??Open*:entry
{
    @num_open_table = count();
}

/*
 * There is no function call inside the thread routine that we can trace,
 * and when adding a simple one to wrap the work, it gets inlined. DTrace shows
 * me the inlined function entry point probe, but does not appear to ever hit
 * it. So, resorting to adding a probe at a point inside the ThreadRoutine that
 * should be hit while it looks for work. This may need to be changed as
 * the code changes or is compiled in different platforms or with different
 * options.
 */
pid$target:eleveldb.so:leveldb??HotThread??ThreadRoutine*:40
/ self->thread_type != HOT_THREAD /
{
    self->thread_type = HOT_THREAD;
    @num_hot_thread = count();
}


/* Mark any thread scheduling an Erlang process as an Erlang scheduler
 * and count them. */

erlang$target:::process-scheduled
/ self->thread_type != SCHEDULER_THREAD /
{
    self->thread_type = SCHEDULER_THREAD;
    @num_sched = count();
}

/* Start counting time spent on Erlang code. */
 erlang$target:::process-scheduled
{
    self->erlang_start_time = vtimestamp;
}

erlang$target:::process-unscheduled
/ self->erlang_start_time /
{
    @erlang_time = sum(vtimestamp - self->erlang_start_time);
    self->erlang_start_time = 0;
}

/* Catch entering/leaving the eleveldb NIF calls and count that time */
pid$target:eleveldb.so:eleveldb??async*:entry,
pid$target:eleveldb.so:eleveldb??range_scan*:entry
{
    self->nif_start_time = vtimestamp;
}

pid$target:eleveldb.so:eleveldb??async*:return,
pid$target:eleveldb.so:eleveldb??range_scan*:return
{
    @nif_time = sum(vtimestamp - self->nif_start_time);
}

pid$target:eleveldb.so:eleveldb??async_write*:entry
{
    @num_writes = count();
}

pid$target:eleveldb.so:eleveldb??async_get*:entry
{
    @num_reads = count();
}

/* Count writes to the write ahead log. That is, append calls
 * inside of AddRecord calls. */
pid$target:eleveldb.so:*AddRecord*:entry
{
    self->logging = 1;
}

pid$target:eleveldb.so:*AddRecord*:return
{
    self->logging = 0;
}

pid$target:eleveldb.so:*PosixMmapFile*Append*:entry
/ self->logging /
{
    s = (slice_t*)copyin(arg2,sizeof(slice_t));
    @logged_size = sum(s->size);
}

/* 24 offset from PosixMmmapFile this to filename char * field YMMV */
/*
str_addr = *(char**)copyin(arg1 + 24, sizeof(char*));
fname = copyinstr((user_addr_t)str_addr);
printf("Writing to log file %p %s\n", str_addr, fname);
*/

/* Count file appends inside of the compaction call. */
pid$target:eleveldb.so:leveldb??CompactionTask??operator()():entry
{
    self->compacting = 1;
    self->compaction_enter_time = vtimestamp;
}

pid$target:eleveldb.so:leveldb??CompactionTask??operator()():return
{
    @compaction_time = sum(vtimestamp - self->compaction_enter_time);
    self->compacting = 0;
    self->compaction_enter_time = 0;
}

pid$target:eleveldb.so:*PosixMmapFile*Append*:entry
/ self->compacting /
{
    s = (slice_t*)copyin(arg2,sizeof(slice_t));
    @compaction_size = sum(s->size);
    /* 24 offset from PosixMmmapFile this to filename char * field YMMV */
    /*
    str_addr = *(char**)copyin(arg1 + 24, sizeof(char*));
    fname = copyinstr((user_addr_t)str_addr);
    printf("Writing to compaction output file %p %s\n", str_addr, fname);
    */

}

/* Count file appends in the middle of a memtable compaction call */
pid$target:eleveldb.so:*DBImpl??CompactMemTable*:entry
{
    self->flushing = 1;
}

pid$target:eleveldb.so:*DBImpl??CompactMemTable*:return
{
    self->flushing = 0;
}

pid$target:eleveldb.so:*PosixMmapFile*Append*:entry
/ self->flushing /
{
    s = (slice_t*)copyin(arg2,sizeof(slice_t));
    @flushing_size = sum(s->size);
    /* 24 offset from PosixMmmapFile this to filename char * field YMMV */
    /*
    str_addr = *(char**)copyin(arg1 + 24, sizeof(char*));
    fname = copyinstr((user_addr_t)str_addr);
    printf("Writing to compaction output file %p %s\n", str_addr, fname);
    */
 } 

/* Measure I/O.
 * io provider routines have 3 arguments:
 * - args[0] : block info
 * - args[1] : device info
 * - args[2] : file info
 * structs are defined in /usr/lib/dtrace/io.d
 * NOTE: io done events contain no process information, so to measure
 * latency you need to correlate them like iosnoop and iotop do, which
 * uses a global associative array with per device entries.
 * Relies on DTrace being supposedly thread safe when not writing the 
 * same key.
 */
io:::start
/ pid == $target && args[0]->b_flags & B_READ /
{
    @bytes_read = sum(args[0]->b_bcount);
}

io:::start
/ pid == $target && !(args[0]->b_flags & B_READ) /
{
    @bytes_written = sum(args[0]->b_bcount);
}

BEGIN
{
    start_time = walltimestamp;

    printf("time_ms,num_writes,num_reads,num_queued,num_open_table,");
    printf("proc_time,sched_time,erlang_time,nif_time,");
    printf("eleveldb_time,hot_thread_time,compaction_time,");
    printf("reads,writes,wal_writes,flush_writes,compaction_writes");
    printf("\n");

    @proc_time = sum(0);
    @sched_time = sum(0);
    @erlang_time = sum(0);
    @nif_time = sum(0);
    @eleveldb_time = sum(0);
    @hot_thread_time = sum(0);
    @compaction_time = sum(0);
    @bytes_read = sum(0);
    @bytes_written = sum(0);
    @logged_size = sum(0);
    @flushing_size = sum(0);
    @compaction_size = sum(0);
}

profile:::tick-1s
{
    normalize(@proc_time,       cpu_dsecs);
    normalize(@sched_time,      cpu_dsecs);
    normalize(@erlang_time,     cpu_dsecs);
    normalize(@nif_time,        cpu_dsecs);
    normalize(@eleveldb_time,   cpu_dsecs);
    normalize(@hot_thread_time, cpu_dsecs);
    normalize(@compaction_time, cpu_dsecs);
    normalize(@bytes_read,      1024);
    normalize(@bytes_written,   1024);
    normalize(@logged_size,     1024);
    normalize(@flushing_size,   1024);
    normalize(@compaction_size, 1024);

    printf("%u", (walltimestamp - start_time) / 1000000);
    printa(",%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u,%@u",
           @num_writes, @num_reads, @num_queued, @num_open_table,
           @proc_time, @sched_time, @erlang_time, @nif_time, @eleveldb_time,
           @hot_thread_time, @compaction_time,
           @bytes_read, @bytes_written,
           @logged_size, @flushing_size, @compaction_size);
    printf("\n");

    /* Notes:
     * hot thread time contains the compaction time. Only repair compactions
     * are done in the eleveldb thread pool.
     */
    trunc(@num_writes);
    trunc(@num_reads);
    trunc(@num_queued);
    trunc(@num_open_table);
    clear(@proc_time);
    clear(@sched_time);
    clear(@erlang_time);
    clear(@nif_time);
    clear(@eleveldb_time);
    clear(@hot_thread_time);
    clear(@compaction_time);
    clear(@bytes_read);
    clear(@bytes_written);
    clear(@logged_size);
    clear(@flushing_size);
    clear(@compaction_size);
}

END {
    printa("# %@u scheduler threads, %@u eleveldb threads, %@u hot threads\n",
           @num_sched, @num_eleveldb, @num_hot_thread);

    /* Clearing everything to avoid DTrace printing partial values */
    trunc(@num_queued);
    trunc(@proc_time);
    trunc(@sched_time);
    trunc(@erlang_time);
    trunc(@nif_time);
    trunc(@eleveldb_time);
    trunc(@hot_thread_time);
    trunc(@compaction_time);
    trunc(@bytes_read);
    trunc(@bytes_written);
    trunc(@logged_size);
    trunc(@flushing_size);
    trunc(@compaction_size);
}

