#pragma D option quiet
self uint64_t start;

struct timeinfo {
    uint64_t start;
    uint64_t duration;
    int section;
};

struct timeinfo p[string];
/* Change this to the maximum expected elapsed time for the section.
 * Defaults to 200 micro-seconds
 */
inline int EXPECTED_MAX = 200000;
/* Change to the desired histogram bucket size. Here one bucket 
 * per 5 micro-seconds
 */
inline int BUCKET_SIZE = 5000;

/* Process entered measured section. */
erlang$target:::user_trace-i4s4
/arg2 % 2 == 1 && p[copyinstr(arg0)].section == 0/ /* No nested tracing */
{
    this->proc = copyinstr(arg0);
    p[this->proc].start = vtimestamp;
    p[this->proc].duration = 0;
    p[this->proc].section = arg2;
}

/* If process running our section is unscheduled, remember where we were */
erlang$target:::process-unscheduled
/p[copyinstr(arg0)].section != 0/
{
    this->proc = copyinstr(arg0);
    p[this->proc].duration += vtimestamp - p[this->proc].start;
    p[this->proc].start = 0;
    @unscheduled[this->proc] = count();
}

/* If process that was interrupted in our section runs again */
erlang$target:::process-scheduled
/ p[copyinstr(arg0)].section != 0/
{
    this->proc = copyinstr(arg0);
    p[this->proc].start = vtimestamp;
}

/* Process exits measured section */
erlang$target:::user_trace-i4s4
/arg2 % 2 == 0 && p[copyinstr(arg0)].section != 0/
{
    this->proc = copyinstr(arg0);
    this->section = arg2;
    this->elapsed = p[this->proc].duration + vtimestamp - p[this->proc].start;
    @cnt[this->proc, this->section] = count();
    @lmin[this->proc, this->section] = min(this->elapsed);
    @lmax[this->proc, this->section] = max(this->elapsed);
    @lavg[this->proc, this->section] = avg(this->elapsed);
    @lq[this->section] = lquantize(this->elapsed, 0, EXPECTED_MAX, BUCKET_SIZE);
    p[this->proc].duration = 0;
    p[this->proc].start = 0;
    p[this->proc].section = 0;
}

BEGIN {
    printf("%s%10s%10s%10s%10s\n", "PID", "Count", "Min", "Avg", "Max");
}

profile:::tick-1s
{
    printa(@cnt, @lmin, @lavg, @lmax);
    printa(@unscheduled);
    printa(@lq);
    trunc(@lmin); trunc(@lmax); trunc(@lavg);trunc(@lq);
    trunc(@cnt); trunc(@unscheduled);
}
