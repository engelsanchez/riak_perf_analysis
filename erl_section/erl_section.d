#pragma D option quiet
self uint64_t start;
int p[string];
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
/arg2 == 1/
{
    this->proc = copyinstr(arg0);
    self->start = vtimestamp;
}

/* If process running our section is unscheduled, remember where we were */
erlang$target:::process-unscheduled
/self->start/
{
    this->proc = copyinstr(arg0);
    p[this->proc] += vtimestamp - self->start;
    self->start = 0;
    @unscheduled = count();
}

/* If process that was interrupted in our section runs again */
erlang$target:::process-scheduled
/ p[copyinstr(arg0)] /
{
    this->proc = copyinstr(arg0);
    self->start = vtimestamp;
}

/* Process exits measured section */
erlang$target:::user_trace-i4s4
/arg2 == 2/
{
    this->proc = copyinstr(arg0);
    this->elapsed = p[this->proc] + vtimestamp - self->start;
    p[this->proc] = 0;
    @lmin = min(this->elapsed);
    @lmax = max(this->elapsed);
    @lavg = avg(this->elapsed);
    @lq = lquantize(this->elapsed, 0, EXPECTED_MAX, BUCKET_SIZE);
    @cnt = count();
    self->start = 0;
}

BEGIN {
    printf("%10s%10s%10s%10s%10s\n", "Count", "Desched", "Min", "Avg", "Max");
}

profile:::tick-1s
{
    printa("%@10u%@10u%@10u%@10u%@10u\n", @cnt, @unscheduled, @lmin, @lavg, @lmax);
    printa(@lq);
    trunc(@lmin); trunc(@lmax); trunc(@lavg);trunc(@lq);
    trunc(@cnt); trunc(@unscheduled);
}
