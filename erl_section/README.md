Compile CPU utilization for a specific section of code.
It only counts the time spent while executing the erlang code, no matter how 
many times the process is scheduled and re-scheduled.

Make sure Erlang is compiled with DTrace support:

* http://www.erlang.org/doc/apps/runtime_tools/DTRACE.html


Place calls to dyntrace:p() around the section that you want to profile:

   % Doing some shitz
   dyntrace:p(1),
   do_some_work(),
   and_then_some_more(),
   whatevs_im_done(),
   dyntrace:p(2),


Collect the pid of the Erlang VM and run dtrace like this:

sudo -i dtrace -p $ERLANG_PID -s ~/path_to_script/erl_section.d

Notice the -i flag to sudo: without it, the END clause in dtrace refuses to
fire. That is not a problem in this script, but it's better to get used to it.
