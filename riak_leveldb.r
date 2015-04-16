#!/usr/bin/env Rscript

arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))

# Load all the necessary packages, installing missing ones when necessary
packages.to.install <- c("reshape2", "plyr", "grid", "getopt", "proto", "ggplot2")

for(p in packages.to.install)
  {
        print(p)
        if (suppressWarnings(!require(p, character.only = TRUE))) {
            install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
            library(p, character.only=TRUE)
        }
  }

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'infile',  'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'title', 't',  2, "character"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1280 }
if (is.null(opt$height))  { opt$height  = 960 }
if (is.null(opt$infile))   { opt$infile  = "riak_leveldb.csv"}
if (is.null(opt$outfile)) { opt$outfile = sub("\\.[^.]*$", ".png", opt$infile) }
if (is.null(opt$title)) { opt$title = "Throughput" }

png(file = opt$outfile, width = opt$width, height = opt$height)

# Load data, massage it into CPU and I/O graphs

d <- read.csv(opt$infile, colClasses=rep("numeric", 17), comment.char = "#")
d$time = (d$time_ms - d$time_ms[1]) / 1000.0
d$only_erlang_time = d$erlang_time - d$nif_time
d$only_sched_time = d$sched_time - d$only_erlang_time
d$all_eleveldb_time = d$eleveldb_time + d$hot_thread_time
d$other_time = d$proc_time - d$all_eleveldb_time

# time_ms,num_writes,num_reads,num_queued,num_open_table,
# riak_time,sched_time,erlang_time,nif_time,eleveldb_time,
# hot_thread_time,compaction_time,
# reads,writes,wal_writes,flush_writes,compaction_writes

###############################################################################
# Operations breakdown

ops_wide <- data.frame(time = d$time,
                       reads = d$num_reads,
                       writes = d$num_writes)

ops <- melt(ops_wide, id.vars = "time", variable.name = "type",
            value.name = "ops")

ops_plot <- ggplot(ops, aes(x = time, y = ops, fill = type)) +
            labs(title = "# Ops", x = "Elapsed Secs", y = "# Ops") +
            geom_area()

############################################################################
#  CPU graph
cpu_wide = data.frame(time = d$time,
                      nif = d$nif_time / 10.0,
                      erlang = d$only_erlang_time / 10.0,
                      sched = d$only_sched_time / 10.0,
                      other = d$other_time / 10.0,
                      eleveldb_threads = d$eleveldb_time / 10.0,
                      compaction = d$compaction_time / 10.0,
                      other_hot = (d$hot_thread_time - d$compaction_time) / 10.0)

cpu = melt(cpu_wide, id.vars = "time", variable.name = "work_type",
           value.name = "cpu")

cpu_plot <- ggplot(cpu, aes(x = time, y = cpu, fill = work_type)) +
                   labs(title = "CPU usage", x = "Elapsed Secs", y = "CPU %") +
                   geom_area()

###############################################################################
# Disk I/O plot
io_wide <- data.frame(time = d$time,
                     writes = d$writes / 1024.0,
                     reads = d$reads / 1024.0)

io <- melt(io_wide, id.vars = "time", variable.name = "io_type",
           value.name = "io")

io_plot <- ggplot(io, aes(x = time, y = io, fill = io_type)) +
            labs(title = "Disk I/O", x = "Elapsed Secs", y = "MB") + geom_area()


###############################################################################
# App read/writes breakdown
rw_wide <- data.frame(time = d$time,
                     log = d$wal_writes / 1024.0,
                     flush = d$flush_writes / 1024.0,
                     compaction = d$compaction_writes / 1024.0)

rw <- melt(rw_wide, id.vars = "time", variable.name = "io_type",
           value.name = "io")

rw_plot <- ggplot(rw, aes(x = time, y = io, fill = io_type)) +
           labs(title = "App I/O", x = "Elapsed Secs", y = "MB") + geom_area()

grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(ops_plot, vp = vplayout(1,1))
print(cpu_plot, vp = vplayout(2,1))
print(io_plot, vp = vplayout(3,1))
print(rw_plot, vp = vplayout(4,1))

dev.off()
