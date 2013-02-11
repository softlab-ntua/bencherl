#!/usr/bin/env Rscript 

# Parse the --file= argument out of command line args and
# determine where base directory is so that we can source
# our common sub-routines
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
dir0 <- dirname(arg0)
source(file.path(dir0, "common.r"))

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'indir',   'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'ylabel1stgraph', 'Y',  2, "character"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1024 }
if (is.null(opt$height))  { opt$height  = 768 }
if (is.null(opt$indir))   { opt$indir  = "current"}
if (is.null(opt$outfile)) { opt$outfile = file.path(opt$indir, "summary.png") }
if (is.null(opt$ylabel1stgraph)) { opt$ylabel1stgraph = "Operations/sec" }

# Load the benchmark data, passing the time-index range we're interested in
b = load_benchmark(opt$indir, opt$tstart, opt$tend)

# If there is no actual data available, bail
if (nrow(b$latencies) == 0)
{
  stop("No latency information available to analyze in ", opt$indir)
}

png(file = opt$outfile, width = opt$width, height = opt$height)

# First plot req/sec from summary
plot1 <- qplot(elapsed, successful / window, data = b$summary,
                geom = c("line", "point"),se = FALSE,span = 0.5,
                xlab = "Elapsed Secs", ylab = opt$ylabel1stgraph, 
                main = "Throughput") 
# Setup common elements of the number plots
number_plot <- ggplot(b$latencies, aes(x = elapsed)) +
                   facet_grid(. ~ op) +
                   labs(x = "Elapsed Secs", y = opt$ylabel1stgraph)
# Plot number
plot2 <- number_plot +
	    geom_point(aes(y = n / window)) +geom_line(aes(y = n / window))

 plot3 <- qplot(elapsed, failed , data = b$summary,
                geom = c("line", "point"),
                xlab = "Elapsed Secs", ylab = "Fails")

# Setup common elements of the latency plots
latency_plot <- ggplot(b$latencies, aes(x = elapsed)) +
                   facet_grid(. ~ op) +
                   labs(x = "Elapsed Secs", y = "Latency (ms)")

# Plot 99 and 99.9th percentiles
plot4 <- latency_plot +
            geom_point(aes(y = mean)) +geom_line(aes(y = mean))




grid.newpage()

pushViewport(viewport(layout = grid.layout(4, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(plot1, vp = vplayout(1,1))
print(plot2, vp = vplayout(2,1))
print(plot3, vp = vplayout(3,1))
print(plot4, vp = vplayout(4,1))

dev.off()
