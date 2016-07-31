library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

graphtitle <- args$graphtitle
ytitle <- args$ytitle

normalized_perf_csv <- paste(args$dir, "/", ytitle, ".csv", sep="")
normalized_perf <- read.csv(normalized_perf_csv)
normalized_perf$idu <- row.names(normalized_perf)
normalized_perf <- transform(normalized_perf, idu = as.numeric(idu))

output_prefix <- paste(args$dir, "/", ytitle, sep="")

print(head(normalized_perf))

library(ggplot2)

p1 <- ggplot(normalized_perf, aes(x=reorder(DRAM, idu), y=value, fill = workload, group=workload, color=workload)) +
  geom_line() +
#  geom_point(aes(shape=workload), size=1) +
  geom_point(size=1) +
  scale_shape_manual(values=seq(0, 15)) +
#  ylim(lowerbound, upperbound) +
#  ylim(1,2.5) +
  xlab("DDR3 family") +
  ylab(ytitle) +
  ggtitle(graphtitle) +
  theme(axis.text.x=element_text(angle=60, vjust=0.5),
      legend.text=element_text(size=8))

ggsave(file=paste(output_prefix, ".png", sep=""), p1)
ggsave(file=paste(output_prefix, ".pdf", sep=""), p1)
