library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")
parser$add_argument("--workload-list", required=TRUE, nargs="+", help="workload list")
parser$add_argument("--filename-list", required=TRUE, nargs="+", help="filename list")

args <- parser$parse_args()

graphtitle <- args$graphtitle
ytitle <- args$ytitle

workload_list <- args$workload_list
filename_list <- args$filename_list 

total_dfs <- data.frame(DRAM=character(), value=double(), workload=character())

for (i in 1:length(workload_list)) {
  print(workload_list[i])
  csv <- paste(args$dir, "/", filename_list[i], "/", ytitle, ".csv", sep="")
  df <- read.csv(csv)

  df <- aggregate(value~DRAM, data=df, mean)
  df$workload <- rep(workload_list[i], nrow(df))
  total_dfs <- rbind(total_dfs, df)
}

total_dfs$DRAM <- factor(total_dfs$DRAM, levels = c("DDR3-800D", "DDR3-1066E", "DDR3-1333G", "DDR3-1600H", "DDR3-1866K", "DDR3-2133L"))

output_prefix <- paste(args$dir, "/", graphtitle, "_", "compare_DDR3", sep="")

library(ggplot2)

p1 <- ggplot(total_dfs, aes(x=DRAM, y=value, fill = workload, group=workload, color=workload)) +
  geom_line() +
#  geom_point(aes(shape=workload), size=1) +
  geom_point(size=1) +
  scale_shape_manual(values=seq(0, 15)) +
#  ylim(lowerbound, upperbound) +
#  ylim(1,2.5) +
  xlab("DDR3 generations") +
  ylab(ytitle) +
  ggtitle(graphtitle) +
  theme(axis.text.x=element_text(angle=60, vjust=0.5),
      legend.text=element_text(size=8))

ggsave(file=paste(output_prefix, ".png", sep=""), p1)
ggsave(file=paste(output_prefix, ".pdf", sep=""), p1)
