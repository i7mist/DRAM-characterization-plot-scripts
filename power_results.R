library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--dir", required=TRUE, help="input dir that contains the csv file and also the output dir for plots")
parser$add_argument("--ytitle", required=TRUE, help="input yaxis title")

args <- parser$parse_args()

print(args$dir)

output_prefix <- paste(args$dir, "/", "avg_", args$ytitle, sep="")

workload_path <- args$workload_path
print(workload_path)
#print(paste("workload_path ", args$workload_path, sep=""))
workload_name <- args$workload_name
print(workload_name)
#print(paste("workload_name ", args$workload_name, sep=""))

workload_num <- length(workload_name)

cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FF79A7")

library(dplyr)
library(ggplot2)
library(gridExtra)

csv_path <- paste(args$dir, "/", args$ytitle, ".csv", sep="")
total_dfs <- read.csv(csv_path)

g <- ggplot(total_dfs, aes(x=DRAM, y=value, fill=statistics)) +
  geom_bar(stat='identity', position="stack", colour=NA) +
  scale_fill_manual(values=cbPalette) +
#  facet_wrap(~ workload) +
  ylab(args$ytitle) +
  xlab('') +
  #ylim(0.5, 2) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
  legend.text=element_text(size=8),
  plot.margin=unit(c(1,0,1,1), "lines")) #rep(unit(0,"cm"),each=4))
# barplot

ggsave(file=paste(output_prefix, ".png", sep=""), g)
ggsave(file=paste(output_prefix, ".pdf", sep=""), g)
