library(argparse)
library(gridExtra)
library(dplyr)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

graphtitle <- args$graphtitle
ytitle <- args$ytitle

tradeoffs_norm_perf_csv <- paste(args$dir, "/", ytitle, ".csv", sep="")
tradeoffs_norm_perf <- read.csv(tradeoffs_norm_perf_csv)

ideal_DRAM_perf_csv <- gsub("/row_bank_tradeoff/", "/ideal_DRAM/", tradeoffs_norm_perf_csv)
ideal_DRAM_perf <- read.csv(ideal_DRAM_perf_csv)

output_prefix <- paste(args$dir, "/", ytitle, sep="")

workload_list <- unique(tradeoffs_norm_perf$workload)
workload_num <- length(workload_list)

avg_tradeoffs_norm_perf <- aggregate(.~DRAM, data=tradeoffs_norm_perf, mean)

DRAMs <- c("DDR3-2133L", "DDR3-2133L-bank16", "DDR3-2133L-bank32", "DDR3-2133L-bank64", "DDR3-2133L-bank128", "DDR3-2133L-bank256", "GDDR5-7000", "GDDR5-7000-bank32", "GDDR5-7000-bank64", "GDDR5-7000-bank128", "GDDR5-7000-bank256", "GDDR5-7000-bank512", "HBM-1000", "HBM-1000-bank32", "HBM-1000-bank64", "HBM-1000-bank128", "HBM-1000-bank256", "HBM-1000-bank512", "HMC", "HMC-bank16", "HMC-32", "HMC-bank64", "HMC-bank128", "HMC-bank256", "HMC-RoBaCoVa", "HMC-RoBaCoVa-bank16", "HMC-RoBaCoVa-bank32", "HMC-RoBaCoVa-bank64", "HMC-RoBaCoVa-bank128", "HMC-RoBaCoVa-bank256")

groupname <- c(1, 2, 4, 8, 16, 32, 1, 2, 4, 8, 16, 32, 1, 2, 4, 8, 16, 32, 1, 2, 4, 8, 16, 32, 1, 2, 4, 8, 16, 32)

standardname <- c("DDR3", "DDR3", "DDR3", "DDR3", "DDR3", "DDR3", "GDDR5", "GDDR5", "GDDR5", "GDDR5", "GDDR5", "GDDR5", "HBM", "HBM", "HBM", "HBM", "HBM", "HBM", "HMC", "HMC", "HMC", "HMC", "HMC", "HMC", "HMC-RoBaCoVa", "HMC-RoBaCoVa", "HMC-RoBaCoVa", "HMC-RoBaCoVa", "HMC-RoBaCoVa", "HMC-RoBaCoVa")

DRAM_groupname_map <- data.frame(DRAMs, groupname, standardname)

avg_tradeoffs_norm_perf <- merge(avg_tradeoffs_norm_perf, DRAM_groupname_map, by.x = "DRAM", by.y = "DRAMs")


library(ggplot2)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

newpage_cnt <- 0

print(avg_tradeoffs_norm_perf)

g <- ggplot(avg_tradeoffs_norm_perf, aes(x=groupname, y=value, fill=standardname, group=standardname, color=standardname)) +
     scale_colour_manual(values=cbPalette) +
     geom_line() +
     geom_point(aes(shape=standardname), size=1) +
     xlab("") +
     ylab(paste("average", " ", ytitle,sep="")) +                             # y-axis label
     ggtitle(graphtitle) +
     theme(axis.text.x=element_text(angle=60, vjust=0.5),
         legend.text=element_text(size=8),
         plot.margin=unit(c(1,0,1,1), "line"))

ggsave(file=paste(output_prefix, "_avg", ".pdf", sep=""), g)
ggsave(file=paste(output_prefix, "_avg", ".png", sep=""), g)

warnings()
