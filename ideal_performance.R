library(argparse)
library(gridExtra)
library(dplyr)
library(ggplot2)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

graphtitle <- args$graphtitle
ytitle <- args$ytitle

perf_csv <- paste(args$dir, "/", ytitle, ".csv", sep="")
perf <- read.csv(perf_csv)
perf$idu <- row.names(perf)
perf <- transform(perf, idu = as.numeric(idu))

output_prefix <- paste(args$dir, "/", ytitle, sep="")

DRAMs <- c("DDR3-2133L", "DDR3-2133L-noDRAMlat", "DDR3-2133L-unlimitBW", "DDR3-2133L-ideal-lat-BW", "GDDR5-7000", "GDDR5-7000-noDRAMlat", "GDDR5-7000-unlimitBW", "GDDR5-7000-ideal-lat-BW", "HBM-1000", "HBM-1000-noDRAMlat", "HBM-1000-unlimitBW", "HBM-1000-ideal-lat-BW", "HMC", "HMC-noDRAMlat", "HMC-unlimitBW", "HMC-ideal-lat-BW")

standardname <- c("DDR3", "DDR3", "DDR3", "DDR3", "GDDR5", "GDDR5", "GDDR5", "GDDR5", "HBM", "HBM", "HBM", "HBM", "HMC", "HMC", "HMC", "HMC")

DRAM_standardname_map <- data.frame(DRAMs, standardname)

upper_bound <- subset(perf, grepl("ideal-lat-BW", DRAM))
names(upper_bound)[names(upper_bound) == "value"] <- "upper_bound"
upper_bound <- merge(upper_bound, DRAM_standardname_map, by.x="DRAM", by.y="DRAMs")
upper_bound <- upper_bound[c("standardname", "upper_bound", "workload")]

lower_bound <- subset(perf, (DRAM == "DDR3-2133L") | (DRAM == "GDDR5-7000") | (DRAM == "HBM-1000") | (DRAM == "HMC"))
names(lower_bound)[names(lower_bound) == "value"] <- "lower_bound"
lower_bound <- merge(lower_bound, DRAM_standardname_map, by.x="DRAM", by.y="DRAMs")
lower_bound <- lower_bound[c("standardname", "lower_bound", "workload")]

lat_BW_tradeoff <- subset(perf, grepl("noDRAMlat", DRAM) | grepl("unlimitBW", DRAM))
lat_BW_tradeoff <- merge(lat_BW_tradeoff, DRAM_standardname_map, by.x="DRAM", by.y="DRAMs")
lat_BW_tradeoff <- merge(lat_BW_tradeoff, upper_bound, by.x=c("standardname", "workload"), by.y=c("standardname", "workload"))
lat_BW_tradeoff <- merge(lat_BW_tradeoff, lower_bound, by.x=c("standardname", "workload"), by.y=c("standardname", "workload"))

lat_BW_tradeoff <- arrange(lat_BW_tradeoff, idu)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7", "#0072B2")

print(lat_BW_tradeoff)

g <- ggplot(lat_BW_tradeoff, aes(width=.7, x=reorder(workload, -idu), y=value, fill=DRAM, colour=DRAM, group=standardname)) +
  xlab("workloads") +                               # x-axis label
  ylab(ytitle) +                             # y-axis label
  geom_bar(position=position_dodge(), stat="identity", colour="black", size=0.2) +
  scale_fill_manual(values=cbPalette) +
  geom_errorbar(aes(width=.7, y=upper_bound, ymax=upper_bound, ymin=upper_bound), colour = "#000000", position=position_dodge()) +
  geom_errorbar(aes(width=.7, y=lower_bound, ymax=lower_bound, ymin=lower_bound), colour = "#000000", position=position_dodge()) +
  theme(axis.text.x=element_text(angle=60, vjust=0.5),
      legend.text=element_text(size=8),
      plot.margin=unit(c(1,0,1,1), "line"))

ggsave(file=paste(output_prefix, ".pdf", sep=""), g)
ggsave(file=paste(output_prefix, ".png", sep=""), g)

