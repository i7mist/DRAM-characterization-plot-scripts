# Read the data from a file. The data is about a study on the effect of vitamin C on tooth growth in guinea pigs. There are 4 columns: `X`, `len`, `supp` and `dose`. This example visually presents tooth growth progress depending on the delivery method: `OJ` (orange juice) or `VC` (ascorbic acid)
library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")
parser$add_argument("--trace-type", required=TRUE, help="type of trace (CPU or DRAM)")

args <- parser$parse_args()

allbandwidth_utilization_csv <- paste(args$dir, "/bandwidth_utilization.csv", sep="")
allbandwidth_csv <- paste(args$dir, "/bandwidth.csv", sep="")
if (args$trace_type == "CPU") {
  ref_memory_intensity_csv <- paste(args$dir, "/MPKI.csv", sep="")
} else if (args$trace_type == "DRAM") {
  ref_memory_intensity_csv <- paste(args$dir, "/average_outstanding_requests.csv", sep="")
}
print(args$graphtitle)

all_bandwidth_utilization <- read.csv(allbandwidth_utilization_csv)
all_bandwidth <- read.csv(allbandwidth_csv)
print(head(all_bandwidth))
ref_memory_intensity <- read.csv(ref_memory_intensity_csv)

all_bandwidth_utilization$idu <- row.names(all_bandwidth_utilization)
all_bandwidth$idu <- row.names(all_bandwidth)
ref_memory_intensity$idu <- row.names(ref_memory_intensity)

all_bandwidth_utilization <- transform(all_bandwidth_utilization, idu = as.numeric(idu))
all_bandwidth <- transform(all_bandwidth, idu = as.numeric(idu))
ref_memory_intensity <- transform(ref_memory_intensity, idu = as.numeric(idu))

print(head(ref_memory_intensity))
print(head(all_bandwidth_utilization))
#print(head(all_bandwidth))

graphtitle <- args$graphtitle
output_prefix <- paste(args$dir, "/bandwidth_utilization", sep="")

# Summarise the data with `ddply` from the **plyr** package, group by `supp` and `dose` and compute the mean of the corresponding values under the `len` column. This creates a new data frame whose columns are: `supp`, `dose` and `length`. 

#library(plyr)
library(dplyr)

all_bandwidth <- filter(all_bandwidth, DRAM == "DDR3-2133L" | DRAM == "DDR4-2400R" | DRAM == "GDDR5-7000" | DRAM == "HBM-1000" |  DRAM == "LPDDR3-2133" | DRAM == "LPDDR4-3200" | DRAM == "WideIO-266" | DRAM == "WideIO2-1067")
all_bandwidth_utilization <- filter(all_bandwidth_utilization, DRAM == "DDR3-2133L" | DRAM == "DDR4-2400R" | DRAM == "GDDR5-7000" | DRAM == "HBM-1000" |  DRAM == "LPDDR3-2133" | DRAM == "LPDDR4-3200" | DRAM == "WideIO-266" | DRAM == "WideIO2-1067")
ref_memory_intensity <- filter(ref_memory_intensity, DRAM == "DDR3-2133L")

# The next block of code renders the plot, the columns `dose` and `length` on the x, y axes and `supp` is used to group the data and [colour](https://www.getdatajoy.com/learn/Colour_Names:_Complete_List) each line.
cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

library(ggplot2)
p1<-ggplot(all_bandwidth, aes(x=reorder(workload, idu), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=cbPalette) +
    scale_shape_manual(values=1:8) +
    scale_colour_manual(values=cbPalette) +
    geom_line() +
    geom_point(aes(shape=DRAM), size=1) +
    xlab("") +                               # x-axis label
    ylab("bandwidth (Bps)") +                             # y-axis label
    ggtitle(graphtitle)  +                          # title
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    legend.text=element_text(size=8),
    plot.margin=unit(c(1,0,0,1), "lines"))


if (args$trace_type == "CPU") {
p2<-ggplot(ref_memory_intensity, aes(x=reorder(workload, idu), y=ref_MPKI)) +
    geom_bar(stat='identity') +
    xlab("") +
    ylab("average bank parallelism") +
    ylim(0,200) +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    plot.margin=unit(c(1,2,0,0), "lines"))
} else if (args$trace_type == "DRAM") {
p2<-ggplot(ref_memory_intensity, aes(x=reorder(workload, idu), y=ref_memory_intensity)) +
    geom_bar(stat='identity') +
    xlab("") +
    ylab("average bank parallelism") +
    ylim(0,50) +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    plot.margin=unit(c(1,2,0,0), "line"))
}

p3<-ggplot(all_bandwidth_utilization, aes(x=reorder(workload, idu), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=cbPalette) +
    scale_shape_manual(values=1:8) +
    scale_colour_manual(values=cbPalette) +
    geom_line() +
    geom_point(aes(shape=DRAM), size=1) +
    xlab("") +                               # x-axis label
    ylab("bandwidth_utilization") +                             # y-axis label
    ylim(0, 1) +
    ggtitle(graphtitle)  +                          # title
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    legend.text=element_text(size=8),
    plot.margin=unit(c(0,0,1,1), "lines"))

library(gridExtra)
library(grid)
grid.arrange(p1,p2,p3,ncol=2,
widths = c(unit(0.6, "npc"), unit(0.4, "npc")))

g <- arrangeGrob(p1,p2,p3,ncol=2,
widths = c(unit(0.6, "npc"), unit(0.4, "npc"))) #generates g
 ggsave(file=paste(output_prefix,".png", sep=""), g) #saves g
 ggsave(file=paste(output_prefix,".pdf", sep=""), g) #saves g

warning()
