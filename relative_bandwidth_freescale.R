# Read the data from a file. The data is about a study on the effect of vitamin C on tooth growth in guinea pigs. There are 4 columns: `X`, `len`, `supp` and `dose`. This example visually presents tooth growth progress depending on the delivery method: `OJ` (orange juice) or `VC` (ascorbic acid)
library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

ref_memory_intensity_csv <- paste(args$dir, "/average_outstanding_requests.csv", sep="")
bandwidth_csv <- paste(args$dir, "/", args$ytitle, ".csv", sep="")
print(bandwidth_csv)
print(ref_memory_intensity_csv)
print(args$ytitle)
print(args$graphtitle)

normalized_bandwidth <- read.csv(bandwidth_csv)
normalized_bandwidth$idu <- row.names(normalized_bandwidth)
normalized_bandwidth <- transform(normalized_bandwidth, idu = as.numeric(idu))
sapply(normalized_bandwidth, mode)
sapply(normalized_bandwidth, class)
ref_memory_intensity <- read.csv(ref_memory_intensity_csv)
ref_memory_intensity$idu <- row.names(ref_memory_intensity)
ref_memory_intensity <- transform(ref_memory_intensity, idu = as.numeric(idu))
ytitle <- args$ytitle
graphtitle <- args$graphtitle
output_prefix <- paste(args$dir, "/", args$ytitle, sep="")
print(output_prefix)

# Summarise the data with `ddply` from the **plyr** package, group by `supp` and `dose` and compute the mean of the corresponding values under the `len` column. This creates a new data frame whose columns are: `supp`, `dose` and `length`. 

#library(plyr)
library(dplyr)
print(head(ref_memory_intensity))

normalized_bandwidth <- filter(normalized_bandwidth, DRAM == "DDR3-2133L" | DRAM == "DDR4-2400R" | DRAM == "GDDR5-7000" | DRAM == "HBM-1000" |  DRAM == "LPDDR3-2133" | DRAM == "LPDDR4-3200" | DRAM == "WideIO-266" | DRAM == "WideIO2-1067")
ref_memory_intensity <- filter(ref_memory_intensity, DRAM == "DDR3-2133L")
print(head(ref_memory_intensity))

# The next block of code renders the plot, the columns `dose` and `length` on the x, y axes and `supp` is used to group the data and [colour](https://www.getdatajoy.com/learn/Colour_Names:_Complete_List) each line.
cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

library(ggplot2)
p1<-ggplot(normalized_bandwidth, aes(x=reorder(workload, idu), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=cbPalette) +
    scale_shape_manual(values=1:8) +
    scale_colour_manual(values=cbPalette) +
    geom_line() +
    geom_point(aes(shape=DRAM), size=1) +
    xlab("") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    ggtitle(graphtitle)  +                          # title
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    legend.text=element_text(size=8),
    plot.margin=unit(c(5,0,5,2), "lines"))


p2<-ggplot(ref_memory_intensity, aes(x=reorder(workload, idu), y=ref_memory_intensity)) +
    geom_bar(stat='identity') +
    xlab("") +
    ylab("average bank parallelism") +
    ylim(0,50) +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    plot.margin=unit(c(5,2,5,0), "lines"))

library(gridExtra)
library(grid)
grid.arrange(p1,p2,ncol=2, nrow=1,
widths = c(unit(0.6, "npc"), unit(0.4, "npc")))

 g <- arrangeGrob(p1,p2,ncol=2, nrow=1,
widths = c(unit(0.6, "npc"), unit(0.4, "npc")), heights = c(unit(0.5, "npc"))) #generates g
 ggsave(file=paste(output_prefix,".png", sep=""), g) #saves g
 ggsave(file=paste(output_prefix,".pdf", sep=""), g) #saves g

 warnings()
