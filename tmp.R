# Read the data from a file. The data is about a study on the effect of vitamin C on tooth growth in guinea pigs. There are 4 columns: `X`, `len`, `supp` and `dose`. This example visually presents tooth growth progress depending on the delivery method: `OJ` (orange juice) or `VC` (ascorbic acid)
library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

hr_dir <- paste(args$dir, "hr", sep="")
lr_dir <- paste(args$dir, "lr", sep="")
hr_refMPKI_csv <- paste(hr_dir, "/MPKI.csv", sep="")
hripc_csv <- paste(hr_dir, "/", args$ytitle, ".csv", sep="")
lr_refMPKI_csv <- paste(lr_dir, "/MPKI.csv", sep="")
lripc_csv <- paste(lr_dir, "/", args$ytitle, ".csv", sep="")

hr_normalized_ipc <- read.csv(hripc_csv)
hr_normalized_ipc$idu <- row.names(hr_normalized_ipc)
hr_normalized_ipc <- transform(hr_normalized_ipc, idu = as.numeric(idu))
lr_normalized_ipc <- read.csv(lripc_csv)
lr_normalized_ipc$idu <- row.names(lr_normalized_ipc)
lr_normalized_ipc <- transform(lr_normalized_ipc, idu = as.numeric(idu))
hr_ref_MPKI <- read.csv(hr_refMPKI_csv)
hr_ref_MPKI$idu <- row.names(hr_ref_MPKI)
hr_ref_MPKI <- transform(hr_ref_MPKI, idu = as.numeric(idu))
lr_ref_MPKI <- read.csv(lr_refMPKI_csv)
lr_ref_MPKI$idu <- row.names(lr_ref_MPKI)
lr_ref_MPKI <- transform(lr_ref_MPKI, idu = as.numeric(idu))
ytitle <- args$ytitle
graphtitle <- args$graphtitle
output_prefix <- paste(args$dir, "/", args$ytitle, sep="")
print(output_prefix)

print(head(hr_normalized_ipc))
print(head(lr_normalized_ipc))
print(head(hr_ref_MPKI))
print(head(lr_ref_MPKI))

# Summarise the data with `ddply` from the **plyr** package, group by `supp` and `dose` and compute the mean of the corresponding values under the `len` column. This creates a new data frame whose columns are: `supp`, `dose` and `length`. 

#library(plyr)
library(dplyr)

hr_ref_MPKI <- filter(hr_ref_MPKI, DRAM == "DDR3-2133L")
lr_ref_MPKI <- filter(lr_ref_MPKI, DRAM == "DDR3-2133L")

# The next block of code renders the plot, the columns `dose` and `length` on the x, y axes and `supp` is used to group the data and [colour](https://www.getdatajoy.com/learn/Colour_Names:_Complete_List) each line.

library(ggplot2)
p1<-ggplot(hr_normalized_ipc, aes(x=reorder(workload, ref_MPKI), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=c("#000000", "#009E73")) +
    scale_colour_manual(values=c("#000000", "#009E73")) +
    geom_line() +
    geom_point(aes(shape=DRAM),size=1) +
    xlab("workloads") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    ggtitle(paste(graphtitle, "(high locality)", sep=""))  +                          # title
    theme(axis.text.x=element_text(angle=30, vjust=0.5),
    legend.text=element_text(size=8),
    plot.margin=unit(c(1,0,1,1), "lines")) #rep(unit(0,"cm"),each=4))


p2<-ggplot(hr_ref_MPKI, aes(x=reorder(workload, value), y=ref_MPKI)) +
    geom_bar(stat='identity') +
    xlab("workloads") +
    ylab("MPKI") +
    ylim(0,200) +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    plot.margin=unit(c(1,1,1,0), "lines")) #rep(unit(0,"cm"),each=4))

p3<-ggplot(lr_normalized_ipc, aes(x=reorder(workload, ref_MPKI), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=c("#000000", "#009E73")) +
    scale_colour_manual(values=c("#000000", "#009E73")) +
    geom_line() +
    geom_point(aes(shape=DRAM),size=1) +
    xlab("workloads") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    ggtitle(paste(graphtitle, "(low locality)", sep = ""))  +                          # title
    theme(axis.text.x=element_text(angle=30, vjust=0.5),
    legend.text=element_text(size=8),
    plot.margin=unit(c(1,0,1,1), "lines")) #rep(unit(0,"cm"),each=4))


p4<-ggplot(lr_ref_MPKI, aes(x=reorder(workload, value), y=ref_MPKI)) +
    geom_bar(stat='identity') +
    xlab("workloads") +
    ylab("MPKI") +
    ylim(0,200) +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8),
    plot.margin=unit(c(1,1,1,0), "lines")) #rep(unit(0,"cm"),each=4))
library(gridExtra)
library(grid)

grid.arrange(p1,p2,p3,p4,ncol=2, nrow=2,
widths = c(unit(0.6, "npc"), unit(0.4, "npc")))

 g <- arrangeGrob(p1,p2,p3,p4,ncol=2, nrow=2,
widths = c(unit(0.6, "npc"), unit(0.4, "npc"))) #generates g
 ggsave(file=paste(output_prefix,".png", sep=""), g) #saves g
 ggsave(file=paste(output_prefix,".pdf", sep=""), g) #saves g

