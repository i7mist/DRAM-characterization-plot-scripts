# Read the data from a file. The data is about a study on the effect of vitamin C on tooth growth in guinea pigs. There are 4 columns: `X`, `len`, `supp` and `dose`. This example visually presents tooth growth progress depending on the delivery method: `OJ` (orange juice) or `VC` (ascorbic acid)
library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

allipc_csv <- paste(args$dir, "/", args$ytitle, ".csv", sep="")
refMPKI_csv <- paste(args$dir, "/MPKI.csv", sep="")
print(args$ytitle)

all_ipc <- read.csv(allipc_csv)
all_ipc$idu <- row.names(all_ipc)
all_ipc <- transform(all_ipc, idu = as.numeric(idu))
ref_MPKI <- read.csv(refMPKI_csv)
ref_MPKI$idu <- row.names(ref_MPKI)
ref_MPKI <- transform(ref_MPKI, idu = as.numeric(idu))
ytitle <- args$ytitle
output_prefix <- paste(args$dir, "/", ytitle,  sep="")

# Summarise the data with `ddply` from the **plyr** package, group by `supp` and `dose` and compute the mean of the corresponding values under the `len` column. This creates a new data frame whose columns are: `supp`, `dose` and `length`. 

#library(plyr)
library(dplyr)

ref_MPKI <- filter(ref_MPKI, DRAM == "DDR3-2133L")
print(head(all_ipc))

# The next block of code renders the plot, the columns `dose` and `length` on the x, y axes and `supp` is used to group the data and [colour](https://www.getdatajoy.com/learn/Colour_Names:_Complete_List) each line.
#cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FF79A7")
cbPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
#cbPalette <- c('#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

library(ggplot2)
theme_set(theme_gray(base_size = 15))
g<-ggplot(all_ipc, aes(x=reorder(workload, idu), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    scale_shape_manual(values = 0:10) +
    geom_line(size=1) +
    geom_point(aes(shape=DRAM), size = 2) +
    xlab("") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    theme(axis.text.x=element_text(angle=90, vjust=0.5),
     panel.background = element_blank(),
#    axis.text.y=element_text(size=14),
#    axis.title=element_text(size=14),
#    legend.text=element_text(size=14),
#    legend.margin=unit(0, "cm"),
    plot.margin=unit(c(0.5, 0.5, 0.2, 0.2), "cm"))

 ggsave(file=paste(output_prefix,".png", sep=""), g) #saves g
 ggsave(file=paste(output_prefix,".pdf", sep=""), g) #saves g

warnings()
