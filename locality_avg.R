# The data is read from a csv file, grouped in columns: Cultivar, Date, Weight, sd, n, se

library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--dir", required=TRUE, help="input dir that contains the csv file and also the output dir for plots")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")

args <- parser$parse_args()

print(args$dir)

csv_path <- paste(args$dir, "/locality.csv", sep="")
output_prefix <- paste(args$dir, "/locality", sep="")

print(csv_path)

locality_exp <- read.csv(csv_path)

# ---
# Stacked Bar Graph
# -----------------

library(dplyr)
#library(plyr)
library(ggplot2)
library(gridExtra)

print(head(locality_exp))

locality_avg_exp <- aggregate(value~statistics+DRAM, data=locality_exp, mean)
ord <- c("row_hit_rate", "row_miss_rate", "row_conflict_rate")
locality_avg_exp$statistics <- factor(locality_avg_exp$statistics, levels = ord)
# reorder rows by specified stats order, for later stacking order
locality_avg_exp <- locality_avg_exp[order(locality_avg_exp$statistics),]
# recover the original order
locality_avg_exp$statistics <- factor(locality_avg_exp$statistics, levels = c("row_conflict_rate", "row_hit_rate", "row_miss_rate"))
print(head(locality_avg_exp))

g<-ggplot(locality_avg_exp, aes(x=DRAM, y=value, fill=statistics)) +
    geom_bar(stat="identity", colour='black') +     # contour colour
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_brewer(breaks=c("row_conflict_rate","row_miss_rate","row_hit_rate"), palette="Paired", guide=FALSE) +              # colour palette
    ylim(0,1.01) +
    ggtitle(args$ytitle) +
    theme_bw() +
    theme(plot.title=element_text(size=8),
        axis.text.x=element_text(angle=40, size=7, vjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(1, 2, 0, 1), "line"))

ggsave(file=paste(output_prefix, "_avg", ".pdf", sep=""), g)
ggsave(file=paste(output_prefix, "_avg", ".png", sep=""), g)
