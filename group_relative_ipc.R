library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--dir", required=TRUE, help="input dir that contains the csv file and also the output dir for plots")
parser$add_argument("--ytitle", required=TRUE, help="input yaxis title")

args <- parser$parse_args()

print(args$dir)

csv_path <- paste(args$dir, "/", args$ytitle, ".csv", sep="")
output_prefix <- paste(args$dir, "/", args$ytitle, sep="")

print(csv_path)

library(dplyr)
library(ggplot2)
library(gridExtra)

for () {
# substitute part of str
#  exp <- read.csv(new_csv_path)
#  rbind(exp)
}

# average
# barplot
# save pics
