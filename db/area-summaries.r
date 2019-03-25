library(data.table)
library(jsonlite)
source('functions.r')

inputfile <- 'data/REARdb.rda'
outputfile <- 'dev/area-summaries.json'

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 2) {
    inputfile <- args[1]
    outputfile <- args[length(args)]
}

load(inputfile)


