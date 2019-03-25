library(data.table)
library(jsonlite)
source('functions.r')

dbfile <- 'data/REARdb.rda'
areasjson <- 'dev/areas.json'
themesjson <- 'dev/themes.json'
outputfile <- 'dev/area-summaries.json'

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 2) {
    dbfile <- args[1]
    areasjson <- args[2]
    themesjson <- args[3]
    outputfile <- args[length(args)]
}


load(dbfile)
setDT(REARdb_Areas)
setDT(REARdb_Source)
setDT(REARdb_Data)

areas <- read_json(areasjson)
themes <- read_json(themesjson)


areaids <- sapply(areas[['areas']], function(area) area$id)

str(themes)



