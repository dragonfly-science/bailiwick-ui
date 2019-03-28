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
areatypes <- c('Regional Council', 'Territorial Authority', 'Ward', 'Total')
REARdb_Areas <- REARdb_Areas[AreaType %in% areatypes]
REARdb_Areas[, areaname := slugify(standardise.areaname(Area))]
setkey(REARdb_Areas, areaname)

setDT(REARdb_Source)
REARdb_Source[, slice:=tolower(ValueName)]
setkey(REARdb_Source, slice)

setDT(REARdb_Data)
setkey(REARdb_Data, DatasetID, AreaID)

areas <- read_json(areasjson)
themes <- read_json(themesjson)

indicators <- do.call(c, lapply(themes[['themes']], function(theme) theme$indicators))
names(indicators) <- sapply(indicators, function (ind) ind$id)

areaids <- unique(sapply(areas[['areas']], function(area) area$id))

lookup <- function(areaname1, indicatorid) {
  datasetid <- REARdb_Source[tolower(indicators[[indicatorid]]$slices), DatasetID]
  aid <- REARdb_Areas[areaname1, AreaID]
  all <- indicatorid == 'mean-house-value'
  vals <- REARdb_Data[.(datasetid, aid)][
                      if(all) {TRUE} else {which.max(Year)}, .(Year, Value)][
                      order(Year)]
  vals <- vals[!is.na(Value)]
  if(nrow(vals) == 0) {
      return(NULL)
  } else {
      return(as.matrix(vals))
  }
}

summaryindicators <-
  c('population-estimates', 'household-income-mean', 'mean-weekly-rent', 'gdp-per-capita', 'mean-house-value')

summaries <-
  sapply(summaryindicators, function (ind) {
    sapply(areaids, function(areaid) {
      lookup(areaid, ind)
    }, simplify=FALSE)
  }, simplify=FALSE)

cat(as.character(toJSON(summaries
    , null='null', auto_unbox=TRUE)), file=outputfile)



