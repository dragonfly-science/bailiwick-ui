library(data.table)
library(jsonlite)
source('functions.r')

dbfile <- 'data/REARdb.rda'
themesjson <- 'dev/themes.json'
prefix <- 'dev'

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 2) {
    dbfile <- args[1]
    themesjson <- args[2]
    prefix <- args[3]
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
setkey(REARdb_Data, DatasetID, AreaID, Dimension1)

themes <- read_json(themesjson)
indicators <- do.call(c, lapply(themes[['themes']], function(theme) theme$indicators))
names(indicators) <- sapply(indicators, function (ind) ind$id)



## Types
## type SummaryNumbers = InsOrdHashMap IndicatorId IndicatorSummary
## type IndicatorSummary = InsOrdHashMap (AreaId, Year, Maybe Feature) SummaryNums
## newtype SummaryNums = SummaryNums [Text] -- [HeadlineNum, LocalNum, NationalNum]

lookupnums <- function(datasetid, areaname1, feature) {

  aid <- REARdb_Areas[areaname1, AreaID][1]

  vals <- REARdb_Data[.(datasetid,aid, feature)][order(Year),
                      .(Year, headline=Value, local = Value - shift(Value))]
  nzid <- REARdb_Areas[areaname=='new-zealand', AreaID]

  vals <- REARdb_Data[.(datasetid, nzid, feature)][vals, on=.(Year)][
                    ,.(Year, headline, local, national= headline/Value)]
  return(vals)
}


for (indid in names(indicators)) {

  outputfile = paste0(prefix, '/', indid, '.json')
  cat("preparing", outputfile, "...")

  unit <- indicators[[indid]]$unit
  datasetid <- REARdb_Source[tolower(indicators[[indid]]$slices), DatasetID]
  years <- sort(unique(REARdb_Data[DatasetID == datasetid, Year]))
  aids <- sort(unique(REARdb_Data[DatasetID == datasetid, AreaID]))
  areanames <- unique(REARdb_Areas[AreaID %in% aids, areaname])
  features <- unique(REARdb_Data[DatasetID == datasetid, as.character(Dimension1)])
  grid <- expand.grid(areaname=areanames, year=years, feature=features)

  summarynumbers <-
      lapply(1:nrow(grid), function (i) {
        vals <- lookupnums(datasetid,
                              as.character(grid$areaname[i]),
                              as.character(grid$feature[i])
                              )
        return(list(
            areaname = as.character(grid$areaname[i]),
            feature  = as.character(grid$feature[i]),
            year     = grid$year[i],
            numbers  = vals[Year==grid$year[i],
                           .(formatValue(unit, headline),
                             formatValue('pp',local),
                             formatValue('number', national))]
            ))
      })

  cat("Done\n")
  cat(as.character(toJSON(summarynumbers, null='null', auto_unbox=TRUE)), file=outputfile)

}
