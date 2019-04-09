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
setkey(REARdb_Data, DatasetID, AreaID)

themes <- read_json(themesjson)
indicators <- do.call(c, lapply(themes[['themes']], function(theme) theme$indicators))
names(indicators) <- sapply(indicators, function (ind) ind$id)

## Types
## type SummaryNumbers = InsOrdHashMap IndicatorId IndicatorSummary
## type IndicatorSummary = InsOrdHashMap (AreaId, Year, Maybe Feature) SummaryNums
## newtype SummaryNums = SummaryNums [Text] -- [HeadlineNum, LocalNum, NationalNum]

localformat <- function(unit, previous, value) {
    ifelse(is.na(previous), '...',
    ifelse(rep(unit, length(previous)) == 'percentage',
        formatValue('points', value-previous),
        formatValue('percentage', 100*(value-previous)/previous)))
}

nationalformat <- function(unit, national, value) {
    formatValue('ratio', value/national)
}

for (indid in names(indicators)) {

  outputfile = paste0(prefix, '/', indid, '.json')
  cat("preparing", outputfile, "...")

  unit <- indicators[[indid]]$unit
  datasetid <- REARdb_Source[tolower(indicators[[indid]]$slices), DatasetID]

  nzid <- REARdb_Areas[areaname=='new-zealand', AreaID]
  nzdata <- REARdb_Data[.(datasetid, nzid), .(Year, Dimension1, national=Value)]
  values <-
      REARdb_Data[.(datasetid)][
        REARdb_Areas, on=.(AreaID)][
        !is.na(Value)][
        nzdata, on=.(Year, Dimension1)]
  setkey(values, AreaID, Dimension1, Year)
  values[, previous := shift(Value,1), by=.(AreaID, Dimension1)]
  values[, max.val := max(Value, na.rm=T)]
  values[, min.val := min(Value, na.rm=T)]
  values[!is.na(Value), colour := colour.teal(Value, min.val, max.val)]

  summarynumbers <-
      values[,
        .(areaid   = areaname,
          year     = Year,
          feature  = slugify(Dimension1),
          headline = formatValue(unit, Value),
          local    = localformat(unit, previous, Value),
          national = nationalformat(unit, national, Value),
          colour   = colour
          )]

  cat(as.character(toJSON(summarynumbers, null='null', auto_unbox=TRUE)), file=outputfile)
  cat(" Done\n")

}
