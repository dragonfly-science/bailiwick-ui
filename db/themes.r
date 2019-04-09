library(data.table)
library(yaml)
library(jsonlite)
source('functions.r')

dbfile <- 'data/REARdb.rda'
themesfile <- 'config/themes.yaml'
outputfile <- 'dev/themes.json'

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 3) {
    dbfile <- args[1]
    themesfile <- args[2]
    outputfile <- args[length(args)]
}

load(dbfile)
setDT(REARdb_Source)
setDT(REARdb_Data)

themes <- yaml.load_file(themesfile)
indicatorsdir <- paste0(dirname(themesfile),'/indicators/')
indicators <-
  lapply(list.files(indicatorsdir, '*.yaml'), function (indicatorfile) {
    indicator <- yaml.load_file(paste0(indicatorsdir, indicatorfile))
    cat("processing indicator: ", indicator$name, "\n")
    slices <- indicator$slices
    sources <- REARdb_Source[tolower(ValueName) %in% tolower(slices)]
    features <- as.character(unique(
      REARdb_Data[DatasetID %in% REARdb_Source[tolower(ValueName) %in% slices, DatasetID], Dimension1]
    ))
    features <- features[!is.na(features)]
    return( list(
        "id"                   = slugify(indicator$name)                       # :: IndicatorId
      , "name"                 = indicator$name                                # :: Text
      , "headerTitle"          = indicator$headerTitle                         # :: Text
      , "summaryTitle"         = indicator$summaryTitle                        # :: Text
      , "absoluteLabel"        = indicator$absoluteLabel                       # :: Maybe Text
      , "defaultChartLeft"     = fromMaybe("map", indicator$leftChart)         # :: ChartId
      , "defaultChartRight"    = fromMaybe("timeseries", indicator$rightChart) # :: ChartId
      , "features"             = fromMaybe(features, indicator$sortedFeature)  # :: [Text]
      , "slices"               = slices
      , "units"                = fromMaybe("count", indicator$units)
      , "valueType"            = fromMaybe("quantity", indicator$valueType)

      , "topDetailLabel"       = indicator$topDetailLabel                      # :: Maybe Text
      , "topFeatureLabel"      = indicator$topFeatureLabel                     # :: Maybe Text
      , "yearEndMonth"         = formatYearEndMonth(sources$YrEndingMth)       # :: Maybe Text
      , "featureText"          = indicator$featureText                         # :: Maybe (Map FeatureId Text)
      , "firstYear"            = fromMaybe("2018", indicator$firstYear)        # :: Text
      , "period"               = fromMaybe(1, indicator$period)                # :: Maybe Int
      , "notes"                = makeList(indicator$notes)                     # :: Maybe [Text]
      , "publishers"           = fromMaybe("unknown publisher",
                                           unique(as.character(sources$Publisher)))      # :: Text
      , "nationalNumCaption"   = fromMaybe("TODO nationalNumCaption",
                                           indicator$nationalNum$caption)   # :: Text
      , "localNumCaption"      = fromMaybe("TODO localNumCaption",
                                           indicator$localNum$caption)      # :: Text
      , "headlineNumCaption"   = fromMaybe("TODO headlineNumCaption",
                                           indicator$headlineNumCaption)   # :: Text

      ))
  })
names(indicators) <- sapply(indicators, function(ind) ind$name)

cat(as.character(toJSON(
    list("themes" =
      lapply(1:length(themes), function(i) {
        theme <- themes[[i]]
        list(
          id    = slugify(theme$name),
          name  = theme$name,
          indicators =
            lapply(theme$indicators, function(indicator) {
              if (indicator %in% names(indicators)) {
                return(indicators[[indicator]])
              } else {
                print(paste("Indicator '", indicator, "' not found, skipping"))
              }
            })
          )
      })), auto_unbox=TRUE, null="null"))
    , file=outputfile)


