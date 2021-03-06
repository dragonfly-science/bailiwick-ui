library(data.table)
library(yaml)
library(jsonlite)
source('functions.r')
source('patch.r')
source('language.r')

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
setkey(REARdb_Source, DatasetID)
setDT(REARdb_Data)
setkey(REARdb_Data, DatasetID)

langKeys <- list()

themes <- yaml.load_file(themesfile)
indicatorsdir <- paste0(dirname(themesfile),'/indicators/')
indicators <-
  lapply(list.files(indicatorsdir, '*.yaml'), function (indicatorfile) {
    indicator <- yaml.load_file(paste0(indicatorsdir, indicatorfile))
    cat("processing indicator: ", indicator$name, "\n")
    slices <- indicator$slices
    sources <- REARdb_Source[tolower(ValueName) %in% tolower(slices)]

    ## Features
    features <- as.character(unique(
      REARdb_Data[REARdb_Source[tolower(ValueName) %in% tolower(slices)],
                  .(ValueName, Dimension1)])[,
                             patch.featureNames(ValueName, Dimension1)]
    )
    features <- features[!is.na(features)]
    names(features) <- slugify(features)
    if ('all' %in% names(features)) {
        features['all'] <- fromMaybe(paste('all ', indicator$featureName), indicator$topFeatureLabel)
    }
    if (length(features) > 0) {
        if (!is.null(indicator$featureOrder)) {
            if (!is.null(indicator$topFeatureLabel)) {
                features <- features[c('all', indicator$featureOrder)]
            } else if ('all' %in% names(features)) {
                features <- features[c('all', indicator$featureOrder)]
            } else {
                features <- features[indicator$featureOrder]
            }
        } else {
            if (!is.null(indicator$topFeatureLabel)) {
                top = indicator$topFeatureLabel
                names(top) = slugify(top)
                features <- c(top, features)
            }
        }
        defaultFeature <- first(names(features))
    } else {
        defaultFeature <- NULL
    }

    # Years
    years <- sort(unique(REARdb_Data[REARdb_Source[tolower(ValueName) %in% tolower(slices)], Year]))

    # charts from list to map
    names(indicator$charts) <- sapply(indicator$charts, function (c) c$type)

    return( list(
        "id"                   = slugify(indicator$name)                       # :: IndicatorId
      , "name"                 = indicator$name                                # :: Text
      , "headerTitle"          = indicator$headerTitle                         # :: Text
      , "summaryTitle"         = indicator$summaryTitle                        # :: Text
      , "absoluteLabel"        = indicator$absoluteLabel                       # :: Maybe Text
      , "defaultChartLeft"     = fromMaybe("map", indicator$leftChart)         # :: ChartId
      , "defaultChartRight"    = fromMaybe("timeseries", indicator$rightChart) # :: ChartId
      , "features"             = fromMaybe(list(), names(features))            # :: [FeatureId]
      , "slices"               = slices
      , "units"                = fromMaybe("count", indicator$units)
      , "valueType"            = fromMaybe("quantity", indicator$valueType)
      , "topDetailLabel"       = indicator$topDetailLabel                      # :: Maybe Text
      , "topFeatureLabel"      = indicator$topFeatureLabel                     # :: Maybe Text
      , "defaultFeature"       = defaultFeature                                # :: Maybe Text
      , "featureName"          = indicator$featureName                         # :: Maybe Text
      , "featureDropdownLabel" = indicator$featureDropdownLabel                # :: Maybe Text
      , "yearEndMonth"         = formatYearEndMonth(sources$YrEndingMth)       # :: Maybe Text
      , "featureText"          = fromMaybe(as.list(features),
                                           indicator$featureText)              # :: Map FeatureId Text
      , "firstYear"            = toString(min(years))
      #fromMaybe("2018", indicator$firstYear)        # :: Text
      , "period"               = fromMaybe(1, indicator$period)                # :: Maybe Int
      , "notes"                = makeList(indicator$notes)                     # :: Maybe [Text]
      , "publishers"           = fromMaybe("unknown publisher",
                                           unique(as.character(sources$Publisher)))      # :: Text
      , "nationalNumCaption"   = fromMaybe("TODO nationalNumCaption",
                                           indicator$nationalNum$caption)      # :: Text
      , "localNumCaption"      = fromMaybe("TODO localNumCaption",
                                           indicator$localNum$caption)         # :: Text
      , "headlineNumCaption"   = fromMaybe("TODO headlineNumCaption",
                                           indicator$headlineNumCaption)       # :: Text

      , "years"                = years                                         # :: [Int]
      , "charts"               = indicator$charts
      , "languageConfig"       = indicator$'language-config'
      , "captions"             = captions(indicator, length(features) > 0)
      , "labels"               = labels(indicator, length(features) > 0)
      ))
  })
names(indicators) <- sapply(indicators, function(ind) ind$name)

#
# Get unique keys for indicator language config.
#
# keys <-
#   lapply(list.files(indicatorsdir, '*.yaml'), function (indicatorfile) {
#     indicator <- yaml.load_file(paste0(indicatorsdir, indicatorfile))
#     names(indicator$'language-config')
#   })

# print(unique(unlist(keys)))

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
