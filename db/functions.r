slugify <- function(name) {
    gsub(' ',        '-',
    gsub('[^a-z -]', '',
    gsub('---',      '-',
    gsub('\\&',      'and',
    gsub('%',        'pc',
    gsub('\\+',      'plus',
    gsub('/',        'or',
#    chartr('āēīōū',  'aeiou',
           tolower(name))))))))#)
}

fromMaybe <- function(def, val) if (is.null(val)) { def } else { val }

standardise.areaname <- function(areaname) {
  areaname <- gsub(' Region', '', areaname)
  areaname <- gsub(' Ward', '', areaname)
  areaname <- ifelse(!grepl('^(Southland|Waikato)', areaname), gsub(' District', '', areaname), areaname)
  areaname <- ifelse(!grepl('^Wellington', areaname), gsub(' City', '', areaname), areaname)
  return(areaname)
}

