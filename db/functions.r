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

formatValue <- function(unit, value) {
  if (unit == "dollars") {
    paste0("$", format(round(value), big.mark =',', trim=T))
  } else if (unit == "milliondollars") {
    paste0("$", format(round(value), big.mark =',', trim=T))
  } else if (unit == "percentage") {
    paste0(format(round(value), big.mark =','), '%')
  } else if (unit == "count") {
    paste0(format(round(value), big.mark =','))
  } else if (unit == "hectares") {
    paste0(format(round(value), big.mark =','))
  } else if (unit == "float") {
    paste0(format(round(value), big.mark =','))
  } else {
      as.character(value)
  }

}
