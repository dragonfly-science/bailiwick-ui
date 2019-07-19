## Functions for creating captions and labels
source('functions.r')

defaultBase <- function (short, lc, hasFeatures) {
  sub <- lc[['subject']]
  if (!is.null(sub)) {
    subject <- sub
    subjectShort <- fromMaybe(sub, lc[['subject-short']])
    subjectQuantity <- fromMaybe(sub, lc[['subject-quantity']])
  } else {
    subjectShort <- lc[['subject-short']]
    subjectQuantity <- lc[['subject-quantity']]
    subject <- paste(subjectQuantity, 'of', subjectShort)
  }
  subjectAccessor <- fromMaybe("the", lc[["subject-accessor"]])
  if (!is.null(lc[["feature-as-subject-label"]])) {
    subjectAccessor <- paste(subjectAccessor, "$feature$")
  }
  areaAccessor <- fromMaybe("in", lc[["area-accessor"]])
  featureAccessor <- fromMaybe("in", lc[["feature-accessor"]])

  subject <- paste(subjectAccessor, subject)
  subjectQuantity <- paste(subjectAccessor, subjectQuantity)

  if (short) {
    area <- paste0(" ", areaAccessor, " $area$")
  } else {
    area <- ""
  }

  if (hasFeatures) {
    if (!is.null(lc[["feature-as-subject-label"]]) || !short) {
      feature = ""
    } else {
      featureLabel <- lc[['feature-label']]
      if (is.null(featureLabel)) {
        feature <- paste0(" ", featureAccessor, " $feature$")
      } else {
        feature <- paste0(" ", featureAccessor, " $feature$ ", featureLabel)
      }
    }
  } else {
    feature <- ""
  }

  base <- paste0(subject, feature, area)

  return(function (caption) {

    if (caption == 'rate')
      return(paste("the annual percentage change in", base))

    if (caption == 'annual-rate')
      return(paste("the annual percentage change in", base))

    if (caption == 'difference')
      return(paste("the annual percentage point change in", base))

    if (caption == 'ratio-nz')
      return(paste0("the ratio of ", base, " to the national ", subjectShort, feature))

    if (caption == 'feature-percentage')
      return(paste("the percentage of", subjectShort, area, feature))

    if (caption == 'national-percentage') {
      b = paste("the percentage of the total", subject, feature)
      singular <- fromMaybe(FALSE, lc[['singular']])
      if (short) {
          return(paste(b, "that", ifelse(singular, "was", "were"), area))
      } else {
          return(b)
      }
    }

    if (caption == 'original')
      return(base)

    if (caption == 'indexed')
      return(paste(base, "indexed to $firstYear$"))

    if (caption == 'percapita')
      return(paste0(base, ", per 10,000 people"))

    if (caption == 'ratio')
      return(paste("The ratio is", subject, feature, areaAccessor, "$selectedArea$, relative to"
                   , subjectQuantity, feature, areaAccessor, "$compareArea$"))

    stop(paste("Unknown caption type:", caption))
  })

}

transforms <- function(indicator, hasFeatures) {
  if (indicator$valueType == "quantity") {
    reqTransforms <- c("original", "national-percentage", "annual-rate", "indexed", "percapita")
    if (hasFeatures) {
      reqTransforms <- c(reqTransforms, 'feature-percentage')
    }
  } else if (indicator$valueType == "change") {
    reqTransforms <- c("original", "national-percentage", "percapita")
    if (hasFeatures) {
      reqTransforms <- c(reqTransforms, 'feature-percentage')
    }
  } else if (indicator$valueType == "aggregate") {
    if (indicator$units == "percentage") {
        reqTransforms <- c("original", "ratio-nz", "difference", "indexed")
    } else {
        reqTransforms <- c("original", "ratio-nz", "annual-rate", "indexed")
    }
  } else {
    stop(paste("Unknown valueType: ", indicator$valueType))
  }
  return(reqTransforms)
}

captions <- function(indicator, hasFeatures) {

  reqTransforms <- transforms(indicator, hasFeatures)
  lc <- indicator[['language-config']]
  defaultCaption <- defaultBase(TRUE, lc, hasFeatures)
  return(as.list(
      sapply(c('ratio', reqTransforms), function(caption) {
        return(fromMaybe(defaultCaption(caption), lc[['captions']][[caption]]))
      })))
}

labels <- function(indicator, hasFeatures) {

  reqTransforms <- transforms(indicator, hasFeatures)
  lc <- indicator[['language-config']]
  defaultCaption <- defaultBase(FALSE, lc, hasFeatures)
  return(as.list(
      sapply(reqTransforms, function(caption) {
        return(fromMaybe(defaultCaption(caption), lc[['labels']][[caption]]))
      })))
}

