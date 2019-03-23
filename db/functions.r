slugify <- function(name) {
    gsub(' ',        '-',
    gsub('[^a-z -]', '',
    gsub('---',      '-',
    gsub('\\&',      'and',
    gsub('%',        'pc',
    gsub('\\+',      'plus',
    gsub('/',        'or',
    chartr('āēīōū',  'aeiou',
           tolower(name)))))))))
}

fromMaybe <- function(def, val) if (is.null(val)) { def } else { val }


