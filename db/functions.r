
slugify <- function(name) {
    gsub(' ',       '-',
    gsub('[^a-z -]', '',
    gsub('---',     '-',
    gsub('\\&',     'and',
    gsub('%',       'pc',
    gsub('\\+',        'plus',
    gsub('/',       'or',
    chartr('āēīōū', 'aeiou',
           tolower(name)))))))))
}
