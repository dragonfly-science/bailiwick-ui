## Rscript to patch some specific names
## From mbie-rear-2015 db/sql/base/patch.sql

patch.featureNames <- function(valuename, dimension1)
  return(
    ifelse(is.na(dimension1), NA,
    ifelse(valuename %in% c('Tertiary qualification completions by level',
                            'Tertiary qualification completions by level (domestic)',
                            'Tertiary Provision by level'),
          ifelse(dimension1 == 'L01-02.Certificates',          'Level 1-2',
          ifelse(dimension1 == 'L03-04.Certificates',          'Level 3-4',
          ifelse(dimension1 == 'L05-06.Diplomas.&.Grad.Certs', 'Level 5-6',
          ifelse(dimension1 == 'L07-08.Degrees.&.Postgrad',    'Level 7-8',
          ifelse(dimension1 == 'L09-10.Masters.&.Doctorates',  'Level 9-10',
                 as.character(dimension1)))))),
    ifelse(valuename %in% c('Tertiary enrolments by age (domestic)'),
          ifelse(dimension1 == 'Age.under.21', 'under 21 years',
          ifelse(dimension1 == 'Age.21-24',    '21-24 years',
          ifelse(dimension1 == 'Age.25+',      '25 years and over',
                 as.character(dimension1)))),
    ifelse(valuename %in% c('Tertiary enrolments by ethnicity'),
          ifelse(dimension1 == 'Pacific.Peoples', 'Pacific Peoples',
          ifelse(dimension1 == 'Maori',           'Māori',
          ifelse(dimension1 == 'European',        'European/Pākehā',
                 as.character(dimension1)))),
          as.character(dimension1)))))
  )

