library(data.table)
library(jsonlite)
source('functions.r')

inputfile <- 'data/REARdb.rda'
outputfile <- 'dev/areas.json'

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 2) {
    inputfile <- args[1]
    outputfile <- args[length(args)]
}

load(inputfile)
setDT(REARdb_Areas_Hierarchy)

areas <- unique(REARdb_Areas_Hierarchy[
                !(TA2015_label == 'Area Outside Territorial Authority' |
                  REGC2015_label == 'Area Outside Region'),
                .(reg  = REGC2015_label,
                  ta   = TA2015_label,
                  ward = WARD2015_label)])

areas[, reg := gsub(' Region', '', reg)]
areas[!grepl('^(Southland|Waikato)', ta), ta := gsub(' District', '', ta)]
areas[!grepl('^Wellington', ta), ta := gsub(' City', '', ta)]
areas[, ward := gsub(' Ward', '', ward)]

areas <- areas[!(
    ta == 'Taupo'      & !(reg == 'Waikato') |
    ta == 'Rotorua'    & !(reg == 'Bay of Plenty') |
    ta == 'Tararua'    & !(reg == 'Manawatu-Wanganui') |
    ta == 'Stratford'  & !(reg == 'Taranaki') |
    ta == 'Rangitikei' & !(reg == 'Manawatu-Wanganui') |
    ta == 'Waitomo'    & !(reg == 'Waikato') |
    ta == 'Waitaki'    & !(reg == 'Otago'))]


all = rbind(
    data.table(name='New Zealand', 'id'='new-zealand', level='nz'),
    areas[, .(name=unique(reg), id=slugify(unique(reg)), level='reg')][order(id)],
    areas[ta!='Auckland', .(name=unique(ta), id=slugify(unique(ta)), level='ta')][order(id)],
    areas[ta=='Auckland', .(name=unique(ward), id=slugify(unique(ward)), level='ward')][order(id)]
    )
cat(as.character(toJSON(
    list("areas" =
      lapply(1:nrow(all), function(i) {
        a <- all[i]
        list(
          id    = a$id,
          name  = a$name,
          level = a$level,
          parents = if(a$level=='reg') {
                        list('new-zealand')
                    } else if (a$level=='ta') {
                        as.list(areas[ta==a$name, slugify(unique(reg))])
                    } else if (a$level=='ward') {
                        list('auckland')
                    } else {
                        list()
                    },
          children = if(a$level=='reg' & a$id != 'auckland') {
                         as.list(areas[reg==a$name, slugify(unique(ta))])
                     } else if(a$level=='reg' & a$id == 'auckland') {
                         as.list(areas[reg==a$name, slugify(unique(ward))])
                     } else {
                         list()
                     }
          )
      })), auto_unbox=TRUE)), file=outputfile)
