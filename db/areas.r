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
                .(reg  = standardise.areaname(REGC2015_label),
                  ta   = standardise.areaname(TA2015_label),
                  ward = standardise.areaname(WARD2015_label))])

areas <- areas[!(
    ta == 'Taupo'      & !(reg == 'Waikato') |
    ta == 'Rotorua'    & !(reg == 'Bay of Plenty') |
    ta == 'Tararua'    & !(reg == 'Manawatu-Wanganui') |
    ta == 'Stratford'  & !(reg == 'Taranaki') |
    ta == 'Rangitikei' & !(reg == 'Manawatu-Wanganui') |
    ta == 'Waitomo'    & !(reg == 'Waikato') |
    ta == 'Waitaki'    & !(reg == 'Otago'))]

nz <- data.table(name='New Zealand', 'id'='new-zealand', level='nz')
regions <-
    areas[, .(name=unique(reg), id=slugify(unique(reg)), level='reg')][order(id)]
tas <-
    areas[ta!='Auckland' & !(ta %in% regions$name),
          .(name=unique(ta), id=slugify(unique(ta)), level='ta')][order(id)]
wards <-
    areas[ta=='Auckland',
          .(name=unique(ward),
            id=paste0('auckland--',slugify(unique(ward))),
            level='ward')][
          order(id)]

all = rbind(nz, regions, tas, wards)

output <-
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
                         as.list(areas[reg==a$name & !(ta ==a$name),
                                       slugify(unique(ta))])
                     } else if(a$level=='reg' & a$id == 'auckland') {
                         as.list(areas[reg==a$name,
                                 paste0('auckland--',slugify(unique(ward)))])
                     } else {
                         list()
                     }
          )
    })

cat(as.character(toJSON(output, auto_unbox=TRUE)), file=outputfile)
