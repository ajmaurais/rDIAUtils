readDataFile <-
function(file.basename, ext='.tsv', sep='\t') {
    if(!file.exists(paste0(file.basename, '.Rda'))) {
        message(paste('Reading', paste0(file.basename, ext)))
        ret <- read.csv(paste0(file.basename, ext), sep=sep)
        saveRDS(ret, file=paste0(file.basename, '.Rda'))
    } else {
        message(paste('Reading', paste0(file.basename, '.Rda')))
        ret <- readRDS(paste0(file.basename, '.Rda'))
    }
    ret
}
