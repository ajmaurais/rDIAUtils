readLongCsv <-
function(fname)
{
    if(!file.exists(paste0(fname, '.Rda')))
    {
        message(paste('Loading', paste0(fname, '.tsv')))
        dat <- read.csv(paste0(fname, '.tsv'), sep='\t')
        
        dat.l <- dat %>% pivot_longer(!one_of(c('protein', 'ion')), names_to='replicate', values_to='area') %>%
            dplyr::rename(precursor='ion') # rename ion column because it annoys me

        saveRDS(dat.l, file=paste0(fname, '.Rda'))
    } else {
        message(paste('Loading', paste0(fname, '.Rda')))
        dat.l <- readRDS(paste0(fname, '.Rda'))
    }
    dat.l
}
