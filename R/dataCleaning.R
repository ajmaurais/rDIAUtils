
#' Convert a date string into POSIX format
#' 
#' @param dat Dataframe
#' @param acquiredTimeCol Name of column with with the UTC date to parse
#' 
#' @returns Dataframe with acquiredTimeCol as an R POSIXlt object.
#' 
parseAcquiredTime <- function(d, acquiredTimeCol='acquiredTime') {
    date.cols <- c('date', 'h', 'm', 's', 'pam')
    d <- tidyr::extract(d, acquiredTimeCol, into=date.cols,
                        regex='([12]?[0-9]/[0-9]{1,2}/[0-9]{2,4}) ([0-9]{1,2}):([0-9]{2}):([0-9]{2}) ([AP]M)',
                        convert=F)
    d$h <- as.numeric(d$h) + ifelse(d$pam == 'PM', 12, 0)
    d$h <- ifelse(d$h == 24, 0, d$h)
    d[[acquiredTimeCol]] <- with(d, paste0(date, ' ', h, ':', m, ':', s))
    d[[acquiredTimeCol]] <- as.POSIXlt(d[[acquiredTimeCol]], format='%m/%d/%Y %H:%M:%OS')
    if(anyNA(d[,c(date.cols, acquiredTimeCol)])){
        stop('Damnit! There are unparsable dates!')
    }
    d <- dplyr::select(d, !dplyr::one_of(date.cols))
    return(d)
}

#' Set zeros to the mininum non-zero area in each batch
zeroToMin <- function(x) {
    sele <- x == 0 | is.na(x)
    x[sele] <- min(x[!sele])
    x
}

readDataFile <- function(file.basename, ext='.tsv', sep='\t') {
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

readLongCsv <- function(fname)
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

#' Convert long dataframe to wide numeric matrix.
#' 
#' @param d Long formated dataframe
#' @param valuesFrom The name of the column with values that should be in the matrix.
#' @param rowsName Name of the column to be row names in the matrix
#' @param columnsName Name of the column to be column names in the matrix
#' 
#' @returns A numeric matrix.
#' 
longToMatrix <- function(d, valuesFrom, rowsName, columnsName)
{
    d.w <- d %>% dplyr::select(all_of(c(valuesFrom, rowsName, columnsName))) %>%
        tidyr::pivot_wider(names_from=all_of(columnsName), values_from=all_of(valuesFrom))
    d.m <- as.matrix(dplyr::select(d.w, -all_of(rowsName)))
    rownames(d.m) <- d.w[[rowsName]]
    d.m
}

