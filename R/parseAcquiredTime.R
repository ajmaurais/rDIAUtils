parseAcquiredTime <-
function(d, acquiredTimeCol='acquiredTime') {
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
