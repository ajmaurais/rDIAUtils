
#' Convert a date string into POSIX format
#'
#' @param dat Dataframe
#' @param acquiredTimeCol Name of column with with the UTC date to parse
#'
#' @return Dataframe with acquiredTimeCol as an R POSIXlt object.
#'
#' @export
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

#' Set zeros to the minimum non-zero area in x.
#'
#' @export
zeroToMin <- function(x) {
    sele <- x == 0 | is.na(x)
    x[sele] <- min(x[!sele])
    x
}

#' Parse tsv file and save it as a serilized R object.
#'
#' If the serilized file already exists, it is read instead of the tsv file
#'
#' @param file.basename The basename of the tsv/Rda file.
#' @param ext The file extension. Default is '.tsv'
#' @param sep The deliminator. Default is '\\t'
#'
#' @export
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

#' Parse DirectLFQ matrix into a long formatted dataframe.
#'
#' Also save intermediate .Rda file similar to rDIAUtils::readDataFile.
#'
#' @param fname The DirectLFQ matrix.
#'
#' @seealso [rDIAUtils::readDataFile]
#'
#' @import magrittr
#' @export
readDirectLFQMatrix <- function(fname)
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
#' @param d Long formatted dataframe
#' @param valuesFrom The name of the column with values that should be in the matrix.
#' @param rowsName Name of the column to be row names in the matrix
#' @param columnsName Name of the column to be column names in the matrix
#'
#' @return A numeric matrix.
#'
#' @import magrittr
#' @export
longToMatrix <- function(d, valuesFrom, rowsName, columnsName)
{
    d.w <- d %>% dplyr::select(all_of(c(valuesFrom, rowsName, columnsName))) %>%
        tidyr::pivot_wider(names_from=all_of(columnsName), values_from=all_of(valuesFrom))
    d.m <- as.matrix(dplyr::select(d.w, -all_of(rowsName)))
    rownames(d.m) <- d.w[[rowsName]]
    d.m
}

#' Convert long dataframe to wide dataframe.
#'
#' @param d Long formatted dataframe
#' @param valuesFrom The name of the column with values to be spread into columns.
#' @param rowsName Name of the column(s) to keep for each row.
#' @param columnsName Name of the column to be column names.
#'
#' @return A wide format dataframe.
#'
#' @import magrittr
#' @export
pivotLonger <- function(d, valuesFrom, rowsName, columnsName)
{
    dplyr::select(d, all_of(c(valuesFrom, rowsName, columnsName))) %>%
        tidyr::pivot_wider(names_from=all_of(columnsName), values_from=all_of(valuesFrom))
}

#' Read metadata and replicates from batch database.
#'
#' Metadata is converted from long format used in database and converted to wide format.
#' The values from the sampleMetadataTypes table are then used to convert each column
#' into the appropiate date type.
#'
#' @param conn An initialized database connection
#'
#' @return Replicate and metadata dataframe
#'
#' @export
readWideMetadata <- function(conn)
{
    dat.rep <- DBI::dbGetQuery(conn,
                                'SELECT
                                    r.replicate,
                                    r.id as replicateId,
                                    r.project,
                                    r.ticArea,
                                    r.acquiredRank
                                FROM replicates r
                                WHERE r.includeRep == TRUE')
    dat.meta.l <- DBI::dbGetQuery(conn, 'SELECT * FROM sampleMetadata;')
    meta.types <- DBI::dbGetQuery(conn, 'SELECT annotationKey, annotationType
                                         FROM sampleMetadataTypes;')

    meta.converters <- as.list(setNames(meta.types$annotationType,
                                        meta.types$annotationKey))
    meta.converters <- lapply(meta.converters, function(x) {
        if(x == 'BOOL') { return(as.logical) }
        if(x == 'INT') { return(as.integer) }
        if(x == 'FLOAT') { return(as.numeric) }
        if(x == 'STRING') { return(function(x) {ifelse(x == '', NA, x)}) }
        if(x == 'NULL') { return(function(x) {NA}) }
        stop(paste(x, 'is an unknown variable type!'))
    })

    dat.meta.w <- tidyr::pivot_wider(dat.meta.l, names_from='annotationKey', values_from='annotationValue')
    for(column in names(meta.converters)) {
        dat.meta.w[[column]] <- meta.converters[[column]](dat.meta.w[[column]])
    }

    dplyr::left_join(dat.rep, dat.meta.w, by='replicateId')
}
