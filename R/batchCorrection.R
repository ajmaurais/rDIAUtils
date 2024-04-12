
#' Perform batch correction using the selected method.
#' 
#' @param d Long formatted dataframe where each row is an observation for a single condition.
#' @param quantCol Name of the column with quantative values.
#' @param batch1 Name of the column to use for the first batch.
#' 
#' @param batch2 Name of the column to use for the second batch. (optional). Using a second batch is not supported with ComBat
#' @param covariate.cols The names of the covariate columns. (optional)
#' @param rowsName Name of the column to be row names in the wide matrix. Default is 'precursor'
#' @param columnsName Name of the column to be column names in the wide matrix. Default is 'replicate'
#' @param bc.method The batch correction column to use. One of c('combat', 'limma'). Default is 'limma'
#' @param return.format How should the resulting dataframe be formatted?
#'              'Long': Long formatted dataframe. (default)
#'              'Wide': Wide formatted dataframe.
#'              'Matrix': Wide formatted numeric matrix.
#' 
#' @return Batch corrected dataframe.
#' 
#' @import magrittr
#' @export
batchCorrection <- function(d, quantCol, batch1, batch2=NULL, covariate.cols=NULL,
                            rowsName='precursor', columnsName='replicate', bc.method='limma',
                            return.format='long')
{
    batch.cols <- c(batch1)
    if(!is.null(batch2)) {
        batch.cols <- c(batch.cols, batch2)
    }

    label.cols <- c(batch.cols)
    if(!is.null(covariate.cols)) {
        label.cols <- c(label.cols, covariate.cols)
    }

    # convert to numeric matrix for batch correction function
    d.m <- longToMatrix(d, quantCol, rowsName, columnsName)

    # make labels dataframe
    d.labels <- d %>% dplyr::select(dplyr::all_of(c(columnsName, label.cols))) %>%
        dplyr::distinct()
    for(column in label.cols) {
        d.labels[[column]] <- factor(d.labels[[column]])
    }
    d.labels <- d.labels[which(d.labels[[columnsName]] %in% colnames(d.m)),]

    # check for replicates with missing metadata
    if(length(label.cols) > 1) {
        na.replicates.sele <- apply(d.labels[,label.cols], 1, function(x) any(is.na(x)))
    } else {
        na.replicates.sele <- is.na(d.labels[[label.cols]])
    }
    if(any(na.replicates.sele)) {
        for(row_i in which(na.replicates.sele)) {
            warning(paste('Missing metadata for replicate:', d.labels$replicate[row_i]))
        }
        stop(paste('Missing metadata annotations for', length(which(na.replicates.sele)), 'replicate(s)!'))
    }
    
    # Actually do batch correction
    if(bc.method == 'combat')
    {
        if(!is.null(batch2)) {
            stop('ComBat only supports 1 batch!')
        }

        if(length(covariate.cols) > 2) {
            stop('ComBat only supports up to 2 covariate variables.')
        }

        formula_str <- paste('~', paste0('d.labels[[\"', covariate.cols, '\"]]', collapse=' + '))
        modcombat <- if(is.null(covariate.cols)) NULL else { model.matrix(formula(formula_str), data = d) }

        d.m.bc <- sva::ComBat(d.m, d.labels[[batch1]], mod=modcombat, par.prior = T, prior.plots = F)
    }
    else if(bc.method == 'limma')
    {
        formula_str <- paste('~', paste0('d.labels[[\"', covariate.cols, '\"]]', collapse=' + '))
        modlimma <- if(is.null(covariate.cols)) matrix(1, ncol(d.m), 1) else { model.matrix(formula(formula_str)) }

        d.m.bc <- limma::removeBatchEffect(x=d.m, batch1=d.labels[[batch1]],
                                           batch2=if(is.null(batch2)) NULL else {d.labels[[batch2]]},
                                           design=modlimma)
    } else {
        stop(paste(bc.method, 'is an unknown batch correction method!'))
    }

    # If return format is matrix, we are done
    if(return.format == 'matrix') {
        return(d.m.bc)
    }

    # convert matrix to wide dataframe
    d.w.bc <- data.frame(d.m.bc)
    d.w.bc[[rowsName]] <- rownames(d.w.bc)
    rownames(d.w.bc) <- 1:nrow(d.w.bc)
    d.w.bc <- dplyr::select(d.w.bc, all_of(rowsName), all_of(colnames(d.m.bc)))

    if(return.format == 'wide') {
        return(d.w.bc)
    }
    else if(return.format == 'long') {
        # convert to long dataframe
        d.l.bc <- d.w.bc %>% tidyr::pivot_longer(all_of(colnames(d.m.bc)), names_to=columnsName, values_to=quantCol) %>%
            dplyr::left_join(d.labels, by=columnsName) %>%
            dplyr::select(all_of(c(columnsName, rowsName, label.cols, quantCol)))
        return(d.l.bc)
    }
    stop(paste(return.format, 'is an unknown return.format!'))
}

