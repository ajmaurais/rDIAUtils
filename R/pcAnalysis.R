
#' Perform PCA analysis on a column in a long formatted dataframe
#' 
#' @param dat Long formatted dataframe
#' @param quantCol The name of the column with quantative values to analyze
#' @param rowsName Name of the column to be row names in the wide matrix
#' @param columnsName Name of the column to be column names in the wide matrix
#' @param x.pc PC 1
#' @param y.pc PC 2
#' @param scale Scale argument passed to prcomp
#' @param na.rm Remove rows with NAs? Default is False.
#'   
#' @return list with 3 slots:
#'       pc: The pc dataframe
#'       x.lab: The x label for x.pc
#'       y.lab: The y label for y.pc
#' 
#' @import magrittr
#' @export
pcAnalysis <- function(dat, quantCol,
                       rowsName='precursor', columnsName='replicate',
                       x.pc=1, y.pc=2, scale=TRUE, na.rm=FALSE)
{

    requiredColumns <- c(quantCol, rowsName, columnsName)
    if(!all(requiredColumns %in% names(dat))) {
        stop('One or more required column names are missing from data frame!')
    }

    # pivot wider
    dat.w <- dplyr::select(dat, dplyr::all_of(c(columnsName, rowsName, quantCol))) %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(columnsName), values_from = dplyr::all_of(quantCol))

    # convert to matrix
    dat.m <- as.matrix(dplyr::select(dat.w, !one_of(rowsName)))
    rownames(dat.m) <- dat.w[[rowsName]]

    # check for missing values
    missing.sele <- apply(dat.m, 1, function(x) any(is.na(x)))
    if(na.rm & any(missing.sele)) {
        warning(paste('Removed', length(which(missing.sele)), 'row(s) with missing values!'))
        dat.m <- dat.m[!missing.sele,]
    } else if(any(missing.sele)) {
        stop(paste('There are', length(which(missing.sele)), 'row(s) with missing values!'))
    }

    # Remove rows with zero variance (if any)
    zero.var <- apply(dat.m, 1, var)
    zero.var <- names(zero.var[zero.var == 0])
    if(length(zero.var) > 0) {
        warning(paste('Removing', length(zero.var), 'rows with 0 variance.'))
        dat.m <- dat.m[!rownames(dat.m) %in% zero.var,]
    }

    res.d <- svd(dat.m - rowMeans(dat.m))
    pca <- prcomp(t(dat.m), retx = T, center = T, scale=scale)
    pcVar = round((res.d$d^2)/sum(res.d$d^2) * 100, 2)
    PCs <- data.frame(pc = 1:length(pcVar), pcVar = pcVar)
    pc <- data.frame(pca$x)
    pc[[columnsName]] <- rownames(pc)
    rownames(pc) <- 1:nrow(pc)

    ret <- list()
    ret[['pc']] <- dplyr::select(pc, all_of(c(columnsName, paste0('PC', c(x.pc, y.pc)))))

    ret[['x.lab']] = sprintf(paste0("PC",x.pc,": %.2f%% var"), pcVar[x.pc])
    ret[['y.lab']] = sprintf(paste0("PC",y.pc,": %.2f%% var"), pcVar[y.pc])
    ret
}
