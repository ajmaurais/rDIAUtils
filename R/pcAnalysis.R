pcAnalysis <-
function(dat, quantCol,
                       rowsName='precursor', columnsName='replicate',
                       x.pc=1, y.pc=2)
{

    requiredColumns <- c(quantCol, rowsName, columnsName)
    if(!all(requiredColumns %in% names(dat))) {
        stop('One or more required column names are missing from data frame!')
    }

    dat.w <- dplyr::select(dat, dplyr::all_of(c(columnsName, rowsName, quantCol))) %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(columnsName), values_from = dplyr::all_of(quantCol))

    dat.m <- as.matrix(dplyr::select(dat.w, !one_of(rowsName)))
    rownames(dat.m) <- dat.w[[rowsName]]

    res.d <- svd(dat.m - rowMeans(dat.m))
    pca <- prcomp(t(dat.m), retx = T, center = T, scale = T)
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
