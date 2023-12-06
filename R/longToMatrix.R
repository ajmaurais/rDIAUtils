longToMatrix <-
function(d, valuesFrom, rowsName, columnsName)
{
    d.w <- d %>% dplyr::select(all_of(c(valuesFrom, rowsName, columnsName))) %>%
        tidyr::pivot_wider(names_from=all_of(columnsName), values_from=all_of(valuesFrom))
    d.m <- as.matrix(dplyr::select(d.w, -all_of(rowsName)))
    rownames(d.m) <- d.w[[rowsName]]
    d.m
}
