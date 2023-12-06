getName <-
function(vec) {
    if(is.null(names(vec))){
        return(vec)
    }
    ret <- names(vec)
    ret[ret == ''] <- vec[names(vec) == '']
    return(ret)
}
