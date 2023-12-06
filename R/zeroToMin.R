zeroToMin <-
function(x) {
    sele <- x == 0 | is.na(x)
    x[sele] <- min(x[!sele])
    x
}
