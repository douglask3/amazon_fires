monthOfYear <- function(i) {
    m = i %% 12
    if (m == 0) m = 12
    return(m)
}