standardGrid <- function() {
    grid(nx = 0, ny = NULL, col = "grey")
    lapply(2001:2020, function(i) lines(c(i, i), c(-9E9, 9E9), lty = "dotted", col = "grey"))
    lapply(seq(1, 1201, by = 12),
           function(i) lines(c(i, i), c(-9E9, 9E9), lty = "dotted", col = "grey"))
}
