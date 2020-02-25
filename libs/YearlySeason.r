YearlySeason <- function(season, r, FUN = 'sum') {
    if (is.raster(r)) r = nlayers(r)
    nyears = ceiling(r/12)
    seasons = lapply(1:nyears, function(i) season + (i-1) * 12)    
    test = sapply(seasons, function(i) all(i <= r))
    
    seasons = seasons[test]
    if (is.null(FUN)) return(seasons)
    if (FUN == "sum") out = layer.apply(seasons, function(i) sum(r[[i]]))
    else out = layer.apply(seasons, function(i) FUN(r[[i]]))
    return(out)
}
