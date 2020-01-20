source("libs/writeRaster.Standard.r")

writeInput <- function(dat, dir_out, fname_out, years = NULL) {
    if (is.null(years))
        years =  sapply(strsplit(names(dat[[1]]), '.', fixed = TRUE), substr, 2, 5)[1,]

    if (min(years) < 2001)  dir = paste0(dir_out, '/full_period', '/')
    else  dir = paste0(dir_out, '/from_2001','/')
    
    makeDir(dir)
    fname = paste0(dir, fname_out, paste(range(years), collapse = '-'), '.nc')
    writeRaster.Standard(dat, fname)
}
