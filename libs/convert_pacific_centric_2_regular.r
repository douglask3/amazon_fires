convert_pacific_centric_2_regular <- function(dat, tempWrite = FALSE) {
    dat_names = names(dat0)
    if (xmax(dat) < 180) return(dat)

    index = 1:length(values(dat[[1]]))

    xyz = cbind(xyFromCell(dat,index), values(dat))
    x = xyz[, 1]
    test = x > 180

    x[test] = x[test] - 360

    xyz[,1] = x
    dat = rasterFromXYZ(xyz, crs = projection(dat))
    names(dat) = dat_names

    if (tempWrite) dat = writeRaster(dat, file = memSafeFile())
    return(dat)
}

convert_regular_2_pacific_centric <- function(dat, tempWrite = FALSE) {
    if (xmax(dat) > 300) return(dat)
    dat_names = names(dat0)
    index = 1:length(values(dat[[1]]))

    xyz = cbind(xyFromCell(dat,index), values(dat))
    x = xyz[, 1]
    
    test = x < 0

    x[test] = x[test] + 360

    xyz[,1] = x
    dat = rasterFromXYZ(xyz, crs = projection(dat))

    names(dat) = dat_names

    if (tempWrite) dat = writeRaster(dat, file = memSafeFile())
    return(dat)
}
