library(raster)

sim_dir = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/'
global_extent = extent(c(-80, -35, -23.5, 0))
region = 'outputs/amazon_region/treeCoverTrendRegions.nc'

obs_file = 'burnt_area_MCD64A1.006.nc'
obs_dir = 'outputs/amazon_region/fire_counts/'

files = list.files(sim_dir)
files = files[grepl('sample_no_', files)]

files = files#[1:10]#[round(seq(1, length(files), length.out = 33))]#[sample(1:length(files), 10, replace = FALSE)]

mask = raster(region) != 6
mask[is.na(mask)] = 1
mask = crop(mask, global_extent)
mnths = list(222:224, 225)

totalArea = sum.raster((1-mask)*area(mask), na.rm = TRUE)
exeedance <- function(mnth) {
    
    cmnth = lapply(mnth, function(i) seq(i - 12*floor(i/12), i, 12))
    cmnth = lapply(1:length(cmnth[[1]]), function(i) sapply(cmnth, function(j) j[i]))
    sampleMember <- function(file, dir = sim_dir, var = "burnt_area") {        
        temp_file = paste0('temp/exceedance2019_for_month-', 
                           paste0(mnth,collapse = '-'), '-file-', file, '.Rd')
        
        if (file.exists(temp_file)) {
            load(temp_file)
        } else {
            print(temp_file)
            file = paste0(dir, file)
            
            annualAv <- function(mns) {
                dat = brick(file, varname = var)[[mns]]
                dat = crop(dat, global_extent)   
                sum(dat)
            }
            dat = layer.apply(cmnth, annualAv )    
            
            final = dat[[nlayers(dat)]]
            final[mask] = NaN
            
            final = sum(final > dat)
            exceedNo <- function(num)   {
                final = final >= (nlayers(dat)-num)
                final = 100*sum.raster(final * area(mask), na.rm = TRUE) / totalArea
            }
            frac = sapply(1:19, exceedNo)
            save(frac, file = temp_file)
        }
        return(frac)
    }
    out = sapply(files, sampleMember)
    
    obs = sampleMember(obs_file, obs_dir, "variable")
    return(t(rbind(obs, apply(out, 1, quantile, c(0.05, 0.5, 0.95)))))
}

tab = lapply(mnths, exeedance)
tab = do.call(cbind, tab)
colnames(tab) = paste0(rep(c('early', 'late'), each = ncol(tab)/2), '-', colnames(tab))
tab = round(tab, 0)
write.csv(tab, 'docs/no_years_beats.csv')

