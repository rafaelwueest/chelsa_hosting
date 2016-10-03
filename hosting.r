###############################################################################
#                                                                             #
#=============================================================================#
#                 DOWNLOAD, COMPRESS, AND PROVIDE CHELSA-DATA                 #
#=============================================================================#
#                                                                             #
#                                                                             #
#   WRITTEN BY RAFAEL WÜEST, SEPTEMBER 2016, WSL, BIRMENSDORF, SWITZERLAND    #
#   rafael.wueest@gmail.com                                                   #
#                                                                             #
###############################################################################



### settings et al

## wd
setwd('/Users/wueest/Dropbox/WSL/projects/CHELSA/hosting/')

## par
opar <- par(no.readonly = TRUE)

## packages
require(raster)
require(foreach)
require(doParallel)



### initialize and download version 1.0 data from chelsa

## create file list to be downloaded
flist <- c(
	'http://www.systbot.uzh.ch/static/download/chelsa/CHELSA_temp_1979-2013_land.tif',
	'http://www.systbot.uzh.ch/static/download/chelsa/CHELSA_prec_1979-2013_land.tif',
	paste('http://www.systbot.uzh.ch/static/download/chelsa/monthly_means/CHELSA_prec', 1:12, '1979-2013.tif', sep = '_'),
	paste('http://www.systbot.uzh.ch/static/download/chelsa/monthly_means/CHELSA_temp', 1:12, '1979-2013.tif', sep = '_')
	)
	

## download, compress, and write to right folder one at a time
# create function to be applied
dwnld.cmpr.wrt <- function(x, outdir = '/Volumes/luddmz/lud/chelsa/data/', flist, download = TRUE){
	if(download){
		download.file(flist[x], destfile = basename(flist[x]), method = 'libcurl')
	}
	if(grepl('.zip$', basename(flist[x]))){
		unzip(basename(flist[x]), junkpaths = TRUE, unzip = getOption("unzip"))
		file.remove(basename(flist[x]))
	}
	tmp <- raster(gsub('.zip$', '.tif', basename(flist[x])))
	if(grepl('temp', basename(flist[x])) | grepl('tmin', basename(flist[x])) | grepl('tmax', basename(flist[x]))) {
		new <- round(tmp * 10)
		writeRaster(new, filename = paste(outdir, gsub('.zip$', '.tif', basename(flist[x])), sep = ''), datatype = 'INT2S', overwrite = TRUE)
	} else {
		if(grepl('prec', basename(flist[x]))){
			new <- round(tmp)
			writeRaster(new, filename = paste(outdir, gsub('.zip$', '.tif', basename(flist[x])), sep = ''), datatype = 'INT2S', overwrite = TRUE)
		} else {
			writeRaster(tmp, filename = paste(outdir, gsub('.zip$', '.tif', basename(flist[x])), sep = ''), overwrite = TRUE)
		}
	}
	rm(tmp)
	file.remove(gsub('.zip$', '.tif', basename(flist[x])))
}
# set up cluster
clust <- makeCluster(4)
registerDoParallel(clust)
# run
foreach(i = 15:length(flist), .packages = 'raster', .errorhandling = 'pass') %dopar% {
	dwnld.cmpr.wrt(x = i, flist = flist)
}
# stop cluster
stopCluster(clust)



### update to version 1.1

## copy to archive
# create archive directory
system('mkdir -p /Volumes/luddmz/lud/chelsa/archive/version1.1/')
# create list of files to be copied
cplist1.1 <- paste('/Volumes/luddmz/lud/chelsa/data/', basename(flist), sep = '')
# set up cluster
clust <- makeCluster(12)
registerDoParallel(clust)
# copy files
foreach(i = 1:length(cplist1.1), .packages = 'raster', .errorhandling = 'pass') %dopar% {
	file.copy(from = cplist1.1[i], to = paste('/Volumes/luddmz/lud/chelsa/archive/version1.1', basename(cplist1.1[i]), sep = '/'))
}
# stop cluster
stopCluster(clust)

## create file list to be downloaded
flist1.1 <- c(
	'http://www.systbot.uzh.ch/static/download/chelsa/temp_v_1_1/CHELSA_temp_1979-2013_V1_1.zip',
	'http://www.systbot.uzh.ch/static/download/chelsa/CHELSA_prec_1979-2013_V1_1.tif',
	paste('http://www.systbot.uzh.ch/static/download/chelsa/temp_v_1_1/CHELSA_temp', 1:12, '1979-2013_V1_1.zip', sep = '_'),
	paste('http://www.systbot.uzh.ch/static/download/chelsa/monthly_means_v_1_1/CHELSA_tmax', 1:12, '1979-2013_land.zip', sep = '_'),
	paste('http://www.systbot.uzh.ch/static/download/chelsa/monthly_means_v_1_1/CHELSA_tmin', 1:12, '1979-2013_land.zip', sep = '_'),
	paste('http://www.systbot.uzh.ch/static/download/chelsa/monthly_means_v_1_1/CHELSA_prec', 1:12, '1979-2013_V1_1.zip', sep = '_'),
	paste('http://www.systbot.uzh.ch/static/download/chelsa/bioclim_v_1_1/CHELSA_bio', 1:19, '_1979-2013_V1_1.zip', sep = ''),
	'http://www.systbot.uzh.ch/static/download/chelsa/bioclim_v_1_1/CHELSA_temp_interannual_1979-2013_V1_1.zip',
	'http://www.systbot.uzh.ch/static/download/chelsa/bioclim_v_1_1/CHELSA_prec_interannual_1979-2013_V1_1.zip'
	)

## download, compress, and write to right folder one at a time
# set up cluster
clust <- makeCluster(8)
registerDoParallel(clust)
# run
tst <- foreach(i = 44:50, .packages = 'raster', .errorhandling = 'pass') %dopar% {
	dwnld.cmpr.wrt(x = i, flist = flist1.1, download = TRUE)
}
# stop cluster
stopCluster(clust)
# test if successful
all(as.logical(tst))

## remove files from data repository
file.remove(paste('/Volumes/luddmz/lud/chelsa/data/', setdiff(list.files('/Volumes/luddmz/lud/chelsa/data/'), gsub('.zip$', '.tif', basename(flist1.1))), sep = ''))

## reduce file-sizes of bioclim variables
# compression-writing function
cmpr.wrt.bio <- function(x){
	fnam <- paste('/Volumes/luddmz/lud/chelsa/data/CHELSA_bio', x, '_1979-2013_V1_1.tif', sep = '')
	tmp <- raster(fnam)
	if(x %in% c(1:2, 5:11)){
		new <- round(tmp * 10)
		writeRaster(new, fnam, dataType = 'INT2S', overwrite = TRUE)
	}
	if(x %in% c(3:4, 15)){
		new <- round(tmp * 100)
		writeRaster(new, fnam, dataType = 'INT2S', overwrite = TRUE)
	}
	if(x %in% c(12:14, 16:19)){
		new <- round(tmp)
		writeRaster(new, fnam, dataType = 'INT2S', overwrite = TRUE)
	}
}
# set up cluster
clust <- makeCluster(10)
registerDoParallel(clust)
# run
tst <- foreach(i = 1:19, .packages = 'raster', .errorhandling = 'pass') %dopar% {
	cmpr.wrt.bio(x = i)
}
# stop cluster
stopCluster(clust)








