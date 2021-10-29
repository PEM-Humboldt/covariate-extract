################################################################################
#       Landcover transformation indicator (initial year to final year)        #
################################################################################

# Raster data: Landcover maps for every year that you want to compare. European
# spatial agency (ESA) offers landcover maps from 1992 to 2020 until today 
# (October 2021). You can check the avaliablity of data here:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=overview

# Vector data: This script works with Large SpatialPolygonsDataFrame, for example
# Camera Traps. Perhaps, you can use polygon data too.

# Prepare your shapefile data
library(raster)

setwd("Your/Working/Directory")
Shp <- shapefile("YourPoints.shp")
IDfld <- "YourPointID"    # Check the attribute table of your points and identify
                          # an ID for each point.
prefl <- "YourPrefix"     # This will help you to identify the exported results
bfls <- c(30,500,5000)    # Do you need to use different buffers? Put them here (m)
RD <- "YourResultsfolder" # We recommend to use a specific folder for your results

ShpL <- list()                                  
for(i in bfls){
  Shpb <- buffer(Shp, width= i, dissolve= F)
  nml <- paste("b", i, sep="_")
  ShpL[[nml]] <- Shpb
}                         # Creates a list of buffer features

Shpb <- do.call(rbind, ShpL)                      # Transform the list into a dataframe 
Shpb@data$buff <- rep(bfls, each= nrow(Shp@data)) # Add buffer information in a column

# Prepare your raster data
InD2 <- ("Your/File/Path/With/RasterData")
lsrst <- list.files(InD2, pattern= "\\.tif$", full.names= T)

y= 0
for (i in lsrst){
  rs <- raster(i)
  if (y == 0) {
    data <- raster::stack(rs)
    y <- 1
  } else{
    data <- raster::stack(data, rs)
  }
}

# Get ESA data cover change
tdf.gcs <- spTransform(Shpb, crs(data))
datar <- extract(data, tdf.gcs)
outnm <- file.path(RD, paste(prefl, "ESAcov.RData", sep= "_"))
save(datar, file=outnm) # Save a backup of the raw data in format .RData

Indc <- function(x){
  x <- data.frame(x)
  xx <- x[,2:ncol(x)]
  y <- x[,1:ncol(x)-1]
  yy <- xx==y
  selrw <- which(!is.na(yy), arr.ind= T)
  srow <- unique(selrw[,1])
  sclm <- unique(selrw[,2])
  sumx <- rowSums(yy, na.rm=T) #sum no changes across time
  sumy <- colSums(yy, na.rm=T) #sum no chages across pixels
  data.frame("meant"= mean(sumx[srow], na.rm= T), "sdt"= sd(sumx[srow], na.rm=T),
             "meanpx"= mean(sumy[sclm], na.rm=T), "sdpx"= sd(sumy[sclm], na.rm=T),
             "npix"= length(srow))
}

shpESAcov <- lapply(datar, Indc)
shpESAcov <- do.call(rbind, shpESAcov)
shpESAcov$stdMeanpx <- shpESAcov$meanpx/shpESAcov$npix
shpESAcov$cfvt <- shpESAcov$sdt/shpESAcov$meant
shpESAcov$polid <- tdf.gcs@data[,IDfld]
outnm <- file.path(RD, paste(prefl, "ESAcov.csv", sep= "_"))
write.csv(shpESAcov, outnm) 


