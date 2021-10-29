################################################################################
#                            Year of disturbance                               #
#                           Forest Integrity (SCI)                             #
#                           Human Footprint Index                              #
#                                Tree Height                                   #
#                           Height above sea level                             #
#                             Population density                               #
################################################################################

# Raster data: 
# - Year of disturbance: Estimation of year of disturbance produced by Matt 
#   Hansen (University of Maryland) between 2000 and 2020. Available in:
#   https://data.globalforestwatch.org/documents/134f92e59f344549947a3eade9d80783/explore
# - Forest Integrity (SCI): Indicator developed by Hansen et al. 2020. Script
#   available in: https://www.nature.com/articles/s41597-019-0214-3#code-availability
# - Human Footprint Index: Spatiotemporal variation of anthropic impact. You can 
#   use the global version available here: 
#   https://www.cell.com/one-earth/pdfExtended/S2590-3322(20)30418-8 or a 
#   different variable like Global Human Modification available here:
#   https://essd.copernicus.org/articles/12/1953/2020/essd-12-1953-2020-assets.html
# - Tree Height: Global forest canopy height developed by Potapov et al., 2020
#   and available here https://glad.umd.edu/dataset/gedi
# - Height above sea level: DEM
# - Population density: You can use a global layer available in:
#   https://www.worldpop.org/project/categories?id=3


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

# Prepare your raster data for year of disturbance
data <- raster("Your_raster.tif")
tdf.gcs <- spTransform(Shpb, crs(data))
tdfDist <- extract(data, tdf.gcs)
polid <- tdf.gcs@data[,IDfld]
outnm <- file.path(RD, paste(prefl, "Dist.RData", sep= ""))
save(tdfDist, file=outnm)
Indic2 <- function(x){
  x[x>20] <- NA  #According to the variable, here you can set conditional for anomalies 
  Minx <- min(x, na.rm= T)
  Meanx <- mean(x, na.rm= T)
  Maxx <- max(x, na.rm= T)
  Stdx <- sd(x, na.rm= T)
  quant <- quantile(x, prob= c(0.1, 0.5, 0.9), na.rm= T)
  numx <- length(x)
  y <- data.frame(Meanx, Maxx, Stdx, numx, t(quant))
  return(y)
}

datat <- lapply(tdfDist, Indic2) 
datatt <- do.call(rbind, datat)
names(datatt) <- gsub("\\.", "", names(datatt))
datatt$polid <- paste(polid, rep(bfls, each= npol), sep= "_")
outfn <- file.path(RD, paste(prefl, "Dist.csv", sep= ""))
write.csv(datatt,outfn)

# You can load and prepare the raster data for another variables like Forest
# Integrity, Human Footprint Index, Tree Height, population density or anything
# else that have a unique raster and you want to know similar statistics about.