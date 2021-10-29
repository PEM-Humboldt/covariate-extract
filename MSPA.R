################################################################################
#             Morphological Spatial Pattern Analysis (2000-2013)               #
################################################################################

# Raster data: Fragmentation categories calculated with Hansen forest cover for 
# 2000 and 2013. The algorhitm MSPA can be found here: 
# https://forest.jrc.ec.europa.eu/en/activities/lpa/mspa/

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

# Functions Reclass and INdic to process data
reclasMSPA<-function(x){
  inc <- c(0, 1, 3, 5, 9, 17, 33, 35, 37, 65, 67, 69, 129)
  #MPSA classes: novalue,branch,edge,perforation,islet,core,bridge,bridge in edge,
  #bridge in perforation, loop,loop in edge,loop in perforation,nodata)
  #the order from low to high fragmentation is now: core,perforation,loop,
  #bridge,edge,branch,islet
  newc <- c(NA, 2, 3, 50, 1, 100, 12, 11, 13, 16, 15, 17, NA)
  y <- newc[match(x, inc)]
}

Indic3<-function(x){
  x[x==0] <- NA
  Minx <- min(x, na.rm= T)
  Meanx <- mean(x, na.rm= T)
  Maxx <- max(x, na.rm= T)
  Stdx <- sd(x, na.rm= T)
  quant <- quantile(x, prob= c(0.1, 0.5, 0.9), na.rm= T)
  cofv <- Meanx/Stdx
  npix <- length(x)
  y <- data.frame(Meanx, Maxx, Stdx, cofv, t(quant), npix)
  return(y)
}

# Prepare your raster data for MSPA and apply functions
InD2 <- ("Your/File/Path/With/RasterData")
lsdir <- list.dirs(InD2)
lsdir <- lsdir[grep("_17_25_11", lsdir)]

y=0
for (j in lsdir){
  nmy <- gsub("(^.*_)([0-3]{4})(_.*$)", "\\2", j)
  lsrst <- list.files(j, pattern= "\\.tif$", full.names= T)
  lsrst <- lsrst[grep("_17_25_11_[2,3,4,5]", lsrst)]
  for (i in 1:4){
    rs <- raster(lsrst[i])
    assign(paste("rs", i, sep=""), rs)
  }
  rs <- raster::mosaic(rs1, rs2, rs3, rs4, fun= max)
  tdf.gcs <- spTransform(Shpb, crs(rs))
  data <- extract(rs, tdf.gcs)
  outnm <- file.path(RD, paste(prefl, nmy, "MSPA.RData", sep=""))
  save(data, file=outnm )
  data2 <- lapply(data, reclasMSPA) #reclassify values to reflect fragmentation
  save(data2, file= outnm)
  datat <- lapply(data2, Indic3)
  datatt <- do.call(rbind, datat)
  names(datatt) <- gsub("\\.", "", names(datatt))
  names(datatt) <- paste(names(datatt), substr(nmy, 3, 4), sep= "_")
  if (y == 0) {
    polid <- tdf.gcs@data[,IDfld]
    datatt$polid <- paste(polid, rep(bfls, each= npol), sep= "_") 
    datattt <- datatt
    y <- 1
  } else{
    datattt <- cbind(datattt,datatt)
  }
}

# Comparing results between two years
datattt$dynM <- datattt$Meanx_13 - datattt$Meanx_00
datattt$dynSt <- datattt$Stdx_13 - datattt$Stdx_00
datattt$dynCf <- datattt$cofv_13 - datattt$cofv_00
outfn <- file.path(RD, paste(prefl, "MSPA.csv", sep= ""))
write.csv(datattt,outfn)

