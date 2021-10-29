################################################################################
#           Distance to Settlements and Roads (for different years)            #
################################################################################

# Raster data: Euclidean distance from main highways (primary, secundary and 
# tertiary) and from Settlements. The map of highways can be downloaded from 
# Open StreetMaps and Settlements could be calculated from Urbanized categories
# in ESA Land Cover Maps or from https://ghsl.jrc.ec.europa.eu/.

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

# Prepare your raster data for Settlements
InD2 <- ("Your/File/Path/With/RasterData")
lsrst <- list.files(InD2, pattern="\\.tif$", full.names= T)
y= 0
for (i in lsrst){
  rs <- raster(i)
  rse <- extent(rs)
  if (y == 0) {
    data <- raster::stack(rs)
    datae <- extent(data)
    y <- 1
  } else{
    test <- sum((rse[1]== datae[1])*(rse[2]== datae[2])*(rse[3]== datae[3])
                *(rse[4]==datae[4]))
    if(test== 0){
      rs <- resample(rs, data, resample='ngb') 
    }
    data <- raster::stack(data, rs)
  }
}  #### Check before implementing. Rasters will be cut when resampled

save(data,file=file.path(RD,"AsentData.RData")) 

# Get distance to Settlements
tdf.gcs <- spTransform(Shpb,crs(data))
polid <- tdf.gcs@data[,IDfld]
flnm <- paste(prefl, "AsentData.RData", sep="_")
shpAsent <- extract(data, tdf.gcs)
save(shpAsent, file= file.path(RD, flnm)) # Save a backup of the raw data (.RData)

# Trendt and Indicator functions
# In Line 72 5 corresponds to the number of statistics calculated in Line 82
# In line 75 the numbers are the years of raster data (i.e. 70 = 1970, 100 = 2000, etc.)

trendt <- function(x){
  repn <- nrow(x)
  x <- as.vector(x)
  if(sum(is.na(x))>0){
    b <- rep(NA, 5)
  }
  else{
    xx <- data.frame(x, "year"= rep(c(70, 90, 100, 114), each= repn))
    names(xx)[1] <- c("value")
    m <- lm(value~year, data= xx)
    b <- summary(m)$coefficients[2,]
    b <- c(b, summary(m)$adj.r.squared)
  }
  #print(b)
  names(b) <- c("Estimate", "Std.Error", "tvalue", "Pr(>|t|)", "adjR2")
  return(b)
} 


Indic <- function(x){
  print(dim(x)[[1]])
  if(dim(x)[[1]]>1){
    Minx <- apply(x, 2, min, na.rm=T)
    Maxx <- apply(x, 2, max, na.rm=T)
    Stdx <- apply(x, 2, sd, na.rm=T)
    Meanx <- apply(x, 2, mean, na.rm=T)
    rangexp <- Minx/Maxx
    cofvx <- Stdx/Meanx
    trng <- trendt(rangexp)
    tstd <- trendt(Stdx)
    tcofv <- trendt(cofvx)
  }else{
    Minx <- Maxx <- Meanx <- unlist(x)
    Stdx <- rangexp <- cofvx <- rep(NA, 4)
    trng <- tstd <- tcofv <- rep(NA, 5)
  }
  tmean <- tryCatch({trendt(Meanx)}, error= function(err){rep(NA, 5)})
  xl <- c(Minx, Maxx, Stdx, Meanx, rangexp, cofvx, tstd, tmean, trng, tcofv)
  yr <- c(70, 90, 100, 114)
  var <- c("Min", "Max", "Std", "mean", "prang", "coefv")
  nm1 <- as.vector(t(outer(var, yr, paste, sep="")))
  nm2 <- as.vector(t(outer(var[3:6], names(tcofv), paste, sep= "")))
  names(xl) <- c(nm1, nm2)
  return(xl)
}  

# Apply trendt and indicator functions to the data

pDisAsen <- lapply(shpAsent, Indic)
pDisAsen2 <- data.frame(do.call(rbind, pDisAsen))
pDisAsen2$Site <- paste(polid, rep(bfls, each= npol), sep= "_")
outfn <- file.path(RD, paste(prefl, "DisAsen.csv", sep= "_"))
write.csv(pDisAsen2, outfn)

# You can load and prepare the raster data for roads and repeat this same code
# to get the same indicators.
