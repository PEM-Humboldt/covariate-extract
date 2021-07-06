# Covariate-Extract

Extract spatially explicit variables associated with the measurement of ecosystem quality from vector information.

# Getting started

To run the following repository, no special installation is required, other than the packages necessary to prepare and extract the data from raster files.

# Prerequisites

This routine is developed in R language, so you will have to install R with a version 3.5 or higher and a code editor (for example RStudio). You also need to install the following package from CRAN repository:

``` 
library(raster)
library(sp)
library(rgdal)
library(reshape2)
library(rgeos)
library(ggplot2)
library(mgcv)
library(devtools)
```
# Installing and Running

Once you have installed R and the packages, the routine will show you a method to extract different variables of a raster file from vectorial data as points or polygons. According to the selected variable, the routine will calculate different statistics to summary the information. At the moment this routine only extract and prepare the spatial data and it is the user's responsibility to use this data to carry out another kind of analysis. The variables available until today are:
1. **ESAcov:** Coverage transformation indicator to 300 m2 from 1992 to 2015. The first columns indicate the coverage code by year. The columns Loss, Gain, Permanence indicate the percentage of the area with loss, gain, or permanence of natural coverage between 1992 and 2015. With these percentages, a coverage condition indicator, shpESAI, is calculated, defined by the following formula (((permanence + ( gain / 2) +1) / (loss + 1)) - 1) / 23. High values ​​indicate better conservation.
2. **AsentData y dViasTren:** They are two similar files, one with the indicators from the distance to settlements (from 1970 to 2014) and the other with the distance to roads, same period. The tables present the minimum distance for each period; for shp with more than one pixel the statistics of each distance are presented. The columns Estimate, Std.Error, tvalue, P less than 0.05, AdjR2 are the attributes of the linear model that estimates the trend of change of the minimum distances between 1970 and 2014.
3. **Dist**: compiles statistics derived from the disturbance year estimate with data from Matt Hansen (University of Maryland) from 2000 to 2016.The statistics are for the years of the disturbance. In the original data, the value from 0 to 16 indicates the year of the disturbance, zero indicates no disturbance, and 16 indicates that the last disturbance detected was in 2016. The mean and standard deviation statistics are indicators of the prevalence of the disturbance, with corresponding low statistics to areas less disturbed or of less spatial variability in the disturbance.
4. **FInt**: This is the indicator developed by Andrew Hansen that combines three variables: Percentage of Cover, Height of the Canopy, and year since the disturbance. The combination generates 27 forest classes with lower values ​​indicating low integrity and high values ​​indicating high integrity. All of this is from remote sensing. The table presents the indicator and associated statistics for the study area.
5. **Other possible indicadors:** Human density, Spatial Human Footprint (as a measure of direct human pressure)

# Authors

1. Susana Rodróiguez Buriticá
2. Luis Hernando Romero Jiménez 

# License

This project is licensed under the MIT License - see the LICENSE.md file for details
