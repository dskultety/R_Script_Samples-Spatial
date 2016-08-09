#-------------------------------------------------------------------------------------
# Name:         Land_Cover_Extract.R
# Type:         R script
# Purpose:      Extracts land cover raster cell values for raster cells that intersect 
#               polygons and calculates area in square meters.  Each input polygon is 
#               evaluated seperately, therefore input polygons can be overlapping. 
#               Output is of class dataframe, columns are land cover categories, rows
#               are sites.
#               
# Usage:		    This code is provided as an example only. Usage of this code requires
#				        understanding the intended purpose and all limitations and warnings.
#
# Inputs:       landCoverRaster = input land cover raster
#               buffer = polygons for which land cover is to be determined, object of class
#                 SpatialPolygonsDataFrame
#
# Warnings:     - All inputs must be in the same coordinate system
#               - Assumes that input polygon feature has attribute field called "Site_ID"
#               - Assumes that input land cover raster has a cell size of 30 meters by 
#                 30 meters
#
# Author:       Skultety
#
# Created:      10/20/2014
#
# Copyright:    (c) Skultety 2014
#
#-------------------------------------------------------------------------------------

extractLandCover <- function(landCoverRaster, buffer) 
{
  # Load required packages
  require("raster")
  require("gtools")
  
  # Create empty data frame
  landCoverOut <- data.frame(NULL)
  
  for(row in 1:nrow(buffer))
  {
    # Get Site_ID for row
    siteID <- as.vector(buffer[row,]$Site_ID)
    # Extract land cover for buffer
    landCover <- extract(x=landCoverRaster,y=buffer[row,],small=TRUE,df=TRUE,weights=TRUE)
    # Add column and convert weights (fractions of covered cells) to square meters
    landCover <- cbind(landCover, landCover$weight * 900)
    # Add clearer column names
    names(landCover) <- c("ID","Code","Weight","Sq_Meters")
    # Summarize output
    landCoverSimp <- aggregate(Sq_Meters ~ Code, data = landCover, FUN = sum)
    # Transpose rows and columns
    landCoverSimp <- t(landCoverSimp)
    # Rename columns with land cover codes
    colnames(landCoverSimp) <- landCoverSimp[1,]
    # Extract row with area, removes row with redunant column names
    landCoverSimp <- subset(landCoverSimp, rownames(landCoverSimp) == "Sq_Meters")
    # Add column with Site_ID
    landCoverSimp <- cbind(as.data.frame(siteID),landCoverSimp)
    # Bind land cover for each polygon to data frame   
    landCoverOut <- smartbind(landCoverOut, landCoverSimp)
  }
  # Remove non-unique rows, smartbind with null dataframe creates duplicate of first row
  landCoverOut <- unique(landCoverOut)
  # Create row names from siteID column
  rownames(landCoverOut) <- landCoverOut$siteID
  # Remove unneeded siteID column
  landCoverOut <- landCoverOut[,2:ncol(landCoverOut)]
  # Put columns in numeric order of land cover classes
  landCoverOut <- landCoverOut[,order(names(landCoverOut))]
  # Return data frame of land covers in square meters
  return(landCoverOut)
}

### LOAD DATA & RUN FUNCTION

# Packages required to load data
require("raster")
require("rgdal")

# setwd() to load data

# Load land cover data as an object of class "raster" 
land_cover_raster <- raster("nlcd_clipped_Chicago_1mi_buffer_utm16_reclass_v2.tif")

# Load polygons from shapefile
buffers <- readOGR(dsn = getwd(), layer = "100_Meter_Buffers")

# Run on testing data
land_cover_buffers <- extractLandCover(land_cover_raster,buffers)

