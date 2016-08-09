#-------------------------------------------------------------------------------------
# Name:         Reclassify_NLCD_Raster.R
# Type:         R script
# Purpose:      Reclassifies cell values in a National Land Cover Dataset raster in 
#               order to agregrate detailed land cover type to simple categories, for 
#               example reclassify all forest cover types to a single forest category. 
#               Output is saved as a tiff file.
#               
# Usage:		    This code is provided as an example only. Usage of this code requires
#				        understanding the intended purpose and all limitations and warnings.
#
# Inputs:       nlcd_file = input National Land Cover Dataset file to reclassify
#               outputName = output file name for reclassified raster
#               overwrite_layer = overwrites output if it already exists, default = false
#
# Warnings:     - Included land cover codes are only those found in the National Land Cover 
#                 Dataset (USGS 2011) for the northeastern portion of Illinois. Run with
#                 input raster that includes land cover codes not listed will result in 
#                 an error message.
#
# Author:       Skultety
#
# Created:      06/23/2014
#
# Copyright:    (c) Skultety 2014
#
#-------------------------------------------------------------------------------------

reclass_raster <- function(nlcd_file,outputName,overwrite_layer=FALSE)
{
  # Load required packages
  require("raster")
  require("rgdal")
  
  # Import raster
  land_cover_raster <- raster(nlcd_file)
  
  # Reclassify raster
  is <- c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
  becomes <- c(10,20,20,20,20,30,40,40,40,50,70,80,80,90,90)
  rcl <- cbind(is,becomes) 
  land_cover_reclass <- reclassify(x=land_cover_raster,rcl=rcl)
  
  # Export Raster
  writeRaster(x=land_cover_reclass,filename=outputName,format="GTiff",overwirte=overwite_layer)
  
  return(NULL)
}

### RUN FUNCTION

# setwd() to load data and save data

reclass_raster("nlcd_input.tif","nlcd_reclass.tif",overwrite_layer=FALSE)

