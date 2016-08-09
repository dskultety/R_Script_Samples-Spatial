#-------------------------------------------------------------------------------------
# Name:         Batch_Reproject_Shapefiles.R
# Type:         R script
# Purpose:      Batch reprojects and saves a new copy of all shapefiles in input directory. 
#                              
# Usage:		    This code is provided as an example only. Usage of this code requires
#				        understanding the intended purpose and all limitations and warnings.
#
# Inputs:       indirectory = input directory, default is working directory
#               outdirectory = output directory, default creates folder called "reproj" in working directory
#               extension = text appended to input file name, default is "_reproj" 
#               projargs = coordinate system to reproject to, default is WGS84 lat/long
#               overwrite_layer = overwrite outputs if they already exist, default = false
#
# Author:       Skultety
#
# Created:      03/28/2013
#
# Copyright:    (c) Skultety 2013
#
#-------------------------------------------------------------------------------------

batch_reproject_shapefiles <- function(indirectory=getwd(),outdirectory="reproj",extension="_reproj",
                                       projargs=CRS("+proj=longlat +datum=WGS84"),overwrite_layer=FALSE)
{
  # Load libraries
  require("sp")
  require("rgdal")
  require("foreach")
  
  # Get list of shapefiles in indirectorry
  filesList <- list.files(path=indirectory,pattern="\\.shp$")
  
  # Define function to handle individual shapefiles
  reproject_shapefiles <- function(files,indirectory,outdirectory,extension,projargs,overwrite_layer)
  {
    fileName <- sub(pattern=".shp",replacement="",x=files)
    shape <- readOGR(indirectory,fileName)
    shapeReproj <- spTransform(shape,projargs)
    outputName <- paste(fileName, extension, sep="")
    writeOGR(shapeReproj,dsn=outdirectory,layer=outputName,driver="ESRI Shapefile",overwrite_layer=overwrite_layer)
    return(shapeReproj)
  }
  
  # Process shapefiles using foreach loop
  outList <- foreach(files=filesList, .packages=c("sp","rgdal")
    ) %dopar% { reproject_shapefiles(files,indirectory,outdirectory,extension,projargs,overwrite_layer) }
  
  # Return list of reprojected spatial dataframe objects
  return(c("Shapefiles successfully reprojected"))
}