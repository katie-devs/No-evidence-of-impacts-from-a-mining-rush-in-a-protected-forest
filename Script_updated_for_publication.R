#     Load libraries

install.packages("terra")
install.packages("Synth")
install.packages("dplyr")
library("terra")
library("Synth")
library("dplyr")
library("ggplot")


#     Load vector data - HYDROBASINS Level 9 Drainage basins shapefiles for Madagascar and outline of Province of Toamasina. 
# Basins shapefile includes variable values and area of deforestation per year for each basins (this was extracted in ArcGIS) 

Toam <- vect("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/Madagascar Admininstrative Boundaries/Toamasina.shp")

basins <- vect("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Chapter 3_Impact_Eval_Bemainty_Gem_Rush/Bemainty_Impact_Eval/Mada_Basins_Lev9_edited_final.shp")

basins <- basins[, -c(27,28)]

# Delete Tab_D and Tab_Area1 as there were 'Turned off' on the ArcGIS layer as not useful
# col_names is right. Change dataframe names to match.

col_names <- c("PFAF_ID","Shape_length","Area_m2","Area_km2","Pop_density_2011","Pop_growth_2001_2011","River_density","Perc_for_2011","Area_m2","Mean_Dist_Sett","Pop_count_2011","Pop_count_2001","Mean_elevation","Mean_slope","Std_Slope","Mean_dist_road","Mean_dist_tracks","Mean_dist_rivers","Length_rivers_m","For_Area_2011_m2","Overlap_PAs","Perc_overlap_PAs","Overlap_offsets","Perc_overlap_offsets","Mean_dist_edge","mean_annual_rain","VALUE_1984","VALUE_1985","VALUE_1987","VALUE_1988","VALUE_1989","VALUE_1990","VALUE_1991","VALUE_1992","VALUE_1993","VALUE_1994","VALUE_1995","VALUE_1996","VALUE_1997","VALUE_1998","VALUE_1999","VALUE_2000","VALUE_2001","VALUE_2002","VALUE_2003","VALUE_2004","VALUE_2005","VALUE_2006","VALUE_2007","VALUE_2008","VALUE_2009","VALUE_2010","VALUE_2011","VALUE_2012","VALUE_2013","VALUE_2014","VALUE_2015","VALUE_2016","VALUE_2017","VALUE_2018","VALUE_2019","VALUE_2020","VALUE_2021")

names(basins) <- col_names


# Now there are two options:

# Option 1: Load large, pre-saved data files where annual forest cover and degradation area totals have already 
#           been extracted within each basin unit (these files are available on GitHub to download). Run this then skip to line 562.

# Option 2: Run the full code, including the calculation of the above layers. Warning - this takes a lot time and has very high memory requirements


# ******************************* OPTION 1 *******************************************************************# 


# Load large, pre-saved data files (annual forest cover and degradation area per basin) 

For_Area_ext2 <- read.csv("For_Area_ext2_21_04.csv")  

Deg_Area_ext2 <- read.csv("Outputs_3/Deg_Area_ext2_21_04.csv")  


### Join For_Area_ext layer back to basins shapefile

basins <- terra::merge(basins, For_Area_ext2, by.x = "PFAF_ID", by.y = "PFAF_ID")

names(basins)

basins <- basins[, -c(27:64)]         # Delete duplicate columns

basins <- basins[order(basins$Row.names, basins$Year), ]       # Order by row names and year.      

# Delete extra area_m2 value

basins <- basins[, -9]

basins$Annual_defor_ha <- basins$Annual_defor_m2/10000      # Calculate annual deforestation in hectares

basins$Area_ha <- basins$Area_m2_1/10000                    # Calculate area of the basin in hectares

basins <- basins[, c(1:4,96, 5:30, 95, 31:94)]

writeVector(basins, "basins_merged.shp", filetype = "ESRI Shapefile", overwrite = TRUE)

# Clip basins shapefile to boundary of Province of Toamasina to reduce size. Also because degradation area was only
# calculated for basins in this province because of memory limitations.


Toam <- project(x = Toam, y = "EPSG:32738")

intersect_Toam <- relate(basins, Toam, "intersects", pairs = T)

intersect_Toam <- intersect_Toam[, 1]

basins_Toam <- basins[intersect_Toam, ]

plot(Toam)
plot(basins_Toam, col = "red", alpha = 0.5, add = T)

basins_Toam <- basins_Toam[order(basins_Toam$Row.names, basins_Toam$Year), ]       # Order by row names and year.      

# Merge Deg_Area_ext2 data to basins_Toam and tidy.


basins_Toam <- terra::merge(basins_Toam, Deg_Area_ext2, by.x = c("PFAF_ID", "Year"), by.y = c("PFAF_ID", "Year"))

length(which(basins_Toam$Annual_degradation_m2 == Deg_Area_ext2$Annual_degradation_m2))  # They are the same
length(which(basins_Toam$PFAF_ID == Deg_Area_ext2$PFAF_ID))  # They are the same

names(basins_Toam)

basins_Toam <- basins_Toam[, -c(97:102)]  # Remove duplicate columns

basins_Toam$Annual_degradation_ha <- basins_Toam$Annual_degradation_m2/10000      # Convert annual degradation to hectares

basins_Toam$Annual_degradation_rate <- 0

basins_Toam$Annual_degradation_rate <- (basins_Toam$Annual_degradation_m2/basins_Toam$Initial_for_cover)*100        # Calculate annual degradation rate

basins_Toam <- basins_Toam[, c(1:96, 98:129, 97, 130, 131)]           

options(scipen = 999999)                           

# Change Row.names.x to Row.names

names(basins_Toam)[28] <- "Row.names"

# Remove observations for 1990 because I don't have an annual deforestation rate for that year.

basins_Toam <- basins_Toam[basins_Toam$Year != 1990, ]

View(values(basins_Toam))

## Remove Perc_For_2011 and Area_For_2011_m2 variables from basins_Toam as these were calculated using TMF data which was not masked
## to Vieilledent et al 2018, For1990 layer. Replace with For2011_Area_m2, as this layer was masked.

names(basins_Toam)

basins_Toam <- basins_Toam[, -c(10, 21)]           

basins_Toam$Perc_For_2011 <- (basins_Toam$For2011_Area_m2/basins_Toam$Area_m2_1.x)*100

writeVector(basins_Toam, "basins_Toam_21_04.shp", filetype = "ESRI Shapefile", overwrite = TRUE)

plot(vect("Madagascar_RP.shp"), col = "grey", ext = ext(basins_Toam))
plot(rast("Outputs_3/TMF_For2011.tif"), col = "dark green", add = T)
polys(basins_Toam, border = "grey30", lwd = 0.5)
polys((basins_Toam[basins_Toam$PFAF_ID == 181950460, ]), border = "yellow", lwd = 1)
#polys(Toam, border= "red", lwd = 1.5)

View(values(basins_Toam[basins_Toam$PFAF_ID == 181950460, ]))


## The Bemainty sub-basin Row.name = 1907! ##

# Convert basins_Toam shapefile to dataframe to use in Synthetic control analysis.

basins_Toam_df <- as.data.frame(basins_Toam)

# Check number of unique basins

length(unique(basins_Toam_df$PFAF_ID))    # 336.



#                  ** SKIP TO LINE 562 **




# **************************************************************


#------------------------------ 1. Calculate Deforestation and Degradation per sub-basin --------------------------------------------------------#


#                 1a) To calculate the annual defor/degradation *rate* I need to calculate the area of forest in each sub-basin for each year 1990-2021. 
#                     - Create annual forest cover layers from the TMF data.


# Area of forest obtained from the TMF Annual Change layers. 
#### Annual Change Collection accessible from https://forobs.jrc.ec.europa.eu/TMF/data#downloads. Annual change collection downloaded for tiles 40E, 10S; 40E, 10S; and 40E, 20S, for all years 1990 - 2021

# Load annual change layers. Merge the 3 layers representing Madagascar for each year. Clip to the extent of Toamasina province (to reduce processing size). Reclassify to leave only forest cover. Reproject

# Create matrix of old and new cell values to use in reclassify

rcl <- as.data.frame(matrix(nrow = 6, ncol = 2))
rcl[,1] <- 1:6
rcl$V2[(rcl$V1 == 1)|(rcl$V1 == 2)|(rcl$V1 == 4)] <- 1

# Vector of years to loop through

year <- 1990:2021


# Create function to merge, clip, reclassify and reproject the 3 TMF raster layers for each year. 
# Annual Change raster has 6 values. From layer symbology (https://forobs.jrc.ec.europa.eu/TMF/data.php) 
# 1= Undisturbed tropical moist forest
# 2= Degraded tropical moist forest
# 3= Deforested Land
# 4= Tropical Moist Forest regrowth
# 5= Permanent and Seasonal water
# 6 = Other Land cover
# Reclassify to remove 3,5,6. Change to NA. 

read_rast <- function(year){
  
  path1 <- paste("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/Tropical Moist Forests/Complete_Annual_Change/forobs/products/tmf_v1/AnnualChange/JRC_TMF_AnnualChange_v2_",year,"_AFR_ID6_S20_E40.tif", sep = "")
  path2 <- paste("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/Tropical Moist Forests/Complete_Annual_Change/forobs/products/tmf_v1/AnnualChange/JRC_TMF_AnnualChange_v2_",year,"_AFR_ID19_S10_E40.tif", sep = "")
  path3 <- paste("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/Tropical Moist Forests/Complete_Annual_Change/forobs/products/tmf_v1/AnnualChange/JRC_TMF_AnnualChange_v2_",year,"_AFR_ID20_S10_E50.tif", sep = "")
  
  bottom <- rast(path1)
  mid <- rast(path2)
  right <- rast(path3)
  
  # Merge
  
  merged <- merge(bottom,mid,right, overwrite = TRUE)    
  
  # Clip to extent of Toamasina like I did for the deforestation year data. 
  
  merg_clipped <- crop(merged, Toam, overwrite = TRUE)
  
  # Reclassify to leave only forested cells
  
  merg_clipped_rc <- classify(merg_clipped, rcl, right = NA, overwrite = TRUE)
  
  merg_clipped_rc_rp <- project(x = merg_clipped_rc, y = "EPSG:32738", method = "near", overwrite = TRUE)
  
  f <- paste("Outputs_2/TMF_For",year,".tif", sep = "")
  
  writeRaster(merg_clipped_rc_rp, filename = f, datatype= "INT2U", overwrite = TRUE)
  
  tmpFiles(remove = TRUE)
  
}


# Run function for each of the 32 years in the time series.

for(i in year){
  read_rast(i)
}


# I should have masked the TMF Annual Change layers to Vieilledent et al (2018) forest cover 1990 layer for Madagascar. Do now.
# Loop to read in forest cover layers in Outputs_2, mask to For_1990 and re-save:

# First, test:

For_1990 <- rast("for1990.tif")

mask_rast <- function(year){
  x<- rast(paste("Outputs_2/TMF_For",year,".tif", sep= ""))
  y <- mask(x, For_1990)
  path <- paste("Outputs_3/TMF_For",year,".tif", sep = "")
  writeRaster(y, filename = path, datatype= "INT2U", overwrite = TRUE)
}

for(i in year){
  mask_rast(i)
}



#                         1b) Extract forest cover per sub-basin per year


basins_df <- data.frame(basins)

# Condense data frame to only include PFAF_ID and the count of deforestation per year. Then extract forest cover per basin
# per year using the shapefile and join back to this dataframe. Calculate annual defor rate (%) by dividing annual deforestation by forest cover at end of previous year. 

basins_df_2 <- basins_df[, c(1, 32:63)]

year <- 1990

# Create first dataframe and join the rest to it through loop.

For90 <- rast("Outputs_3/TMF_For1990.tif")

Pixel_area <- res(For90)[1] * res(For90)[2]

For_90_ext <- extract(For90, basins, na.rm = TRUE)      
For_90_tab <- as.data.frame(table(For_90_ext))          # Table counts the number of pixels per sub-basin, labelled by ID. ID corresponds to the row.names in the original basins vector.  
For_90_tab$For1990_Area_m2 <- For_90_tab$Freq * Pixel_area          # Convert count of pixels to area in m2
For_90_tab <- For_90_tab[, -c(2,3)]

options(scipen = 999)
 
# Join the extracted forest data to the basins dataframe (which includes the deforestation data) to create new df.

For_Area_ext <- merge(basins_df_2, For_90_tab, by.x = "row.names", by.y = "ID", all = TRUE, sort = TRUE)
For_Area_ext$Row.names <- as.numeric(For_Area_ext$Row.names) 
For_Area_ext <- For_Area_ext[order(For_Area_ext$Row.names), ]       # Order

length(which(For_Area_ext$PFAF_ID == basins_df$PFAF_ID))            # Check that the order matches 

# Now repeat for all the other years and join to the For_Area_ext data

years2 <- 1991:2021

# Function to extract forest area for the inputted year for all basins and add to the For_Area_ext layer 

Extract_func <- function(year){
  x <- rast(paste("Outputs_3/TMF_For",year,".tif", sep = ""))
  y <- extract(x, basins, na.rm = TRUE)
  z <- as.data.frame(table(y))
  col_name <- paste("For",year,"_Area_m2", sep = "")
  z$new_col <- z$Freq * Pixel_area
  names(z)[4] <- col_name
  # in z ID corresponds to the row number in basins. 
  z <- z[, -c(2,3)]
  z$ID <- as.numeric(z$ID)
  For_Area_ext <- merge(For_Area_ext, z, by.x = "Row.names", by.y = "ID", all = TRUE, sort = TRUE)
  return(For_Area_ext)
}

# Loop to run function for all years.

for(i in years2){
  For_Area_ext <- Extract_func(i)
  gc()
}

write.csv(For_Area_ext, "Outputs_3/For_cover_by_basin_21_04.csv")



# Reformat data to panel data format where each PFAF_ID (representing each unique drainage basin) has a row for each year of the study period. Forest cover and annual forest loss transposed to columns

For_Area_ext2 <- For_Area_ext %>% slice(rep(1:n(), each = 32))   # Repeat each row 32 times

For_Area_ext2$Year <- rep(1990:2021, 2307)    # Add column for year

For_Area_ext2 <- For_Area_ext2[, c(1,2,67,3:66)]  # Re-order columns

For_Area_ext2$Annual_defor_m2 <- 0

For_Area_ext2$Initial_for_cover <- 0

For_Area_ext2 <- For_Area_ext2[, c(1:3,68,69, 4:67)]


# Write loop to transpose annual deforestation per year in rows to a column (Annual_defor_m2). Iterate over each sub-basin by using rows (in index) where Year = 1990.

index <- which(For_Area_ext2$Year == "1990")             

# For annual deforestation

for(i in index){
  For_Area_ext2$Annual_defor_m2[i:nrow(For_Area_ext2)] <- as.numeric(t(For_Area_ext2[i, 6:37]))
  }

View(For_Area_ext2[For_Area_ext2$PFAF_ID == 181937204, c(3, 4,6:37)])

# For initial forest cover 

index2 <- which(For_Area_ext2$Year == "1991")              # Forest cover values refer to forest cover at the END of that year. So I can't calculate the Annual deforestation rate for 1990 as I don't 

i <-2
                                                          # have forest cover values for the start of 1990, only the end. Therefore I can only calculate the annual deforestation rate from 1991.  
for(i in index2){
  For_Area_ext2$Initial_for_cover[i:(i + 30)] <- as.numeric(t(For_Area_ext2[i, 38:68]))
}


### I need to calculate annual forest loss as a % of forest cover at the start of the year. Forest cover layers represent forest cover at the END of that year.
### So need to calculate annual forest loss as a % of forest cover at the end of the previous year. 

# Calculate the Annual_defor_rate

For_Area_ext2$Annual_defor_rate <- (For_Area_ext2$Annual_defor_m2/For_Area_ext2$Initial_for_cover)*100

For_Area_ext2 <- For_Area_ext2[ , c(1:5, 70, 6:69)]

write.csv(For_Area_ext2, "For_Area_ext2_21_04.csv")


### Join For_Area_ext layer back to basins shapefile

basins <- terra::merge(basins, For_Area_ext2, by.x = "PFAF_ID", by.y = "PFAF_ID")

names(basins)

basins <- basins[, -c(27:64)]         # Delete duplicate columns

basins <- basins[order(basins$Row.names, basins$Year), ]       # Order by row names and year.      

# Delete extra area_m2 value

basins <- basins[, -9]

basins$Annual_defor_ha <- basins$Annual_defor_m2/10000      # Calculate annual deforestation in hectares

basins$Area_ha <- basins$Area_m2_1/10000                    # Calculate area of the basin in hectares

basins <- basins[, c(1:4,96, 5:30, 95, 31:94)]

writeVector(basins, "basins_merged.shp", filetype = "ESRI Shapefile", overwrite = TRUE)



##                          1c) Upload Annual Degradation data and extract within sub-basins


# This was too computationally expensive to do for the whole of Madagascar so I extracted degradation data for basins in 
# Toamasina province. 

# Clip the basins shapefile (with inludes the annual forest cover and deforestation columns) to the extent of Toamasina.

# Select sub-basins which intersect with the Toamasina province: 

Toam <- project(x = Toam, y = "EPSG:32738")

intersect_Toam <- relate(basins, Toam, "intersects", pairs = T)

intersect_Toam <- intersect_Toam[, 1]

basins_Toam <- basins[intersect_Toam, ]

plot(Toam)
plot(basins_Toam, col = "red", alpha = 0.5, add = T)

basins_Toam <- basins_Toam[order(basins_Toam$Row.names, basins_Toam$Year), ]       # Order by row names and year.      

length(unique(basins_Toam_df$PFAF_ID))

# There are 336 basins which intersect or are within Toamasina province.


                        # Upload adapted TMF Annual Disturbance disturbance data (edited from the TMF Annual Disruptions data layers in GEE). See SI for details on creation of this dataset. 


# setwd("D:/PhD/Data/Tropical Moist Forests product/Annual_Disruptions-Edited/TMF_Annual_Disturbance")

# Layers from 2013 onwards are split into different tiles but the 00000000-0000000 version covers the whole of Toamasina province so just use that.

Deg90 <- rast("Annual_Disruptions_1990_masked.tif")
res(Deg90)

Pixel_area2 <- 30*30

rcl2 <- as.data.frame(matrix(nrow = 2, ncol = 2))
rcl2[,1] <- c(0,1)
rcl2$V2[2] <- 1

Deg90 <- classify(Deg90, rcl2, right = NA, overwrite = TRUE)    # Reclassify to remove 0 (no degradation) values which makes extract and table functions too big for R to handle

Deg90_ext <- extract(Deg90, basins_Toam, na.rm = TRUE)          # Extract degraded pixels per basin
Deg90_tab <- as.data.frame(table(Deg90_ext))                    # Sum the number of degraded pixels per sub-basin

Deg90_tab$Deg1990_area_m2 <- Deg90_tab$Freq * Pixel_area2       # Convert the count of pixels to area
Deg90_tab <- Deg90_tab[, -c(2,3)]

options(scipen = 999)

# Join the extracted degradation data to the basins_Toam dataframe

basins_Toam_df <- as.data.frame(basins_Toam)
basins_Toam_df <- basins_Toam_df[, 1:4]

Deg_Area_ext <- merge(basins_Toam_df, Deg90_tab, by.x = "row.names", by.y = "ID", all = TRUE, sort = TRUE)
Deg_Area_ext$Row.names <- as.numeric(Deg_Area_ext$Row.names) 
Deg_Area_ext <- Deg_Area_ext[order(Deg_Area_ext$Row.names), ]       # Order

length(which(Deg_Area_ext$PFAF_ID == basins_Toam_df$PFAF_ID))            # Check that the order matches 

# Now repeat for all the other years and join to the For_Area_ext data

years2 <- 1991:2021

Extract_func2 <- function(year){
  x <- rast(paste("Annual_Disruptions_",year,"_masked.tif", sep = ""))
  x <- classify(x, rcl2, right = NA, overwrite = TRUE)
  y <- extract(x, basins_Toam, na.rm = TRUE)
  z <- as.data.frame(table(y))
  col_name <- paste("Deg",year,"_Area_m2", sep = "")
  z$new_col <- z$Freq * Pixel_area2
  names(z)[4] <- col_name
  # in z ID corresponds to the row number in basins. 
  z <- z[, -c(2,3)]
  z$ID <- as.numeric(z$ID)
  Deg_Area_ext <- merge(Deg_Area_ext, z, by.x = "Row.names", by.y = "ID", all = TRUE, sort = TRUE)
  return(Deg_Area_ext)
}

for(i in years2){
  Deg_Area_ext <- Extract_func2(i)
}


# Reformat data to panel data format where each PFAF_ID has a row for each year of the study period with a column for forest cover and forest loss

Deg_Area_ext2 <- Deg_Area_ext %>% slice(rep(1:n(), each = 32))   # Repeat each row 32 times

Deg_Area_ext2$Year <- rep(1990:2021, 336)    # Add column for year

Deg_Area_ext2 <- Deg_Area_ext2[, c(1:6, 39, 7:38)]

Deg_Area_ext2$Annual_degradation_m2 <- 0

Deg_Area_ext2 <- Deg_Area_ext2[, c(1:7, 40, 8:39)]


# Write loop to transpose annual degradation per year within rows to a column. Iterate over each sub-basin by using rows (in index) where Year = 1990.

index_deg <- which(Deg_Area_ext2$Year == "1990")             

# For annual degradation

for(i in index_deg){
  Deg_Area_ext2$Annual_degradation_m2[i:(i+31)] <- as.numeric(t(Deg_Area_ext2[i, 9:40]))  
}

View(Deg_Area_ext2[Deg_Area_ext2$PFAF_ID == 181940506, 7:40])


# Join back to basins_Toam shapefile

Deg_Area_ext2 <- Deg_Area_ext2[order(Deg_Area_ext2$Row.names, Deg_Area_ext2$Year), ]    # Order by row names and year.      

write.csv(Deg_Area_ext2, "Outputs_3/Deg_Area_ext2_21_04.csv")

basins_Toam <- terra::merge(basins_Toam, Deg_Area_ext2, by.x = c("PFAF_ID", "Year"), by.y = c("PFAF_ID", "Year"))

length(which(basins_Toam$Annual_degradation_m2 == Deg_Area_ext2$Annual_degradation_m2))  # They are the same
length(which(basins_Toam$PFAF_ID == Deg_Area_ext2$PFAF_ID))  # They are the same


basins_Toam <- basins_Toam[, -c(97:102)]  # Remove duplicate columns

basins_Toam$Annual_degradation_ha <- basins_Toam$Annual_degradation_m2/10000      # Convert annual degradation to hectares

basins_Toam$Annual_degradation_rate <- 0

basins_Toam$Annual_degradation_rate <- (basins_Toam$Annual_degradation_m2/basins_Toam$Initial_for_cover)*100        # Calculate annual degradation rate

basins_Toam <- basins_Toam[, c(1:96, 98:129, 97, 130, 131)]           

options(scipen = 999999)                           

# Change Row.names.x to Row.names

names(basins_Toam)[28] <- "Row.names"

# Remove observations for 1990 because I don't have an annual deforestation rate for that year.

basins_Toam <- basins_Toam[basins_Toam$Year != 1990, ]


## Remove Perc_For_2011 and Area_For_2011_m2 variables from basins_Toam as these were calculated using TMF data which was not masked
## to Vieilledent et al 2018, For1990 layer. Replace with For2011_Area_m2, as this layer was masked.

names(basins_Toam)

basins_Toam <- basins_Toam[, -c(10, 21)]           

basins_Toam$Perc_For_2011 <- (basins_Toam$For2011_Area_m2/basins_Toam$Area_m2_1.x)*100

writeVector(basins_Toam, "basins_Toam_21_04.shp", filetype = "ESRI Shapefile", overwrite = TRUE)

plot(vect("Madagascar_RP.shp"), col = "grey", ext = ext(basins_Toam))
plot(rast("Outputs_3/TMF_For2011.tif"), col = "dark green", add = T)
polys(basins_Toam, border = "grey30", lwd = 0.5)
polys((basins_Toam[basins_Toam$PFAF_ID == 181950460, ]), border = "yellow", lwd = 1)
#polys(Toam, border= "red", lwd = 1.5)

View(values(basins_Toam[basins_Toam$PFAF_ID == 181950460, ]))

## The Bemainty sub-basin Row.name = 1907! ##

# Convert basins_Toam shapefile to dataframe to use in Synthetic control analysis.

basins_Toam_df <- as.data.frame(basins_Toam)


# When re-running, run straight through the code from here.


## -----------------------------------------Synthetic Control -------------------------------------------------------##

# I ran analyses:
# - For two different outcomes (deforestation and degradation)
# - And three different ways of measuring outcomes (rate, raw and cumulative)
#-  at two spatial scales (first for the CAZ and second for the wider province of Toamasina)



##                           1) Using only basins from CAZ for donor pool


##                                  1a) Data Preparation

# Select basins.  

CAZ <- vect("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/PAs_Jorge_data/CAZ.shp")

# First select sub-basins which intersect the CAZ

intersect_CAZ <- relate(basins_Toam, CAZ, "intersects", pairs = T)

# Subset basins_Toam to select only ones which intersect the CAZ.

intersect_CAZ <- intersect_CAZ[, 1]

basins_CAZ <- basins_Toam[intersect_CAZ, ]
    
unique(basins_CAZ$PFAF_ID)            # This leaves 47 basins

plot(basins_CAZ)
plot(CAZ, add = T, col = "red", alpha = 0.5)

unique(basins_CAZ$PFAF_ID[basins_CAZ$Perc_For_2011 > 70])

# Subset basins to include only ones where Perc_For_2011 is greater than 70%.

basins_CAZ <- basins_CAZ[basins_CAZ$Perc_For_2011 > 70, ]

# Subset basins to include only those where Perc_overlap_PAs < 10. This removes the basin in the South which overlaps with Mantadia NP and CFAM - 181950692

basins_CAZ <- basins_CAZ[basins_CAZ$Perc_overlap_PAs < 10, ]

unique(basins_CAZ$PFAF_ID[basins_CAZ$Perc_overlap_PAs > 10])



      # 10 basins left

# Subset basins to include only those where Perc_overlap_offsets is less than (<) 10.

basins_CAZ <- basins_CAZ[basins_CAZ$Perc_overlap_offsets < 10, ]

      # This leaves 10 basins including Bemainty.


# Subset basins to include those which don't contain any other known mine sites.

Mine_sites <- vect("C:/Users/ktd19ycv/OneDrive - Bangor University/Documents/Data/Mine_sites/Mine_sites_v3_to_visualise_incl_VP.shp")

# Remove mine sites in Bemainty basin from this because I want to keep Bemainty basin.

Mine_sites <- subset(Mine_sites, Mine_sites$Name != c("Ambodipaiso", "Tananarive"))

# Find out which basins contain mine sites.

intersect_mines <- relate(basins_CAZ, Mine_sites, "contains", pairs = TRUE)

intersect_mines <- intersect_mines[, 1]

# Use this to subset the basins data, removing this basin

basins_CAZ <-  basins_CAZ[-intersect_mines, ]

length(unique(basins_CAZ$PFAF_ID))

# Donor pool = 8. 


                                # 1b) Format for synthetic control

# Convert shapefile into data.frame

basins_CAZ_df <- as.data.frame(basins_CAZ)

write.csv(basins_CAZ_df, "Outputs_3/basins_CAZ_df_21_04.csv")

basins_CAZ_df$PFAF_ID <- as.character(basins_CAZ_df$PFAF_ID)

# Create dataframe which will represent the control drainage basins

controls <- unique(basins_CAZ_df$Row.names)

# Remove the Bemainty drainage basin. Check this is PFAF_ID ending in 666.

controls <- controls[-8]

View(basins_CAZ_df[basins_CAZ_df$Row.names == 1907, ])

#                           1c) Run Synthetic Control


                # Annual deforestation rate as outcome. Area_ha and Perc_for_2011 as predictors.

names(basins_CAZ_df)[c(6,7,8,10,13,14, 16, 17, 18, 24, 25,130)] 

predictors_perc <- c(6,7,8,10,13,14, 16, 17, 18, 24, 25,130)      


# Function to prepare data using data.prep in Synth package. Provide the input data, the list of predictors, dependent variable and list of controls

data_prep <- function(input, preds, depend, cont){
  output <- dataprep(input, predictors = preds,    # Predictors correct
           dependent = depend, unit.variable = "Row.names",
           time.variable = "Year", treatment.identifier = 1907, 
           controls.identifier = cont, time.predictors.prior = c(1991:2011),
           time.plot = c(1991:2021), time.optimize.ssr = c(1991:2011)
  )
           return(output)
}


# The next function is to prepare data for the in-time placebo tests. Setting the year of the intervention three years earlier to 2009 (see Methods)

data_prep2 <- function(input, preds, depend, cont){
  output <- dataprep(input, predictors = preds,    # Predictors correct
                     dependent = depend, unit.variable = "Row.names",
                     time.variable = "Year", treatment.identifier = 1907, 
                     controls.identifier = cont, time.predictors.prior = c(1991:2009),
                     time.plot = c(1991:2021), time.optimize.ssr = c(1991:2009)
  )
  return(output)
}

# Run data_prep function
  
dataprep.out.rate <- data_prep(basins_CAZ_df, predictors_perc, "Annual_defor_rate", controls)

# Run Synth using dataprep.out as input.

synth.out.rate <- synth(dataprep.out.rate, optimxmethod = "All")

synth.out.rate$loss.v     # The MSPE is 0.0005946928 

# Plot basins which comprise the SC

synth.out.rate$solution.w

For11 <- rast("Outputs_3/TMF_For2011.tif")

plot(For11, col = "dark grey", ext = ext(basins_CAZ))
polys(basins_CAZ, lwd = 1, border = "grey30")
polys(basins_CAZ[basins_CAZ$Row.names == 1904, ], border = "blue", lwd = 1)
polys(basins_CAZ[basins_CAZ$Row.names == 1905, ], border = "blue", lwd = 1)
polys(basins_CAZ[basins_CAZ$Row.names == 1906, ], border = "blue", lwd = 1)
polys(basins_CAZ[basins_CAZ$Row.names == 1907, ], border = "red", lwd = 1)
plot(Mine_sites, add =T, col = "yellow")

# Manually calculate difference in deforestation rate between treated unit and synthetic control, using outcomes and the synthetic control weights.

gaps.rate <- dataprep.out.rate$Y1plot-(
  dataprep.out.rate$Y0plot%*%synth.out.rate$solution.w
)

# Create table of results.

synth.tables <- synth.tab(dataprep.res = dataprep.out.rate, synth.res = synth.out.rate)


# Function to extract predictor values for treated unit and SC, and weights and save.

Tab_results <- function(table, version, scale){
  z <- as.data.frame(cbind(table$tab.pred, table$tab.v))
  z$Predictors <- row.names(z)
  z <- apply(z,2,as.character)
  z <- as.data.frame(z)
  write.csv(z, paste("Outputs_3/SC_summary_", version, scale, "_predictors.csv", sep = ""))
  return(z)
  }

# Run above function.

Summary_table_rate <- Tab_results(synth.tables, "rate", "CAZ")  


# Make prettier path plot and add raw outcomes in rest of donor pool units using for loop. 
# Plot the annual deforestation rate in Bemainty and Synthetic Bemainty

path_rate <- rbind(dataprep.out.rate$Y1plot, (
dataprep.out.rate$Y0plot%*%synth.out.rate$solution.w))

path_rate <- as.data.frame(path_rate)
path_rate$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_rate)[1] <- "Defor_rate"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.rate$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Defor_rate"
  path_rate <- rbind(path_rate, x)
}

path_rate$Year <- rep(1991:2021, 10)

write.csv(path_rate, "Outputs_3/Defor_rate_outcomes_Bemainty_SC_and_controls.csv")


pal <-c(rep("grey70", 8), "black", "red")


ggplot(path_rate, aes(x= Year, y = Defor_rate, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Annual deforestion rate (%)", breaks = seq(0, 2, 0.2))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
#  ggtitle("Comparison of Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")


                # Raw Annual Deforestation as outcome. Area_ha and Perc_for_2011 as predictors.
  

dataprep.out.raw <- data_prep(basins_CAZ_df, predictors_perc, "Annual_defor_ha", controls)

synth.out.raw <- synth(dataprep.out.raw, optimxmethod = 'All')

synth.out.raw$loss.v    
synth.out.raw$solution.v

synth.tables.raw <- synth.tab(dataprep.res= dataprep.out.raw, synth.res = synth.out.raw)
Summary_table_raw <- Tab_results(synth.tables.raw, "raw", "CAZ")  

gaps.raw <- dataprep.out.raw$Y1plot-(
  dataprep.out.raw$Y0plot%*%synth.out.raw$solution.w
)

path_raw <- rbind(dataprep.out.raw$Y1plot, (
  dataprep.out.raw$Y0plot%*%synth.out.raw$solution.w))

path_raw <- as.data.frame(path_raw)
path_raw$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_raw)[1] <- "Defor_raw"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.raw$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Defor_raw"
  path_raw <- rbind(path_raw, x)
}

path_raw$Year <- rep(1991:2021, 10)

write.csv(path_raw, "Outputs_3/Raw_defor_outcomes_Bemainty_SC_and_controls.csv")

ggplot(path_raw, aes(x= Year, y = Defor_raw, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Raw Annual Deforestation (ha)", breaks = seq(0, 450, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Cumulative Annual Deforestation (ha)")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")


View(basins_CAZ_df[, c(2,26,30)])


                  # Raw cumulative deforestation as outcome.


# Calculate cumulative deforestation

basins_CAZ_df <- basins_CAZ_df %>% group_by(PFAF_ID) %>% mutate(Cum_defor_ha = cumsum(Annual_defor_ha))

basins_CAZ_df <- as.data.frame(basins_CAZ_df)

names(basins_CAZ_df[predictors_perc])

dataprep.out.cum <- data_prep(basins_CAZ_df, predictors_perc, "Cum_defor_ha", controls)

synth.out.cum <- synth(dataprep.out.cum, optimxmethod = "All")

synth.out.cum$loss.v     

synth.tables.cum <- synth.tab(dataprep.res= dataprep.out.cum, synth.res = synth.out.cum)
Summary_table_cum <- Tab_results(synth.tables.cum, "cum", "CAZ")  

gaps.cum <- dataprep.out.cum$Y1plot-(
  dataprep.out.cum$Y0plot%*%synth.out.cum$solution.w
)


write.csv(gaps.cum, "gaps_cumulative_defor.csv")

path_cum <- rbind(dataprep.out.cum$Y1plot, (
  dataprep.out.cum$Y0plot%*%synth.out.cum$solution.w))

path_cum <- as.data.frame(path_cum)
path_cum$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_cum)[1] <- "Defor_cum"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.cum$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Defor_cum"
  path_cum<- rbind(path_cum, x)
}

path_cum$Year <- rep(1991:2021, 10)

ggplot(path_cum, aes(x= Year, y = Defor_cum, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Cumulative Annual Deforestation (ha)", breaks = seq(0, 3100, 200))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Cumulative Annual Deforestation (ha)")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



#                                 1e) Placebo tests


            ## Using annual deforestation rate as the outcome variable


# Create the first gaps table using the Bemainty valley as the treatd unit and compare to SC values

# Gaps = Treated - Synthetic Control

gaps.rate <- as.data.frame(gaps.rate)
names(gaps.rate) <- "Gap"
gaps.rate$Unit <- "Treated"
gaps.rate$Total_MSPE <- rep(synth.out.rate$loss.v, 31)  

# Then run a for loop to create a synthetic control for each of the control basins in the donor pool.
# Calculate the gap in values between each control and its synthetic control and add to gaps.rate dataframe. 

for(i in controls){
  data.prep.out <- dataprep(basins_CAZ_df, predictors = predictors_perc,
                            dependent = "Annual_defor_rate", unit.variable = "Row.names",
                            time.variable = "Year", treatment.identifier = i, 
                            controls.identifier = controls[-(which(controls == i))],
                            time.predictors.prior = c(1991:2011),
                            time.plot = c(1991:2021), time.optimize.ssr = c(1991:2011))
  synth.out <- synth(data.prep.out, optimxmethod = "All")
  x <- data.prep.out$Y1plot-(
    data.prep.out$Y0plot%*%synth.out$solution.w)
  x <- as.data.frame(x)
  names(x) <- "Gap"
  x$Unit <- i
  x$Total_MSPE <- rep(synth.out$loss.v, 31)
  gaps.rate <- rbind(gaps.rate, x)  
}

gaps.rate$Year <- rep(1991:2021, 9)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(gaps.rate$Unit[gaps.rate$Total_MSPE > (5*gaps.rate$Total_MSPE[1])]) 

gaps.rate.filt <- gaps.rate[gaps.rate$Total_MSPE < (5*gaps.rate$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(gaps.rate.filt$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- gaps.rate.filt[gaps.rate.filt$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

# Plot


ggplot(gaps.rate.filt, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in annual deforestion rate (%)", breaks = seq(-1, 1, 0.2))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
 # ggtitle("Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")
       

                  ## Raw Annual Deforestation as the outcome variable


gaps.raw <- as.data.frame(gaps.raw)
names(gaps.raw) <- "Gap"
gaps.raw$Unit <- "Treated"
gaps.raw$Total_MSPE <- rep(synth.out.raw$loss.v, 31)

# Make function to do placebo tests. Then run a for loop to create a synthetic control for each of the control basins in the donor pool.
# Calculate the gap in values between each control and its synthetic control and add to gaps.rate dataframe. 

Placebo_tests <- function(input, preds, depend, cont, gaps){
  for(i in controls){
  data.prep.out <- dataprep(input, predictors = preds,
                            dependent = depend, unit.variable = "Row.names",
                            time.variable = "Year", treatment.identifier = i, 
                            controls.identifier = cont[-(which(cont == i))],
                            time.predictors.prior = c(1991:2011),
                            time.plot = c(1991:2021), time.optimize.ssr = c(1991:2011))
  synth.out <- synth(data.prep.out, optimxmethod = "All")
  x <- data.prep.out$Y1plot-(
    data.prep.out$Y0plot%*%synth.out$solution.w)
  x <- as.data.frame(x)
  names(x) <- "Gap"
  x$Unit <- i
  x$Total_MSPE <- rep(synth.out$loss.v, 31)
  gaps <- rbind(gaps, x)  
  }
  return(gaps)
}

# Run function
 
gaps.raw <- Placebo_tests(basins_CAZ_df, predictors_perc, "Annual_defor_ha", controls, gaps.raw)

gaps.raw$Year <- rep(1991:2021, 9)

unique(gaps.raw$Unit[gaps.raw$Total_MSPE > (5*gaps.raw$Total_MSPE[1])])

gaps.raw.filt <- gaps.raw[gaps.raw$Total_MSPE < (5*gaps.raw$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(gaps.raw.filt$Unit)) - 1)   
pal <- c(pal, "black")

# Add column with Year


NoTreated <- gaps.raw.filt[gaps.raw.filt$Unit != "Treated", ]

MinMax.raw <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax.raw) <- c("Year", "Min", "Max")
  
ggplot(gaps.raw.filt, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax.raw, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in raw annual deforestation (ha)", breaks = seq(-100, 350, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.5)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.5)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
#  ggtitle("Raw Annual Deforestation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")


                ### Cumulative deforestation as the outcome variable.


gaps.cum <- as.data.frame(gaps.cum)
names(gaps.cum) <- "Gap"
gaps.cum$Unit <- "Treated"
gaps.cum$Total_MSPE <- rep(synth.out.cum$loss.v, 31)  

gaps.cum <- Placebo_tests(basins_CAZ_df, predictors_perc, "Cum_defor_ha", controls, gaps.cum)

gaps.cum$Year <- rep(1991:2021, 9)

unique(gaps.cum$Unit[gaps.cum$Total_MSPE < (5*gaps.cum$Total_MSPE[1])])

gaps.cum.filt <- gaps.cum[gaps.cum$Total_MSPE < (5*gaps.cum$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(gaps.cum.filt$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- gaps.cum.filt[gaps.cum.filt$Unit != "Treated", ]

MinMax.cum <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax.cum) <- c("Year", "Min", "Max")

ggplot(gaps.cum.filt, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax.cum, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in cumulative deforestation (ha)", breaks = seq(-400, 1100, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.5)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.5)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
#  ggtitle("Cumulative Deforestation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = c("black",NA)),
        legend.position = "none")






#                           2) Using unprotected basins from Toamasina province


#                           2a) Data Preparation

###    Use the basins_Toam layer 

# Subset basins to include only ones where Perc_For_2011 is more than 70%

length(unique(basins_Toam$Row.names[basins_Toam$Perc_For_2011 > 70]))     # This leaves 38 basins

basins_Toam <- basins_Toam[basins_Toam$Perc_For_2011 > 70, ]

# Subset basins to include only those where Perc_overlap_PAs < 10

basins_Toam <- basins_Toam[basins_Toam$Perc_overlap_PAs < 10, ]

# Subset basins to include only those where Perc_overlap_offsets is less than (<) 10.

basins_Toam <- basins_Toam[basins_Toam$Perc_overlap_offsets < 10, ]

# Find out which basins contain mine sites.

intersect_Toam2 <- relate(basins_Toam, Mine_sites, "contains", pairs = TRUE)

# Keep only the column with the row numbers of the basins which contain mine sites

intersect_Toam2 <- intersect_Toam2[, 1]

# Use this to subset the basins data, removing this basin

basins_Toam <-  basins_Toam[-intersect_Toam2, ]

length(unique(basins_Toam$Row.names)) -1

# This gives a donor pool of 13


                    ###    Format for synthetic control


basins_Toam_df2 <- as.data.frame(basins_Toam)

write.csv(basins_Toam_df2, "Outputs_3/basins_Toam_df2_updated_24_04.csv")


basins_Toam_df2$PFAF_ID <- as.character(basins_Toam_df2$PFAF_ID)

controls <- unique(basins_Toam_df2$Row.names)

controls <- controls[-13]   # Remove the Bemainty basin. 


# Calculate cumulative deforestation

basins_Toam_df2 <- basins_Toam_df2 %>% group_by(PFAF_ID) %>% mutate(Cum_defor_ha = cumsum(Annual_defor_ha))

basins_Toam_df2 <- as.data.frame(basins_Toam_df2)




                          ## 2b) Run the synthetic control


                            ### Annual deforestation rate

Toam.dataprep.out.rate <- data_prep(basins_Toam_df2, predictors_perc, "Annual_defor_rate", controls)

Toam.synth.out.rate <- synth(Toam.dataprep.out.rate, optimxmethod = "All")

# Create table of results.

Toam.synth.tables <- synth.tab(dataprep.res = Toam.dataprep.out.rate, synth.res = Toam.synth.out.rate)

# Function to extract predictor values for treated unit and SC, and weights and save.


Tab_results_Toam <- function(table, version){
  z <- as.data.frame(cbind(table$tab.pred, table$tab.v))
  z$Predictors <- row.names(z)
  z <- apply(z,2,as.character)
  z <- z[, c(5,4,1,2)]
  z <- as.data.frame(z)
  x <- table$tab.w
  x <- x[, c(3,1)]
  x$Treated <- NA
  x$Synthetic <- NA
  names(x)[1] <- "Covariate/Unit"
  names(x)[2] <- "Weight"
  names(z)[1] <- "Covariate/Unit"
  names(z)[2] <- "Weight"
  z <- rbind(z,x)
  write.csv(z, paste("Outputs_3/Toam_weights_", version, ".csv", sep = ""))
  return(z)
}

# Run above function.

Toam_summary_table_rate <- Tab_results_Toam(Toam.synth.tables, "_defor_rate")  

Toam.gaps.rate <- Toam.dataprep.out.rate$Y1plot-(
  Toam.dataprep.out.rate$Y0plot%*%Toam.synth.out.rate$solution.w
)


                            ## Raw Annual Deforestation


Toam.dataprep.out.raw <- data_prep(basins_Toam_df2, predictors_perc, "Annual_defor_ha", controls)

Toam.synth.out.raw <- synth(Toam.dataprep.out.raw, optimxmethod = "All")

Toam.synth.out.raw$loss.v # 23.809


# Create table of results.

Toam.synth.tables.raw <- synth.tab(dataprep.res = Toam.dataprep.out.raw, synth.res = Toam.synth.out.raw)

Toam_summary_table_raw <- Tab_results_Toam(Toam.synth.tables.raw, "defor_raw")  

Toam.gaps.raw <- Toam.dataprep.out.raw$Y1plot-(
  Toam.dataprep.out.raw$Y0plot%*%Toam.synth.out.raw$solution.w
)


                          ## Cumulative deforestation


Toam.dataprep.out.cum <- data_prep(basins_Toam_df2, predictors_perc, "Cum_defor_ha", controls)

Toam.synth.out.cum <- synth(Toam.dataprep.out.cum, optimxmethod = "All")

Toam.synth.out.cum$loss.v # 29.15


# Create table of results.

Toam.synth.tables.cum <- synth.tab(dataprep.res = Toam.dataprep.out.cum, synth.res = Toam.synth.out.cum)

Toam_summary_table_cum <- Tab_results_Toam(Toam.synth.tables.cum, "cum")  

Toam.gaps.cum<- Toam.dataprep.out.cum$Y1plot-(
  Toam.dataprep.out.cum$Y0plot%*%Toam.synth.out.cum$solution.w
)

Toam_path_cum <- rbind(Toam.dataprep.out.cum$Y1plot, (
  Toam.dataprep.out.cum$Y0plot%*%Toam.synth.out.cum$solution.w))

Toam_path_cum <- as.data.frame(Toam_path_cum)
Toam_path_cum$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(Toam_path_cum)[1] <- "Defor_cum"

for(i in (1:13)){
  x <- as.data.frame(Toam.dataprep.out.cum$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Defor_cum"
  Toam_path_cum <- rbind(Toam_path_cum, x)
}


Toam_path_cum$Year <- rep(1991:2021, 15)

Toam_path_cum2 <- Toam_path_cum[Toam_path_cum$Unit == "Bemainty"|
                                  Toam_path_cum$Unit == "Synthetic Bemainty"|
                                  Toam_path_cum$Unit == "1906"|
                                  Toam_path_cum$Unit == "1503"|
                                  Toam_path_cum$Unit == "1492", ]


pal <-c("grey70", "orange", "orange", rep("grey70", 8), "orange", "grey70", "black", "red")

pal2 <-c("yellow", "grey", "orange", "black", "red")


ggplot(Toam_path_cum, aes(x= Year, y = Defor_cum, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Cumulative Annual Deforestation (ha)", breaks = seq(0, 3000, 100))+
  geom_vline(xintercept = 2012, linetype="dashed", color = "brown", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dashed", color = "brown", size= 0.75)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Cumulative Annual Deforestation (ha)")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"))






                               ## 2c) Placebo tests


                        # Annual deforestation rate

Toam.gaps.rate <- as.data.frame(Toam.gaps.rate)
names(Toam.gaps.rate) <- "Gap"
Toam.gaps.rate$Unit <- "Treated"
Toam.gaps.rate$Total_MSPE <- rep(Toam.synth.out.rate$loss.v, 31)  

Toam.gaps.rate <- Placebo_tests(basins_Toam_df2, predictors_perc, "Annual_defor_rate", controls, Toam.gaps.rate)


Toam.gaps.rate$Year <- rep(1991:2021, 14)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(Toam.gaps.rate$Unit[Toam.gaps.rate$Total_MSPE > (5*Toam.gaps.rate$Total_MSPE[1])]) 
# 7. 

length(unique(Toam.gaps.rate$Unit))

#This only leaves 6 acceptable placebo comparisons.
Toam.gaps.rate <- Toam.gaps.rate[Toam.gaps.rate$Total_MSPE < (5*Toam.gaps.rate$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(Toam.gaps.rate$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.rate[Toam.gaps.rate$Unit != "Treated", ]

Toam_MinMax.rate <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(Toam_MinMax.rate) <- c("Year", "Min", "Max")

ggplot(Toam.gaps.rate, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = Toam_MinMax.rate, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in annual deforestion rate (%)", breaks = seq(-0.5, 1.5, 0.2))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  # ggtitle("Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")

                      
                          ## Raw annual deforestation


Toam.gaps.raw <- as.data.frame(Toam.gaps.raw)
names(Toam.gaps.raw) <- "Gap"
Toam.gaps.raw$Unit <- "Treated"
Toam.gaps.raw$Total_MSPE <- rep(Toam.synth.out.raw$loss.v, 31)  

Toam.gaps.raw <- Placebo_tests(basins_Toam_df2, predictors_perc, "Annual_defor_ha", controls, Toam.gaps.raw)

Toam.gaps.raw$Year <- rep(1991:2021, 14)

unique(Toam.gaps.raw$Unit[Toam.gaps.raw$Total_MSPE > (5*Toam.gaps.raw$Total_MSPE[1])]) 

Toam.gaps.raw <- Toam.gaps.raw[Toam.gaps.raw$Total_MSPE < (5*Toam.gaps.raw$Total_MSPE[1]), ]

unique(Toam.gaps.raw$Unit)

# This leaves 9 placebo comparisons.

pal <- rep("grey60", length(unique(Toam.gaps.raw$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.raw[Toam.gaps.raw$Unit != "Treated", ]

Toam_MinMax.raw <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(Toam_MinMax.raw) <- c("Year", "Min", "Max")


ggplot(Toam.gaps.raw, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = Toam_MinMax.raw, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in  raw annual deforestion (ha)", breaks = seq(-300, 250, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  # ggtitle("Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



                    ## Cumulative deforestation

Toam.gaps.cum <- as.data.frame(Toam.gaps.cum)
names(Toam.gaps.cum) <- "Gap"
Toam.gaps.cum$Unit <- "Treated"
Toam.gaps.cum$Total_MSPE <- rep(Toam.synth.out.cum$loss.v, 31)  

Toam.gaps.cum <- Placebo_tests(basins_Toam_df2, predictors_perc, "Cum_defor_ha", controls, Toam.gaps.cum)

Toam.gaps.cum$Year <- rep(1991:2021, 14)

unique(Toam.gaps.cum$Unit[Toam.gaps.cum$Total_MSPE > (5*Toam.gaps.cum$Total_MSPE[1])]) 
#6


Toam.gaps.cum <- Toam.gaps.cum[Toam.gaps.cum$Total_MSPE < (5*Toam.gaps.cum$Total_MSPE[1]), ]
# This only leaves 7 placebo comparisons.

pal <- rep("grey60", length(unique(Toam.gaps.cum$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.cum[Toam.gaps.cum$Unit != "Treated", ]

Toam_MinMax.cum <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(Toam_MinMax.cum) <- c("Year", "Min", "Max")


ggplot(Toam.gaps.cum, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = Toam_MinMax.cum, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in cumulative deforestion (ha)", breaks = seq(-400, 800, 100))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  # ggtitle("Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



# ------------------------------------------------------------------------------------#

                            # 3) Degradation analysis


                    # 3a) Using basins from the CAZ as the donor pool

                            # Annual degradation rate

controls <- unique(basins_CAZ_df$Row.names)

controls <- controls[-8]

dataprep.out.rate.deg <- data_prep(basins_CAZ_df, predictors_perc, "Annual_degradation_rate", controls)

synth.out.rate.deg <- synth(dataprep.out.rate.deg, optimxmethod = "All")

synth.out.rate.deg$loss.v    # 0.005438866 


synth.tables.rate.deg <- synth.tab(dataprep.res= dataprep.out.rate.deg, synth.res = synth.out.rate.deg)
Summary_table_rate_deg <- Tab_results(synth.tables.rate.deg, "degradation_rate_", "CAZ")  

gaps.rate.deg <- dataprep.out.rate.deg$Y1plot-(
  dataprep.out.rate.deg$Y0plot%*%synth.out.rate.deg$solution.w
)


# Make prettier path plot and add raw outcomes in rest of donor pool units. 
# Plot the annual deforestation rate in Bemainty and Synthetic Bemainty

path_rate_deg <- rbind(dataprep.out.rate.deg$Y1plot, (
  dataprep.out.rate.deg$Y0plot%*%synth.out.rate.deg$solution.w))

path_rate_deg <- as.data.frame(path_rate_deg)
path_rate_deg$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_rate_deg)[1] <- "Degradation_rate"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.rate.deg$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Degradation_rate"
  path_rate_deg <- rbind(path_rate_deg, x)
}

path_rate_deg$Year <- rep(1991:2021, 10)

write.csv(path_rate_deg, "Outputs_3/Degradation_rate_outcomes_Bemainty_SC_and_controls.csv")


# Or, just the donor units which comprise the SC

path_rate2 <- path_rate[path_rate$Unit == "Bemainty"|
                          path_rate$Unit == "Synthetic Bemainty"|
                          path_rate$Unit == "1904"|
                          path_rate$Unit == "1905"|
                          path_rate$Unit == "1906", ]


pal <-c(rep("grey70", 8), "black", "red")
pal2 <-c("yellow", "grey", "orange", "black", "red")

# Make donor units which comprise SC orange and the others grey.

pal3 <- c(rep("grey60", 4), "orange", "orange", "orange", "grey60", "black", "red")


ggplot(path_rate_deg, aes(x= Year, y = Degradation_rate, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Annual degradation rate (%)", breaks = seq(0, 4.5, 0.5))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



                      # Raw annual degradation


dataprep.out.raw.deg <- data_prep(basins_CAZ_df, predictors_perc, "Annual_degradation_ha", controls)

synth.out.raw.deg <- synth(dataprep.out.raw.deg, optimxmethod = "All")

synth.out.raw.deg$loss.v    # 659


synth.tables.raw.deg <- synth.tab(dataprep.res= dataprep.out.raw.deg, synth.res = synth.out.raw.deg)
Summary_table_raw_deg <- Tab_results(synth.tables.raw.deg, "raw_degradation_", "CAZ")  

gaps.raw.deg <- dataprep.out.raw.deg$Y1plot-(
  dataprep.out.raw.deg$Y0plot%*%synth.out.raw.deg$solution.w
)


# Make prettier path plot and add raw outcomes in rest of donor pool units. 
# Plot the annual deforestation rate in Bemainty and Synthetic Bemainty

path_raw_deg <- rbind(dataprep.out.raw.deg$Y1plot, (
  dataprep.out.raw.deg$Y0plot%*%synth.out.raw.deg$solution.w))

path_raw_deg <- as.data.frame(path_raw_deg)
path_raw_deg$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_raw_deg)[1] <- "Raw_degradation"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.raw.deg$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Raw_degradation"
  path_raw_deg <- rbind(path_raw_deg, x)
}

path_raw_deg$Year <- rep(1991:2021, 10)

write.csv(path_raw_deg, "Outputs_3/Degradation_raw_outcomes_Bemainty_SC_and_controls.csv")


# Or, just the donor units which comprise the SC

path_rate2 <- path_rate[path_rate$Unit == "Bemainty"|
                          path_rate$Unit == "Synthetic Bemainty"|
                          path_rate$Unit == "1904"|
                          path_rate$Unit == "1905"|
                          path_rate$Unit == "1906", ]


pal <-c(rep("grey70", 8), "black", "red")
pal2 <-c("yellow", "grey", "orange", "black", "red")

# Make donor units which comprise SC orange and the others grey.

pal3 <- c(rep("grey60", 4), "orange", "orange", "orange", "grey60", "black", "red")


ggplot(path_raw_deg, aes(x= Year, y = Raw_degradation, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Raw annual degradation (ha)", breaks = seq(0, 1050, 100))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")




                      # Cumulative degradation

basins_CAZ_df <- basins_CAZ_df %>% group_by(PFAF_ID) %>% mutate(Cum_degradation_ha = cumsum(Annual_degradation_ha))

# Code from https://www.r-bloggers.com/2022/06/cumulative-sum-calculation-in-r/

basins_CAZ_df <- as.data.frame(basins_CAZ_df)

dataprep.out.cum.deg <- data_prep(basins_CAZ_df, predictors_perc, "Cum_degradation_ha", controls)

synth.out.cum.deg <- synth(dataprep.out.cum.deg, optimxmethod = "All")

synth.out.cum.deg$loss.v    # 810


synth.tables.cum.deg <- synth.tab(dataprep.res= dataprep.out.cum.deg, synth.res = synth.out.cum.deg)
Summary_table_cum_deg <- Tab_results(synth.tables.cum.deg, "cum_degradation_", "CAZ")  

gaps.cum.deg <- dataprep.out.cum.deg$Y1plot-(
  dataprep.out.cum.deg$Y0plot%*%synth.out.cum.deg$solution.w
)



# Make prettier path plot and add raw outcomes in rest of donor pool units. 
# Plot the annual deforestation rate in Bemainty and Synthetic Bemainty

dataprep.out.cum.deg$Y1plot

path_cum_deg <- rbind(dataprep.out.cum.deg$Y1plot, (
  dataprep.out.cum.deg$Y0plot%*%synth.out.cum.deg$solution.w))

path_cum_deg <- as.data.frame(path_cum_deg)
path_cum_deg$Unit <- c(rep("Bemainty", 31), rep("Synthetic Bemainty", 31))
names(path_cum_deg)[1] <- "Cumulative_degradation"

# Then add outcomes in the other units in the donor pool

for(i in (1:8)){
  x <- as.data.frame(dataprep.out.cum.deg$Y0plot[, i])
  x$Unit <- rep(controls[i], 31)
  names(x)[1] <- "Cumulative_degradation"
  path_cum_deg <- rbind(path_cum_deg, x)
}

path_cum_deg$Year <- rep(1991:2021, 10)

write.csv(path_cum_deg, "Outputs_3/Degradation_cum_outcomes_Bemainty_SC_and_controls.csv")


# Or, just the donor units which comprise the SC

path_rate2 <- path_rate[path_rate$Unit == "Bemainty"|
                          path_rate$Unit == "Synthetic Bemainty"|
                          path_rate$Unit == "1904"|
                          path_rate$Unit == "1905"|
                          path_rate$Unit == "1906", ]


pal <-c(rep("grey70", 8), "black", "red")
pal2 <-c("yellow", "grey", "orange", "black", "red")

# Make donor units which comprise SC orange and the others grey.

pal3 <- c(rep("grey60", 4), "orange", "orange", "orange", "grey60", "black", "red")


ggplot(path_cum_deg, aes(x= Year, y = Cumulative_degradation, group = Unit, color = Unit))+
  geom_line(size = 0.75)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Cumulative annual degradation (ha)", breaks = seq(0, 5500, 500))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 1)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 1)+
  #  geom_hline(yintercept = 0, size = 0.75, col = "black", alpha = 0.5)+
  theme_classic()+
  #  ggtitle("Comparison of Annual Deforestation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")





                              # 3b) Placebo tests


                              # Annual degradation rate

gaps.rate.deg <- as.data.frame(gaps.rate.deg)
names(gaps.rate.deg) <- "Gap"
gaps.rate.deg$Unit <- "Treated"
gaps.rate.deg$Total_MSPE <- rep(synth.out.rate.deg$loss.v, 31)

gaps.rate.deg <- Placebo_tests(basins_CAZ_df, predictors_perc, "Annual_degradation_rate", controls, gaps.rate.deg)

gaps.rate.deg$Year <- rep(1991:2021, 9)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(gaps.rate.deg$Unit[gaps.rate.deg$Total_MSPE > (5*gaps.rate.deg$Total_MSPE[1])]) 

# Remove 3 units, this only leaves 5 placebo comparisons

gaps.rate.deg <- gaps.rate.deg[gaps.rate.deg$Total_MSPE < (5*gaps.rate.deg$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(gaps.rate.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- gaps.rate.deg[gaps.rate.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(gaps.rate.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in annual degradation rate (%)", breaks = seq(-2, 2, 0.5))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
#  ggtitle("Annual Degradation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")


                              # Raw Annual Degradation (ha)


gaps.raw.deg <- as.data.frame(gaps.raw.deg)
names(gaps.raw.deg) <- "Gap"
gaps.raw.deg$Unit <- "Treated"
gaps.raw.deg$Total_MSPE <- rep(synth.out.raw.deg$loss.v, 31)

gaps.raw.deg <- Placebo_tests(basins_CAZ_df, predictors_perc, "Annual_degradation_ha", controls, gaps.raw.deg)

gaps.raw.deg$Year <- rep(1991:2021, 9)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(gaps.raw.deg$Unit[gaps.raw.deg$Total_MSPE > (5*gaps.raw.deg$Total_MSPE[1])]) 

# Remove 1 unit, this leaves 7 placebo comparisons

gaps.raw.deg <- gaps.raw.deg[gaps.raw.deg$Total_MSPE < (5*gaps.raw.deg$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(gaps.raw.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- gaps.raw.deg[gaps.raw.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(gaps.raw.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in raw annual degradation (ha)", breaks = seq(-250, 750, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
 # ggtitle("Raw Annual Degradation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = c(NA,"black")),
        legend.position = "none")


                              # Cumulative Annual Degradation


gaps.cum.deg <- as.data.frame(gaps.cum.deg)
names(gaps.cum.deg) <- "Gap"
gaps.cum.deg$Unit <- "Treated"
gaps.cum.deg$Total_MSPE <- rep(synth.out.cum.deg$loss.v, 31)

gaps.cum.deg <- Placebo_tests(basins_CAZ_df, predictors_perc, "Cum_degradation_ha", controls, gaps.cum.deg)

gaps.cum.deg$Year <- rep(1991:2021, 9)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(gaps.cum.deg$Unit[gaps.cum.deg$Total_MSPE > (5*gaps.cum.deg$Total_MSPE[1])]) 

# Remove 4 unit, this leaves 4 placebo comparisons

gaps.cum.deg <- gaps.cum.deg[gaps.cum.deg$Total_MSPE < (5*gaps.cum.deg$Total_MSPE[1]), ]
pal <- rep("grey70", length(unique(gaps.cum.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- gaps.cum.deg[gaps.cum.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(gaps.cum.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in cumulative degradation (ha)", breaks = seq(-800, 1400, 200))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
#  ggtitle("Cumulative Degradation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



#                       # 3c) Using basins from Toamasina as donor pool


# basins_Toam_df2 comprises units in the donor pool (filtered selection)


                        # Annual degradation rate

controls <- unique(basins_Toam_df2$Row.names)

controls <- controls[-13]

Toam.dataprep.out.rate.deg <- data_prep(basins_Toam_df2, predictors_perc, "Annual_degradation_rate", controls)

Toam.synth.out.rate.deg <- synth(Toam.dataprep.out.rate.deg, optimxmethod = "All")

Toam.synth.out.rate.deg$loss.v    # 0.005430037 


Toam.synth.tables.rate.deg <- synth.tab(dataprep.res= Toam.dataprep.out.rate.deg, synth.res = Toam.synth.out.rate.deg)
Summary_table_rate_deg <- Tab_results(Toam.synth.tables.rate.deg, "degradation_rate_", "Toam")  

path.plot(dataprep.res = Toam.dataprep.out.rate.deg, synth.res = Toam.synth.out.rate.deg, tr.intake = c(2012,2016))

gaps.plot(dataprep.res = Toam.dataprep.out.rate.deg, synth.res = Toam.synth.out.rate.deg, tr.intake = c(2012,2016))

Toam.gaps.rate.deg <- Toam.dataprep.out.rate.deg$Y1plot-(
  Toam.dataprep.out.rate.deg$Y0plot%*%Toam.synth.out.rate.deg$solution.w
)



                           # Raw Annual Degradation


Toam.dataprep.out.raw.deg <- data_prep(basins_Toam_df2, predictors_perc, "Annual_degradation_ha", controls)

Toam.synth.out.raw.deg <- synth(Toam.dataprep.out.raw.deg, optimxmethod = "All")

Toam.synth.out.raw.deg$loss.v    # 693

Toam.synth.tables.raw.deg <- synth.tab(dataprep.res= Toam.dataprep.out.raw.deg, synth.res = Toam.synth.out.raw.deg)
Toam.Summary_table_raw_deg <- Tab_results(Toam.synth.tables.raw.deg, "raw_degradation_", "Toam")  

path.plot(dataprep.res = Toam.dataprep.out.raw.deg, synth.res = Toam.synth.out.raw.deg, tr.intake = c(2012,2016))

gaps.plot(dataprep.res = Toam.dataprep.out.raw.deg, synth.res = Toam.synth.out.raw.deg, tr.intake = c(2012,2016))

Toam.gaps.raw.deg <- Toam.dataprep.out.raw.deg$Y1plot-(
  Toam.dataprep.out.raw.deg$Y0plot%*%Toam.synth.out.raw.deg$solution.w
)




                            # Cumulative degradation


basins_Toam_df2 <- basins_Toam_df2 %>% group_by(PFAF_ID) %>% mutate(Cum_degradation_ha = cumsum(Annual_degradation_ha))

# Code from https://www.r-bloggers.com/2022/06/cumulative-sum-calculation-in-r/

basins_Toam_df2 <- as.data.frame(basins_Toam_df2)

Toam.dataprep.out.cum.deg <- data_prep(basins_Toam_df2, predictors_perc, "Cum_degradation_ha", controls)

Toam.synth.out.cum.deg <- synth(Toam.dataprep.out.cum.deg, optimxmethod = "All")

Toam.synth.out.cum.deg$loss.v    # 909.50


Toam.synth.tables.cum.deg <- synth.tab(dataprep.res= Toam.dataprep.out.cum.deg, synth.res = Toam.synth.out.cum.deg)
Toam_Summary_table_cum_deg <- Tab_results(Toam.synth.tables.cum.deg, "cum_degradation_", "Toam")  

path.plot(dataprep.res = Toam.dataprep.out.cum.deg, synth.res = Toam.synth.out.cum.deg, tr.intake = c(2012,2016))

gaps.plot(dataprep.res = Toam.dataprep.out.cum.deg, synth.res = Toam.synth.out.cum.deg, tr.intake = c(2012,2016))

Toam.gaps.cum.deg <- Toam.dataprep.out.cum.deg$Y1plot-(
  Toam.dataprep.out.cum.deg$Y0plot%*%Toam.synth.out.cum.deg$solution.w
)



                        # 3d) Placebo tests degradation Toamasina


                              # Annual degradation rate


Toam.gaps.rate.deg <- as.data.frame(Toam.gaps.rate.deg)
names(Toam.gaps.rate.deg) <- "Gap"
Toam.gaps.rate.deg$Unit <- "Treated"
Toam.gaps.rate.deg$Total_MSPE <- rep(Toam.synth.out.rate.deg$loss.v, 31)

Toam.gaps.rate.deg <- Placebo_tests(basins_Toam_df2, predictors_perc, "Annual_degradation_rate", controls, Toam.gaps.rate.deg)

######

Toam.gaps.rate.deg$Year <- rep(1991:2021, 14)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(Toam.gaps.rate.deg$Unit[Toam.gaps.rate.deg$Total_MSPE > (5*Toam.gaps.rate.deg$Total_MSPE[1])]) 

# Remove 8 units, this only leaves 5 placebo comparisons

Toam.gaps.rate.deg<- Toam.gaps.rate.deg[Toam.gaps.rate.deg$Total_MSPE < (5*Toam.gaps.rate.deg$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(Toam.gaps.rate.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.rate.deg[Toam.gaps.rate.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(Toam.gaps.rate.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in annual degradation rate (%)", breaks = seq(-2, 2, 0.5))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  #  ggtitle("Annual Degradation Rate")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")



                            # Raw Annual Degradation (ha)



Toam.gaps.raw.deg <- as.data.frame(Toam.gaps.raw.deg)
names(Toam.gaps.raw.deg) <- "Gap"
Toam.gaps.raw.deg$Unit <- "Treated"
Toam.gaps.raw.deg$Total_MSPE <- rep(Toam.synth.out.raw.deg$loss.v, 31)

Toam.gaps.raw.deg <- Placebo_tests(basins_Toam_df2, predictors_perc, "Annual_degradation_ha", controls, Toam.gaps.raw.deg)

Toam.gaps.raw.deg$Year <- rep(1991:2021, 14)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(Toam.gaps.raw.deg$Unit[Toam.gaps.raw.deg$Total_MSPE > (5*Toam.gaps.raw.deg$Total_MSPE[1])]) 

# Remove 1 unit, this leaves 12 placebo comparisons

Toam.gaps.raw.deg <- Toam.gaps.raw.deg[Toam.gaps.raw.deg$Total_MSPE < (5*Toam.gaps.raw.deg$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(Toam.gaps.raw.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.raw.deg[Toam.gaps.raw.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(Toam.gaps.raw.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in raw annual degradation (ha)", breaks = seq(-300, 700, 50))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  # ggtitle("Raw Annual Degradation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = c("black", NA)),
        legend.position = "none")




                            # Cumulative Annual Degradation


Toam.gaps.cum.deg <- as.data.frame(Toam.gaps.cum.deg)
names(Toam.gaps.cum.deg) <- "Gap"
Toam.gaps.cum.deg$Unit <- "Treated"
Toam.gaps.cum.deg$Total_MSPE <- rep(Toam.synth.out.cum.deg$loss.v, 31)

Toam.gaps.cum.deg <- Placebo_tests(basins_Toam_df2, predictors_perc, "Cum_degradation_ha", controls, Toam.gaps.cum.deg)

Toam.gaps.cum.deg$Year <- rep(1991:2021, 14)

# Remove placebo treated units where the MSPE is greater than 5x that of the actual treated unit

unique(Toam.gaps.cum.deg$Unit[Toam.gaps.cum.deg$Total_MSPE > (5*Toam.gaps.cum.deg$Total_MSPE[1])]) 

# Remove 4 units, this leaves 4 placebo comparisons

Toam.gaps.cum.deg <- Toam.gaps.cum.deg[Toam.gaps.cum.deg$Total_MSPE < (5*Toam.gaps.cum.deg$Total_MSPE[1]), ]
pal <- rep("grey60", length(unique(Toam.gaps.cum.deg$Unit)) - 1)   
pal <- c(pal, "black")

NoTreated <- Toam.gaps.cum.deg[Toam.gaps.cum.deg$Unit != "Treated", ]

MinMax <- NoTreated %>% group_by(Year) %>% summarise(min(Gap), max(Gap))
names(MinMax) <- c("Year", "Min", "Max")

ggplot(Toam.gaps.cum.deg, aes(x = Year, y = Gap, color = Unit))+
  geom_ribbon(data = MinMax, aes(x = Year, ymin = Min , ymax = Max), fill = "grey80", inherit.aes = FALSE)+
  geom_line(size = 0.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(breaks = seq(1991, 2021, 1))+
  scale_y_continuous(name = "Difference in cumulative degradation (ha)", breaks = seq(-800, 1400, 200))+
  geom_vline(xintercept = 2012, linetype="dotted", color = "blue", size= 0.75)+
  geom_vline(xintercept = 2016, linetype="dotted", color = "blue", size= 0.75)+
  geom_hline(yintercept = 0, size = 0.75, col = "deeppink", size = 0.5, alpha = 0.6)+
  theme_classic()+
  #  ggtitle("Cumulative Degradation")+
  theme(axis.text.x = element_text(colour = c("black", NA, NA, NA)),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")

