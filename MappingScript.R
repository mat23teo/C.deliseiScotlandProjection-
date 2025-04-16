#Mapping Script for creating a map for the occurrence data in Scotland
#Also creating Violin Plots for the different variables 
#Variables are temperature, relative humidity, precip, snow
#1981-2010 CHELSA data & future data
#Occurrence data from GBIF
#Matteo Missier

#### Libraries ####
library(userfriendlyscience)
library(rstatix)
library(car)
library(PMCMRplus)
library(RColorBrewer)
library(terra)
library(sf)
library(ggplot2)
library(tmap)
library(readxl)
library(tidyr)
library(patchwork)
library(raster)
library(viridis)
library(dplyr)
library(ggthemes)


#### Load Map data #### 
#Cairngorms
cairngorms_boundary <- vect("Maps/National_Parks_-_Scotland.shp") 
writeVector(cairngorms_boundary, "cairngorms_boundary.shp", overwrite=TRUE) #rename it
#Elevation Maps
untar("Maps/hillrasters_EUDTM.tar.gz", exdir = "Maps") #Cg
untar("Maps/Svrasters_GMRT.tar.gz", exdir = "Maps") #Sv
cg_geomap <- rast("Maps/output_be.tif") #cairngorms
sv_geomap <- rast("Maps/output_GMRT.tif") #svalbard
#Svalbard
svalbard_boundary <- vect("Maps/xf635sb7068.shp")
writeVector(svalbard_boundary, "svalbard_boundary.shp", overwrite=TRUE) #rename it
#Scotland
uk_boundaries <- vect("Maps/gadm41_GBR_shp/gadm41_GBR_1.shp") #UK
scotland_boundary <- uk_boundaries[uk_boundaries$NAME_1 == "Scotland", ] #Cut to Scotland
writeVector(scotland_boundary, "scotland_boundary.shp", overwrite=TRUE)

#### Load Occurrence data ####
occ_data <- read_excel("SpeciesData/OccurrenceData.xlsx")  #from gbif
#filter NAs and >1960
occ_data <- occ_data[!is.na(occ_data$decimalLongitude) & !is.na(occ_data$decimalLatitude), ]
occ_data <- occ_data %>% 
  filter(year >= 1960)
occ_data_sv <- read_excel("SpeciesData/SvalbardOccurrence.xlsx")
occ_data_sv <- occ_data_sv[!is.na(occ_data_sv$decimalLongitude) & !is.na(occ_data_sv$decimalLatitude), ]
occ_data_sv <- occ_data_sv %>% 
  filter(year >= 1960)

#### Load Current Climate data ####
#Temperature
temp_raster <- rast("Climate/CHELSA_bio1_1981-2010_V.2.1.tif")   #Mean Temperature data
#Precipitation 
pr_raster <- rast("Climate/CHELSA_bio12_1981-2010_V.2.1.tif") #Mean annual precipitation data
#Snow cover days
scd_raster <- rast("Climate/CHELSA_scd_1981-2010_V.2.1.tif") #Number of snow cover days (count data) TREELIM used
#Growing Season Length
gsl_raster <- rast("Climate/CHELSA_gsl_1981-2010_V.2.1.tif") #Mean gsl data
#Koppen-Geiger 
kg_raster <- rast("Climate/CHELSA_kg2_1981-2010_V.2.1.tif") #Kg data


#### Load Future Climate Data ####
#Temperature
ft_temp_raster <- rast("Climate/CHELSA_bio1_2071-2100_ukesm1-0-ll_ssp370_V.2.1.tif")   #Mean data 2070-2100
ft_temp_raster_ssp5 <- rast("Climate/CHELSA_bio1_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif")   #Mean data 2070-2100
#Precipitation
ft_pr_raster <- rast("Climate/CHELSA_bio12_2071-2100_ukesm1-0-ll_ssp370_V.2.1.tif")   #Mean Temperature data
ft_pr_raster_ssp5 <- rast("Climate/CHELSA_bio12_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif")   #Mean Temperature data
#Snow Cover Days
ft_scd_raster <- rast("Climate/CHELSA_scd_2071-2100_ukesm1-0-ll_ssp370_V.2.1.tif")   #Mean data 2070-2100
ft_scd_raster_ssp5 <- rast("Climate/CHELSA_scd_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif")   #Mean data 2070-2100
#Growing Season Length
ft_gsl_raster <- rast("Climate/CHELSA_gsl_2071-2100_ukesm1-0-ll_ssp370_V.2.1.tif") #Mean gsl data
ft_gsl_raster_ssp5 <- rast("Climate/CHELSA_gsl_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif") #Mean gsl data
#Koppen-Geiger
ft_kg_raster <- rast("Climate/CHELSA_kg2_2071-2100_ukesm1-0-ll_ssp370_V.2.1.tif") #Kg data
ft_kg_raster_ssp5 <- rast("Climate/CHELSA_kg2_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif") #Kg data


#### Occurrence Data Mapped ####
### All of Scotland ###
#convert to sf
occ_sf <- st_as_sf(occ_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # Set CRS to WGS84
occ_sv_sf <- st_as_sf(occ_data_sv, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # Set CRS to WGS84
#cairngorms_boundary <- st_as_sf(cairngorms_boundary)
#svalbard_boundary <- st_as_sf(svalbard_boundary)
#scotland_boundary <- st_as_sf(scotland_boundary)
#plot Scotland map, unused
ggplot() +
  geom_sf(data = scotland_boundary, fill = "lightgoldenrod", color = "#636363", alpha = .75) +
  geom_sf(data = cairngorms_boundary, fill = "lightgoldenrod1", color = "#636363", alpha = 0.75) +
  geom_sf(data = occ_sf, color = "darkblue", size = 0.75, alpha = 1) +
  theme_pander() +
  labs(title = "")
### Cairngorms ###
#plot
ggplot() +
  geom_sf(data = cairngorms_sf, fill = "lightgoldenrod1", color = "#636363", alpha = 1) +
  geom_sf(data = occ_sf, color = "darkblue", size = 1, alpha = 1) +
  theme_pander() +
  labs(title = "")
### Svalbard ###
#plot
ggplot() +
  geom_sf(data = svalbard_sf, fill = "lightgoldenrod1", color = "#636363", alpha = 1) +
  geom_sf(data = occ_sv_sf, color = "darkblue", size = 1, alpha = 1) +
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "")


#### Elevation Maps ####
# Ensure CRS matches
if (crs(cg_geomap) != crs(cairngorms_boundary)) {
  cairngorms_boundary <- project(cairngorms_boundary, crs(cg_geomap))
}
if (crs(sv_geomap) != crs(svalbard_boundary)) {
  svalbard_boundary <- project(svalbard_boundary, crs(sv_geomap))
}

# Crop and mask
cg_geomap_crop <- crop(cg_geomap, ext(cairngorms_boundary))  # Use ext() instead of extent()
cg_geomap_mask <- mask(cg_geomap_crop, cairngorms_boundary)
cg_geomap_df <- as.data.frame(cg_geomap_mask, xy = TRUE) %>% na.omit()
cairngorms_sf <- st_as_sf(cairngorms_boundary)
head(cg_geomap_df)
sv_geomap_crop <- crop(sv_geomap, ext(svalbard_boundary))  # Use ext() instead of extent()
sv_geomap_mask <- mask(sv_geomap_crop, svalbard_boundary)
sv_geomap_df <- as.data.frame(sv_geomap_mask, xy = TRUE) %>% na.omit()
svalbard_sf <- st_as_sf(svalbard_boundary)
scotland_sf <- st_as_sf(scotland_boundary)
head(sv_geomap_df)

# Plot Cg elevation map
ggplot() +
  geom_raster(data = cg_geomap_df, aes(x = x, y = y, fill = output_be)) +
  geom_sf(data = cairngorms_sf, fill = NA, color = "white", size = 1) +
  scale_fill_viridis_c(option = "viridis", name = "Elevation (m)") +  # Use viridis color palette
  geom_sf(data = occ_sf, color = "cyan", size = 2, alpha = 0.75) +
  theme_pander() +
  labs(title = "")

# Plot Sv elevation map
ggplot() +
  geom_raster(data = sv_geomap_df, aes(x = x, y = y, fill = output_GMRT)) +
  scale_fill_viridis_c(option = "viridis", name = "Elevation (m)") +  # Use viridis color palette
  geom_sf(data = svalbard_sf, fill = NA, color = "white", size = 0.1, alpha = 0.1) +
  geom_segment(aes(x = 15.5, y = 78.22, xend = 8.5, yend = 78.22),
               color = "black", linewidth = 0.7, linetype = "solid") +
  geom_label(aes(x = 8.5, y = 78.22), label = "Longyearbyen", 
             color = "black", fill = "white", label.size = 0.5, 
             fontface = "bold", size = 4) +
  geom_sf(data = occ_sv_sf, color = "cyan", size = 1.5, alpha = 0.75) +
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "")



#### Current Temperature Violin Plots ####
## Cairngorms ##
cairngorms_boundary_temp <- project(cairngorms_boundary, crs(temp_raster))
# Crop and mask raster to Cairngorms
cairngorms_temp <- crop(temp_raster, cairngorms_boundary_temp)
cairngorms_temp <- mask(cairngorms_temp, cairngorms_boundary_temp)
# Convert raster to data frame
cairngorms_temp_df <- as.data.frame(cairngorms_temp, xy = TRUE, na.rm = TRUE)
summary(ft_cairngorms_temp_df)
# Rename the temperature column (assuming single-band raster)
colnames(cairngorms_temp_df)[3] <- "Temperature"
p_temp <- ggplot(cairngorms_temp_df, aes(x = "", y = Temperature)) +
  geom_violin(fill = "skyblue", color = "black") +
  theme_pander() +
  labs(title = "Annual Temperature Distribution in the Cairngorms",
       y = "Mean Air Temperature across 1-year(°C)",
       x = "")
p_temp
## Svalbard ## 
svalbard_boundary_temp <- project(svalbard_boundary, crs(temp_raster))
# Crop and mask raster to Svalbard
svalbard_temp <- crop(temp_raster, svalbard_boundary_temp)
svalbard_temp <- mask(svalbard_temp, svalbard_boundary_temp)
# Convert raster to data frame
svalbard_temp_df <- as.data.frame(svalbard_temp, xy = TRUE, na.rm = TRUE)
# Rename the temperature column (assuming single-band raster)
colnames(svalbard_temp_df)[3] <- "Temperature"
p_temp_sv <- ggplot(svalbard_temp_df, aes(x = "", y = Temperature)) +
  geom_violin(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Annual Temperature Distribution in the Svalbard",
       y = "Mean Air Temperature across 1-year(°C)",
       x = "")
p_temp_sv

#### Future Temperature Violin Plot ####
## Cairngorms ##
ft_cairngorms_boundary_temp <- project(cairngorms_boundary, crs(ft_temp_raster))
# Crop and mask raster to Cairngorms
ft_cairngorms_temp <- crop(ft_temp_raster, ft_cairngorms_boundary_temp)
ft_cairngorms_temp <- mask(ft_cairngorms_temp, ft_cairngorms_boundary_temp)
# Convert raster to data frame
ft_cairngorms_temp_df <- as.data.frame(ft_cairngorms_temp, xy = TRUE, na.rm = TRUE)
# Rename the temperature column (assuming single-band raster)
colnames(ft_cairngorms_temp_df)[3] <- "Temperature"
#plot
p_ft_temp <- ggplot(ft_cairngorms_temp_df, aes(x = "", y = Temperature)) +
  geom_violin(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Future Annual Temperature Distribution in the Cairngorms",
       y = "Mean Air Temperature across 1-year(°C)",
       x = "")
p_ft_temp
summary(ft_svalbard_temp)
summary(ft_cairngorms_temp_df)
## Svalbard ##
ft_svalbard_boundary_temp <- project(svalbard_boundary, crs(ft_temp_raster))
# Crop and mask raster to Svalbard
ft_svalbard_temp <- crop(ft_temp_raster, ft_svalbard_boundary_temp)
ft_svalbard_temp <- mask(ft_svalbard_temp, ft_svalbard_boundary_temp)
# Convert raster to data frame
ft_svalbard_temp_df <- as.data.frame(ft_svalbard_temp, xy = TRUE, na.rm = TRUE)
# Rename the temp column (assuming single-band raster)
colnames(ft_svalbard_temp_df)[3] <- "Temperature"
#plot
p_ft_temp_sv <- ggplot(ft_svalbard_temp_df, aes(x = "", y = Temperature)) +
  geom_violin(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Future Annual Temperature Distribution in the Svalbard",
       y = "Mean Air Temperature across 1-year(°C)",
       x = "")
p_ft_temp_sv

#### Combined Temp Plots ####
#Label each different dataframe with its location
ft_cairngorms_temp_df <- ft_cairngorms_temp_df %>% mutate(Location = "FCg")
ft_svalbard_temp_df <- ft_svalbard_temp_df %>% mutate(Location = "FSv")
svalbard_temp_df <- svalbard_temp_df %>% mutate(Location = "Sv")
cairngorms_temp_df <- cairngorms_temp_df %>% mutate(Location = "Cg")
#combine dataframes
df_combined <- bind_rows(cairngorms_temp_df, svalbard_temp_df, ft_cairngorms_temp_df, 
                         ft_svalbard_temp_df)
#rejig the levels of the factors
df_combined$Location <- factor(df_combined$Location, 
                               levels = c("Sv", "FSv", "Cg", "FCg"))  # Custom order
#Finding the means
temp_means <- df_combined %>%
  group_by(Location) %>%
  summarise(mean_temperature = mean(Temperature, na.rm = TRUE))
temp_means

#plot
temp_plot_1 <- ggplot(df_combined, aes(x = Location, y = Temperature, fill = Location)) +
  geom_violin() +
  geom_segment(data = temp_means, aes(x = as.numeric(Location) - 0.5, 
                                      xend = as.numeric(Location) + 0.5, 
                                      y = mean_temperature, yend = mean_temperature), 
               color = "navyblue", linetype = 'dashed', size = 1.1) +
  labs(y = "Temperature (°C)", x = "Scenario", 
       title = "",
       color = "Mean") +
  scale_fill_brewer(palette = "Set3") +
  theme_pander()
temp_plot_1

#### Current Precipitation Violin Plots ####
## Cairngorms ##
cairngorms_boundary_precip <- project(cairngorms_boundary, crs(pr_raster))
# Crop and mask raster to Cairngorms
cairngorms_pr <- crop(pr_raster, cairngorms_boundary_precip)
cairngorms_pr <- mask(cairngorms_pr, cairngorms_boundary_precip)
# Convert raster to data frame
cairngorms_pr_df <- as.data.frame(cairngorms_pr, xy = TRUE, na.rm = TRUE)
# Rename the precipitation column (assuming single-band raster)
colnames(cairngorms_pr_df)[3] <- "Precipitation"
p_pr <- ggplot(cairngorms_pr_df, aes(x = "", y = Precipitation)) +
  geom_violin(fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Precipitation Distribution in the Cairngorms",
       y = "Avg annual precipitation over 1-year (kg m-2
year-1)",
       x = "")
p_pr

## Svalbard ##
svalbard_boundary_pr <- project(svalbard_boundary, crs(pr_raster))
# Crop and mask raster to Cairngorms
svalbard_pr <- crop(pr_raster, svalbard_boundary_pr)
svalbard_pr <- mask(svalbard_pr, svalbard_boundary_pr)
# Convert raster to data frame
svalbard_pr_df <- as.data.frame(svalbard_pr, xy = TRUE, na.rm = TRUE)
# Rename the precipitation column (assuming single-band raster)
colnames(svalbard_pr_df)[3] <- "Precipitation"
#plot
p_pr_sv <- ggplot(svalbard_pr_df, aes(x = "", y = Precipitation)) +
  geom_violin(fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Precipitation Distribution in the Svalbard",
       y = "Avg annual precipitation over 1-year (kg m-2
year-1)",
       x = "")
p_pr_sv

#### Future Precipitation Violin Plot ####
## Cairngorms ##
ft_cairngorms_boundary_pr <- project(cairngorms_boundary, crs(ft_pr_raster))
# Crop and mask raster to Cairngorms
ft_cairngorms_pr <- crop(ft_pr_raster, ft_cairngorms_boundary_pr)
ft_cairngorms_pr <- mask(ft_cairngorms_pr, ft_cairngorms_boundary_pr)
# Convert raster to data frame
ft_cairngorms_pr_df <- as.data.frame(ft_cairngorms_pr, xy = TRUE, na.rm = TRUE)
# Rename the precipitation column (assuming single-band raster)
colnames(ft_cairngorms_pr_df)[3] <- "Precipitation"
#plot
p_ft_pr <- ggplot(ft_cairngorms_pr_df, aes(x = "", y = Precipitation)) +
  geom_violin(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Future Annual Precipitation Distribution in the Cairngorms",
       y = "Avg annual precipitation over 1-year (kg m-2
year-1)",
       x = "")
p_ft_pr

## Svalbard ##
ft_svalbard_boundary_pr <- project(svalbard_boundary, crs(ft_pr_raster))
# Crop and mask raster to Cairngorms
ft_svalbard_pr <- crop(ft_pr_raster, ft_svalbard_boundary_pr)
ft_svalbard_pr <- mask(ft_svalbard_pr, ft_svalbard_boundary_pr)
# Convert raster to data frame
ft_svalbard_pr_df <- as.data.frame(ft_svalbard_pr, xy = TRUE, na.rm = TRUE)
# Rename the precipitation column (assuming single-band raster)
colnames(ft_svalbard_pr_df)[3] <- "Precipitation"
#plot
p_ft_pr_sv <- ggplot(ft_svalbard_pr_df, aes(x = "", y = Precipitation)) +
  geom_violin(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Future Annual Precipitation Distribution on Svalbard",
       y = "Avg annual precipitation over 1-year (kg m-2
year-1)",
       x = "")
p_ft_pr_sv

#### Combined Precipitation Plots ####
#Grouped df
ft_cairngorms_pr_df <- ft_cairngorms_pr_df %>% mutate(Location = "FCg")
ft_svalbard_pr_df <- ft_svalbard_pr_df %>% mutate(Location = "FSv")
svalbard_pr_df <- svalbard_pr_df %>% mutate(Location = "Sv")
cairngorms_pr_df <- cairngorms_pr_df %>% mutate(Location = "Cg")
pr_df_combined <- bind_rows(cairngorms_pr_df, svalbard_pr_df, ft_cairngorms_pr_df, 
                            ft_svalbard_pr_df)
#Level the locations appropriately
pr_df_combined$Location <- factor(pr_df_combined$Location, 
                                  levels = c("Sv", "FSv", "Cg", "FCg"))  # Custom order
#Finding the means
pr_means <- pr_df_combined %>%
  group_by(Location) %>%
  summarise(mean_pr = mean(Precipitation, na.rm = TRUE))
head(pr_means)
pr_means
#plot
ggplot(pr_df_combined, aes(x = Location, y = Precipitation, fill = Location)) +
  geom_violin() +
  geom_segment(data = pr_means, aes(x = as.numeric(Location) - 0.5, 
                                      xend = as.numeric(Location) + 0.5, 
                                      y = mean_pr, yend = mean_pr), 
               color = "darkblue", linetype = 'dashed', size = 1.1) +
  labs(y = "Precipitation (kg m-2 year-1)", x = "Location", 
       title = "") +
  scale_fill_brewer(palette = "Set3") +
  theme_pander()

#### Current SCD ####
common_scale <- scale_fill_viridis(option = "plasma", name = "SCD (days)", limits = c(0, 365))

## For Cairngorms specifically ##
cairngorms_boundary_scd <- project(cairngorms_boundary, crs(scd_raster))
cairngorms_boundary_scd <- st_as_sf(cairngorms_boundary_scd)
plot(scd_raster)

# Crop and mask raster to Cairngorms
cairngorms_scd <- crop(scd_raster, cairngorms_boundary_scd)
cairngorms_scd <- mask(cairngorms_scd, cairngorms_boundary_scd)
# Convert raster to data frame
cairngorms_scd_df <- as.data.frame(cairngorms_scd, xy = TRUE, na.rm = TRUE)
colnames(cairngorms_scd_df)[3] <- "SnowCoverDays"
cairngorms_scd_mean <- mean(cairngorms_scd_df["SnowCoverDays"]) #348,778 snow cover days from 1981-2010
#Plot
p_scd <- ggplot() +
    geom_raster(data = cairngorms_scd_df, aes(x = x, y = y, fill = SnowCoverDays)) +
    geom_sf(data = cairngorms_boundary_scd, fill = NA, color = "black", size = 1) +
  common_scale +
    coord_sf() +
    theme_pander() +
    labs(title = "")
p_scd

## Svalbard ##
svalbard_boundary_scd <- project(svalbard_boundary, crs(scd_raster))
svalbard_boundary_scd <- st_as_sf(svalbard_boundary_scd)
# Crop and mask raster to Svalbard
svalbard_scd <- crop(scd_raster, svalbard_boundary_scd)
svalbard_scd <- mask(svalbard_scd, svalbard_boundary_scd)
# Convert raster to data frame
svalbard_scd_df <- as.data.frame(svalbard_scd, xy = TRUE, na.rm = TRUE)
colnames(svalbard_scd_df)[3] <- "SnowCoverDays"
svalbard_scd_count <- sum(svalbard_scd_df["SnowCoverDays"]) #137,840,518 snow cover days from 1981-2010
#Plot
p_scd_sv <- ggplot() +
  geom_tile(data = svalbard_scd_df, aes(x = x, y = y, fill = SnowCoverDays)) +
  geom_sf(data = svalbard_boundary_scd, fill = NA, color = "black", size = 0.0001,
          alpha = 0.005) +
  common_scale +
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "")
p_scd_sv
summary(svalbard_scd_df)
#### Future SCD ####
## Cairngorms ##
ft_cairngorms_boundary_scd <- project(cairngorms_boundary, crs(ft_scd_raster))
ft_cairngorms_boundary_scd <- st_as_sf(ft_cairngorms_boundary_scd)
# Crop and mask raster to Cairngorms
ft_cairngorms_scd <- crop(ft_scd_raster, ft_cairngorms_boundary_scd)
ft_cairngorms_scd <- mask(ft_cairngorms_scd, ft_cairngorms_boundary_scd)
# Convert raster to data frame
ft_cairngorms_scd_df <- as.data.frame(ft_cairngorms_scd, xy = TRUE, na.rm = TRUE)
# Rename the snow cover column (assuming single-band raster)
colnames(ft_cairngorms_scd_df)[3] <- "SnowCoverDays"
ft_cairngorms_scd_count <- sum(ft_cairngorms_scd_df["SnowCoverDays"]) #337 snow cover days from 1981-2010
ft_cairngorms_scd_count
#Plot
p_ft_scd <- ggplot() +
  geom_raster(data = ft_cairngorms_scd_df, aes(x = x, y = y, fill = SnowCoverDays)) +
  geom_sf(data = ft_cairngorms_boundary_scd, fill = NA, color = "black", size = 1) +
  common_scale +
  coord_sf() +
  theme_pander() +
  labs(title = "")
p_ft_scd

## Svalbard ##
ft_svalbard_boundary_scd <- project(svalbard_boundary, crs(ft_scd_raster))
ft_svalbard_boundary_scd <- st_as_sf(ft_svalbard_boundary_scd)
# Crop and mask raster to Svalbard
ft_svalbard_scd <- crop(ft_scd_raster, ft_svalbard_boundary_scd)
ft_svalbard_scd <- mask(ft_svalbard_scd, ft_svalbard_boundary_scd)
# Convert raster to data frame
ft_svalbard_scd_df <- as.data.frame(ft_svalbard_scd, xy = TRUE, na.rm = TRUE)
# Rename the snow cover column (assuming single-band raster)
colnames(ft_svalbard_scd_df)[3] <- "SnowCoverDays"
ft_svalbard_scd_mean <- mean(ft_svalbard_scd_df["SnowCoverDays"]) #14,747,710 snow cover days from 1981-2010

#Plot
p_ft_scd_sv <- ggplot() +
    geom_raster(data = ft_svalbard_scd_df, aes(x = x, y = y, fill = SnowCoverDays)) +
    geom_sf(data = ft_svalbard_boundary_scd, fill = NA, color = "black", size = 0.0001,
            alpha = 0.005) +
    common_scale +
    coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
    theme_pander() +
    labs(title = "")
p_ft_scd_sv


#### Combined SCD ####
#Grouped df
ft_cairngorms_scd_df <- ft_cairngorms_scd_df %>% mutate(Location = "FCg")
ft_svalbard_scd_df <- ft_svalbard_scd_df %>% mutate(Location = "FSv")
svalbard_scd_df <- svalbard_scd_df %>% mutate(Location = "Sv")
cairngorms_scd_df <- cairngorms_scd_df %>% mutate(Location = "Cg")
scd_df_combined <- bind_rows(cairngorms_scd_df, svalbard_scd_df, ft_cairngorms_scd_df, 
                            ft_svalbard_scd_df)
#Level the locations appropriately
scd_df_combined$Location <- factor(scd_df_combined$Location, 
                                  levels = c("Sv", "FSv", "Cg", "FCg"))  # Custom order
#Finding the means
scd_means <- scd_df_combined %>%
  group_by(Location) %>%
  summarise(mean_scd = mean(SnowCoverDays, na.rm = TRUE))
scd_means

## Plot ##
combined_scd_all <- p_scd_sv + p_ft_scd_sv + 
  p_scd + p_ft_scd + plot_layout(ncol = 2)

combined_scd_all


#### Current GSL ####
common_scale_gsl <- scale_fill_viridis(option = "plasma", name = "GSL (days)", limits = c(0, 365))
## Cairngorms ##
cairngorms_boundary_gsl <- project(cairngorms_boundary, crs(gsl_raster))
cairngorms_boundary_gsl <- st_as_sf(cairngorms_boundary_gsl)
# Crop and mask raster to Cairngorms
gsl_cropped <- crop(gsl_raster, cairngorms_boundary_gsl)
gsl_masked <- mask(gsl_cropped, cairngorms_boundary_gsl)
# Convert raster to data frame
gsl_df <- as.data.frame(gsl_masked, xy = TRUE, na.rm = TRUE)
# Rename the Growing season column (assuming single-band raster)
colnames(gsl_df)[3] <- "GrowingSeasonLength"

#Plot map
p_gsl <- ggplot() +
  geom_raster(data = gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
  geom_sf(data = cairngorms_boundary_gsl, fill = NA, color = "black", size = 1) +
  coord_sf() +
  common_scale_gsl +
  theme_pander() +
  labs(title = "") +
  theme(axis.text = element_text(size = 8))      
p_gsl

## Svalbard ##
svalbard_boundary_gsl <- project(svalbard_boundary, crs(gsl_raster))
svalbard_boundary_gsl <- st_as_sf(svalbard_boundary_gsl)
# Crop and mask raster to Svalbard
sv_gsl_cropped <- crop(gsl_raster, svalbard_boundary_gsl)
sv_gsl_masked <- mask(sv_gsl_cropped, svalbard_boundary_gsl)
# Convert raster to data frame
sv_gsl_df <- as.data.frame(sv_gsl_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(sv_gsl_df)[3] <- "GrowingSeasonLength"

#Plot map
p_gsl_sv <- ggplot() +
            geom_raster(data = sv_gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
            geom_sf(data = svalbard_boundary_gsl, fill = NA, color = "black", size = 0.0001,
                    alpha = 0.0001) +
  common_scale_gsl +
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
            theme_pander() +
            labs(title = "") +
  theme(axis.text = element_text(size = 8))  
p_gsl_sv
  #zoomed
p_gsl_sv_zoomed <- ggplot() +
  geom_raster(data = sv_gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
  geom_sf(data = svalbard_boundary_gsl, fill = NA, color = "black", size = 0.0001,
          alpha = 0.005) +
  common_scale_gsl +
  coord_sf(
    xlim = c(12, 18),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(77, 79)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "") +
  theme(axis.text = element_text(size = 8))  
p_gsl_sv_zoomed
  #show maps
p_gsl_sv
p_gsl_sv_zoomed

#### Future GSL ####
## Cairngorms ##
ft_cairngorms_boundary_gsl <- project(cairngorms_boundary, crs(ft_gsl_raster))
ft_cairngorms_boundary_gsl <- st_as_sf(ft_cairngorms_boundary_gsl)
# Crop and mask raster to Cairngorms
ft_gsl_cropped <- crop(ft_gsl_raster, ft_cairngorms_boundary_gsl)
ft_gsl_masked <- mask(ft_gsl_cropped, ft_cairngorms_boundary_gsl)
# Convert raster to data frame
ft_gsl_df <- as.data.frame(ft_gsl_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(ft_gsl_df)[3] <- "GrowingSeasonLength"

#Plot map
p_ft_gsl <- ggplot() +
    geom_raster(data = ft_gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
    geom_sf(data = ft_cairngorms_boundary_gsl, fill = NA, color = "black", size = 1) +
    coord_sf() +
  common_scale_gsl +
    theme_pander() +
    labs(title = "") +
  theme(axis.text = element_text(size = 8))  
p_ft_gsl

## Svalbard ##
ft_svalbard_boundary_gsl <- project(svalbard_boundary, crs(ft_gsl_raster))
ft_svalbard_boundary_gsl <- st_as_sf(ft_svalbard_boundary_gsl)
# Crop and mask raster to Svalbard
ft_sv_gsl_cropped <- crop(ft_gsl_raster, ft_svalbard_boundary_gsl)
ft_sv_gsl_masked <- mask(ft_sv_gsl_cropped, ft_svalbard_boundary_gsl)
# Convert raster to data frame
ft_sv_gsl_df <- as.data.frame(ft_sv_gsl_masked, xy = TRUE, na.rm = TRUE)
# Rename the growing season length column (assuming single-band raster)
colnames(ft_sv_gsl_df)[3] <- "GrowingSeasonLength"

#Plot map
p_ft_gsl_sv <- ggplot() +
    geom_raster(data = ft_sv_gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
    geom_sf(data = ft_svalbard_boundary_gsl, fill = NA, color = "black", size = 0.0001,
            alpha = 0.0001) +
  common_scale_gsl +
  coord_sf(
      xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
      ylim = c(76, 81)  # Keep the full latitude range
    ) +
    theme_pander() +
    labs(title = "")+
  theme(axis.text = element_text(size = 8))  
p_ft_gsl_sv

#zoomed
p_ft_gsl_sv_zoomed <- ggplot() +
    geom_raster(data = ft_sv_gsl_df, aes(x = x, y = y, fill = GrowingSeasonLength)) +
    geom_sf(data = ft_svalbard_boundary_gsl, fill = NA, color = "black", size = 0.0001,
            alpha = 0.005) +
    common_scale_gsl +
    coord_sf(
      xlim = c(12, 18),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
      ylim = c(77, 79)  # Keep the full latitude range
    ) +
    theme_pander() +
    labs(title = "Svalbard (zoomed)")+
  theme(axis.text = element_text(size = 8))  
p_ft_gsl_sv_zoomed
#show maps
p_gsl_sv
p_gsl_sv_zoomed
p_gsl
p_ft_gsl
p_ft_gsl_sv

#### Combined GSL ####
## Count, unused ##
cairngorms_gsl_count <- sum(gsl_df["GrowingSeasonLength"]) 
svalbard_gsl_count <- sum(sv_gsl_df["GrowingSeasonLength"]) 
ft_cairngorms_gsl_count <- sum(ft_gsl_df["GrowingSeasonLength"]) 
ft_svalbard_gsl_count <- sum(ft_sv_gsl_df["GrowingSeasonLength"]) 

summed_gsl_df <- data.frame(
  Location = c("Svalbard", "Cairngorms", "Future Svalbard", "Future Cairngorms"),
  GrowingSeasonLength = c(svalbard_gsl_count, cairngorms_gsl_count, ft_svalbard_gsl_count, 
                    ft_cairngorms_gsl_count))
# Create a matrix with four values
gsl_table <- matrix(c(svalbard_gsl_count, cairngorms_gsl_count, ft_svalbard_gsl_count,
                            ft_cairngorms_gsl_count), 
                          nrow = 2, 
                          ncol = 2, 
                          byrow = TRUE,
                          dimnames = list(c("Observed", "Future"), c("Svalbard", "Cairngorms")))
# Print the matrix
print(gsl_table)
# Find the means
#Grouped df
ft_gsl_df <- ft_gsl_df %>% mutate(Location = "Future Cairngorms")
ft_sv_gsl_df <- ft_sv_gsl_df %>% mutate(Location = "Future Svalbard")
sv_gsl_df <- sv_gsl_df %>% mutate(Location = "Svalbard")
gsl_df <- gsl_df %>% mutate(Location = "Cairngorms")
gsl_df_combined <- bind_rows(gsl_df, sv_gsl_df, ft_gsl_df, 
                             ft_sv_gsl_df)
#Level the locations appropriately
gsl_df_combined$Location <- factor(gsl_df_combined$Location, 
                                   levels = c("Svalbard", "Future Svalbard", "Cairngorms", "Future Cairngorms"))  # Custom order
#Finding the means
gsl_means <- gsl_df_combined %>%
  group_by(Location) %>%
  summarise(mean_gsl = mean(GrowingSeasonLength, na.rm = TRUE))
gsl_means


## Plot ##
combined_gsl_all <- p_gsl_sv + p_ft_gsl_sv + 
  p_gsl + p_ft_gsl +
  plot_layout(ncol = 2)
combined_gsl_all
p_gsl_sv_zoomed 

#### Current KG ####  
common_scale_kg <- scale_fill_viridis_d(option = "plasma", 
                                      name = "Koppen-Geiger Climate Classification")
#Not using this one
common_scale_kg <- scale_fill_brewer(palette = "Set3")

## For Cairngorms specifically ##
cairngorms_boundary_kg <- project(cairngorms_boundary, crs(kg_raster))
cairngorms_boundary_kg <- st_as_sf(cairngorms_boundary_kg)
# Crop and mask raster to Cairngorms
kg_cropped <- crop(kg_raster, cairngorms_boundary_kg)
kg_masked <- mask(kg_cropped, cairngorms_boundary_kg)
# Convert raster to data frame
kg_df <- as.data.frame(kg_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(kg_df)[3] <- "KoppenGeiger"

# Define the Köppen-Geiger classification labels
koppen_labels <- c(
  "10" = "Csc - Temperate, Dry and Cold Summer",
  "11" = "Cwa - Temperate, Dry Winter and Hot Summer",
  "20" = "Dsd - Continental, Dry Summer and Very Cold Winter",
  "30" = "EF - Polar Ice Cap"
) #found from "Peel et al.: Updated world Koppen-Geiger climate classification map"

#Plot map
p_kg <- ggplot() +
  geom_raster(data = kg_df, aes(x = x, y = y, fill = factor(KoppenGeiger))) +
  geom_sf(data = cairngorms_boundary_kg, fill = NA, color = "black", size = 1) +
  geom_sf(data = occ_sf, aes(color = "Occurrences"), size = 1.5, alpha = 0.5) +
  scale_fill_viridis_d(option = "plasma", 
                       name = "Koppen-Geiger Climate Classification",
                       labels = koppen_labels) +
  scale_color_manual(name = "Other", values = c("Occurrences" = "cyan")) + 
  coord_sf() +
  theme_pander() +
  labs(title = "", fill = "Occurrences")
p_kg

## Svalbard ##
svalbard_boundary_kg <- project(svalbard_boundary, crs(kg_raster))
svalbard_boundary_kg <- st_as_sf(svalbard_boundary_kg)
# Crop and mask raster to Cairngorms
sv_kg_cropped <- crop(kg_raster, svalbard_boundary_kg)
sv_kg_masked <- mask(sv_kg_cropped, svalbard_boundary_kg)
# Convert raster to data frame
sv_kg_df <- as.data.frame(sv_kg_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(sv_kg_df)[3] <- "KoppenGeiger"

# Define the Köppen-Geiger classification labels
sv_koppen_labels <- c("30" = "EF - Polar Ice Cap",
                      "31" = "ET - Polar Tundra High Elevation"
) #found from "Peel et al.: Updated world Koppen-Geiger climate classification map"

#Plot map
p_sv_kg <- ggplot() +
  geom_raster(data = sv_kg_df, aes(x = x, y = y, fill = factor(KoppenGeiger))) +
  geom_sf(data = occ_sv_sf, aes(color = "Occurrences"), size = 1, alpha = 1) +
  geom_sf(data = svalbard_boundary_kg, fill = NA, color = "black", size = 1) +
  scale_fill_viridis_d(option = "plasma", 
                       name = "Koppen-Geiger Climate Classification",
                       labels = sv_koppen_labels) +
  scale_color_manual(name = "Other", values = c("Occurrences" = "cyan")) + 
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "", color = "Occurrences")
p_sv_kg

#### Future KG ####
## Cairngorms ##
ft_cairngorms_boundary <- project(cairngorms_boundary, crs(ft_kg_raster))
ft_cairngorms_boundary <- st_as_sf(ft_cairngorms_boundary)
# Crop and mask raster to Cairngorms
ft_kg_cropped <- crop(ft_kg_raster, ft_cairngorms_boundary)
ft_kg_masked <- mask(ft_kg_cropped, ft_cairngorms_boundary)
# Convert raster to data frame
ft_kg_df <- as.data.frame(ft_kg_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(ft_kg_df)[3] <- "KoppenGeiger"
# Define the Köppen-Geiger classification labels
ft_cg_koppen_labels <- c(
  "10" = "Csc - Temperate, Dry and Cold Summer",
  "19" = "Dsc - Continental, Dry and Cold Summer"
) #found from "Peel et al.: Updated world Koppen-Geiger climate classification map"

#Plot map
p_ft_kg <- ggplot() +
  geom_raster(data = ft_kg_df, aes(x = x, y = y, fill = factor(KoppenGeiger))) +
  geom_sf(data = occ_sf, aes(color = "Occurrences"), size = 1, alpha = 1) +
  geom_sf(data = ft_cairngorms_boundary, fill = NA, color = "black", size = 1) +
  scale_fill_viridis_d(option = "plasma", 
                       name = "Koppen-Geiger Climate Classification",
                       labels = ft_cg_koppen_labels) +
  scale_color_manual(name = "Other", values = c("Occurrences" = "cyan")) + 
  coord_sf() +
  theme_pander() +
  labs(title = "", color = "Occurrences")
p_ft_kg

## Svalbard ##
ft_svalbard_boundary <- project(svalbard_boundary, crs(ft_kg_raster))
ft_svalbard_boundary <- st_as_sf(ft_svalbard_boundary)
# Crop and mask raster to Svalbard
ft_kg_sv_cropped <- crop(ft_kg_raster, ft_svalbard_boundary)
ft_kg_sv_masked <- mask(ft_kg_sv_cropped, ft_svalbard_boundary)
# Convert raster to data frame
ft_kg_sv_df <- as.data.frame(ft_kg_sv_masked, xy = TRUE, na.rm = TRUE)
# Rename the koppen-geiger column (assuming single-band raster)
colnames(ft_kg_sv_df)[3] <- "KoppenGeiger"
# Define the Köppen-Geiger classification labels
ft_sv_koppen_labels <- c(
  "7" = "BSk - Dry, Semi-Arid and Cold Steppe",
  "10" = "Csc - Temperate, Dry and Cold Summer",
  "11" = "Cwa - Temperate, Dry Winter and Hot Summer",
  "13" = "Cwc - Temperate, Dry Winter and Cold Summer",
  "14" = "Cfa - Temperate, No Dry Season and Hot Summer",
  "19" = "Dsc - Continental, Dry Summer and Cold Summer",
  "20" = "Dsd - Continental, Dry Summer and Very Cold Winter",
  "24" = "Dwd - Continental, Dry and Very Cold Winter",
  "30" = "ET - Polar Tundra")
#found from "Peel et al.: Updated world Koppen-Geiger climate classification map"

#Plot map  #old occurrence data still there
p_ft_sv_kg <- ggplot() +
  geom_raster(data = ft_kg_sv_df, aes(x = x, y = y, fill = factor(KoppenGeiger))) +
  geom_sf(data = occ_sv_sf, aes(color = "Occurrences"), size = 1, alpha = 1) +
  geom_sf(data = ft_svalbard_boundary, fill = NA, color = "black", size = 1) +
  scale_fill_viridis_d(option = "plasma", 
                       name = "Koppen-Geiger Climate Classification",
                       labels = ft_sv_koppen_labels) +
  scale_color_manual(name = "Other", values = c("Occurrences" = "cyan")) + 
  coord_sf(
    xlim = c(5, 35),  # Crop longitude to exclude Jan Mayen (10°W is excluded)
    ylim = c(76, 81)  # Keep the full latitude range
  ) +
  theme_pander() +
  labs(title = "", color = "Occurrences")
p_ft_sv_kg
 

#### Combined KG Plot ####
svalbard_half <- p_sv_kg + p_ft_sv_kg + 
  plot_layout(ncol = 1) 
cairngorm_half <- p_kg + p_ft_kg + plot_layout(ncol = 1)
cairngorm_half
svalbard_half

#### Analysis Template ####
#template for testing ANOVA assumptions
ggplot(scd_df_combined, aes(x = SnowCoverDays, fill = Location)) +  #replace x with the different variables
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap(~Location, scales = "free") +
  theme_minimal()
#normal distribution?

ggplot(scd_df_combined, aes(sample = SnowCoverDays)) +
  stat_qq() + stat_qq_line() + 
  facet_wrap(~Location) +
  theme_minimal()
#residuals

#testing distribution
ks.test(scd_df_combined$SnowCoverDays[scd_df_combined$Location == "Cg"], "pnorm", 
        mean = mean(scd_df_combined$SnowCoverDays[scd_df_combined$Location == "Cg"]), 
        sd = sd(scd_df_combined$SnowCoverDays[scd_df_combined$Location == "Cg"]))
#testing equal variances
leveneTest(SnowCoverDays ~ Location, data = scd_df_combined)

gsl_df_combined %>%
  group_by(Location) %>%
  summarise(sample_size = n())


#### Temp Analysis ####
## ANOVA Temp##
#assumptions for temperature
 #normally distributed, uneven variances, outliers at the extremes
#need to use a Welch's ANOVA test
#uneven variances between groups
temp_anova <- oneway.test(Temperature ~ Location, data = df_combined, var.equal = FALSE)
temp_anova
summary(temp_anova)
summary(df_combined)
#games howell
games_howellT <- games_howell_test(Temperature ~ Location, data = df_combined)
games_howellT
plot(games_howellT)
summary(cairngorms_temp_df)

#### Precip Analysis ####
#Change the labels
ft_cairngorms_pr_df <- ft_cairngorms_pr_df %>% mutate(Location = "FCg")
ft_svalbard_pr_df <- ft_svalbard_pr_df %>% mutate(Location = "FSv")
svalbard_pr_df <- svalbard_pr_df %>% mutate(Location = "Sv")
cairngorms_pr_df <- cairngorms_pr_df %>% mutate(Location = "Cg")
pr_df_combined <- bind_rows(cairngorms_pr_df, svalbard_pr_df, ft_cairngorms_pr_df, 
                            ft_svalbard_pr_df)
#Level the locations appropriately
pr_df_combined$Location <- factor(pr_df_combined$Location, 
                                  levels = c("Sv", "Cg", "FSv", "FCg"))  # Custom order

#assumptions for precipitation 
 #normally distributed, uneven variances, outliers at the extremes
#need to use a Welch's ANOVA test
#uneven variances between groups
pr_anova <- oneway.test(Precipitation ~ Location, data = pr_df_combined, var.equal = FALSE)
pr_anova
games_howell <- games_howell_test(Precipitation ~ Location, data = pr_df_combined)
games_howell

#### SCD Analysis ####
#Change the labels
ft_cairngorms_scd_df <- ft_cairngorms_scd_df %>% mutate(Location = "FCg")
ft_svalbard_scd_df <- ft_svalbard_scd_df %>% mutate(Location = "FSv")
svalbard_scd_df <- svalbard_scd_df %>% mutate(Location = "Sv")
cairngorms_scd_df <- cairngorms_scd_df %>% mutate(Location = "Cg")
scd_df_combined <- bind_rows(cairngorms_scd_df, svalbard_scd_df, ft_cairngorms_scd_df, 
                            ft_svalbard_scd_df)
#Level the locations appropriately
scd_df_combined$Location <- factor(scd_df_combined$Location, 
                                  levels = c("Sv", "Cg", "FSv", "FCg"))  # Custom order

## ANOVA scd ##
scd_anova <- oneway.test(SnowCoverDays ~ Location, data = scd_df_combined, var.equal = FALSE)
scd_anova
games_howell <- games_howell_test(SnowCoverDays ~ Location, data = scd_df_combined)
games_howell


#### GSL Analysis ####
#Change the labels
ft_gsl_df <- ft_gsl_df %>% mutate(Location = "FCg")
ft_sv_gsl_df <- ft_sv_gsl_df %>% mutate(Location = "FSv")
sv_gsl_df <- sv_gsl_df %>% mutate(Location = "Sv")
gsl_df <- gsl_df %>% mutate(Location = "Cg")
gsl_df_combined <- bind_rows(gsl_df, sv_gsl_df, ft_gsl_df, 
                             ft_sv_gsl_df)
#Level the locations appropriately
gsl_df_combined$Location <- factor(gsl_df_combined$Location, 
                                   levels = c("Sv", "Cg", "FSv", "FCg"))  # Custom order

## ANOVA gsl ##
gsl_anova <- oneway.test(GrowingSeasonLength ~ Location, data = gsl_df_combined, var.equal = FALSE)
gsl_anova
games_howell <- games_howell_test(GrowingSeasonLength ~ Location, data = gsl_df_combined)
games_howell
