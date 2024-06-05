setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Local Public Finance/2nd Assignment (Presentation)/Data")

library(tidyverse)
library(readxl)
library(tmap)
library(sf)

print(list.files <- list.files())

tourism <- read_xlsx("IJASE-AnalysisofTheRoleofTourisminTheEconomyinIndonesia.xlsx")
tourism
#cleaning data
tourism <- tourism%>%
  na.omit(tourism)

tourism$year <- as.numeric(tourism$year)

#Recheck total province in the data 
length(unique(tourism$province))
str(tourism)


# Data Distribution before log natural -------------------------------------------------------
#variable 1
ggplot(tourism, aes(x = HDI)) +
  geom_histogram()
#variable 2
ggplot(tourism, aes(x = `log_GRDP`)) +
  geom_histogram()
#variable 3
ggplot(tourism, aes(x = `log_unemployment`)) +
  geom_histogram()
#variable 4
ggplot(tourism, aes(x = `log_FDI`)) +
  geom_histogram()
#variable 5
ggplot(tourism, aes(x = `log_DDI`)) +
  geom_histogram()
#variable 6
ggplot(tourism, aes(x = `log_foreign_tourist`)) +
  geom_histogram()
#variable 7
ggplot(tourism, aes(x = `log_domestic_tourist`)) +
  geom_histogram()
#variable 8
ggplot(tourism, aes(x = `log_tourism_employment`)) +
  geom_histogram()
#variable 9
ggplot(tourism, aes(x = `log_accommodation`)) +
  geom_histogram()
#variable 10
ggplot(tourism, aes(x = `log_pop_density`)) +
  geom_histogram()

# Rename province's name -----------------------------------------------------
province_names <- unique(tourism$province)
province_names <- sort(province_names)
province_names

# Urutkan nama provinsi
province_names <- sort(province_names)
province_names

# Plotting to the MAP -----------------------------------------------------

#Step 1
idn <- st_read("gadm41_IDN.gpkg")
st_layers("gadm41_IDN.gpkg")

#level 2 (province level)
idn.lv2 <- st_read("gadm41_IDN.gpkg", layer = "ADM_ADM_1")
#Rename
idn.lv2 <- idn.lv2 %>%
  rename(Province = NAME_1)
print(idn.lv2$Province)
glimpse(idn.lv2)
head(idn.lv2$NAME_1, n = 34)
plot(st_geometry(idn.lv2))


#step 2
FinalData <- inner_join(idn.lv2, tourism, by = join_by(Province == province))
str(FinalData)

#Step 3 

tmap_mode("plot")
# HDI 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_HDI_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "HDI", palette = "YlGnBu", n = 7,  
          title = "HDI (2018)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black") 

# Print Map
idn_HDI_2018
#download
tmap_save(idn_HDI_2018, "HDI(2018).png", width = 10, height = 8)
# HDI 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map
idn_HDI_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "HDI", palette = "YlGnBu", n = 7,  
          title = "HDI (2019)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_HDI_2019
#download
tmap_save(idn_HDI_2019, "HDI(2019).png", width = 10, height = 8)

# HDI 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_HDI_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "HDI", palette = "YlGnBu", n = 7,  
          title = "HDI (2020)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_HDI_2020
#download
tmap_save(idn_HDI_2020, "HDI(2020).png", width = 10, height = 8)

# GRDP 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_GRDP_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "GRDP", palette = "Blues", n = 7,  
          title = "GRDP (2018)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_GRDP_2018

#download
tmap_save(idn_GRDP_2018, "GRDP(2018).png", width = 10, height = 8)

# GRDP 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_GRDP_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "GRDP", palette = "Blues", n = 7,  
          title = "GRDP (2019)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_GRDP_2019
#download
tmap_save(idn_GRDP_2019, "GRDP(2019).png", width = 10, height = 8)

# GRDP 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_GRDP_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "GRDP", palette = "Blues", n = 7,  
          title = "GRDP (2020)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_GRDP_2020
#download
tmap_save(idn_GRDP_2020, "GRDP(2020).png", width = 10, height = 8)

# Foreign Tourist 2018------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_FT_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "foreign_tourist", palette = "Reds", n = 7,  
          title = "Foreign Tourist (2018)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_FT_2018
# Download
tmap_save(idn_FT_2018, "foreign_tourist(2018).png", width = 10, height = 8)

# Foreign Tourist 2019------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_FT_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "foreign_tourist", palette = "Reds", n = 7,  
          title = "Foreign Tourist (2019)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_FT_2019
#download
tmap_save(idn_FT_2019, "foreign_tourist(2019).png", width = 10, height = 8)


# Foreign Tourist 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_FT_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "foreign_tourist", palette = "Reds", n = 7,  
          title = "Foreign Tourist (2020)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_FT_2020
#download
tmap_save(idn_FT_2020, "foreign_tourist(2020).png", width = 10, height = 8)


# Domestic Tourist 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_DT_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "domestic_tourist", palette = "Greens", n = 7,  
          title = "Domestic Tourist (2018)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_DT_2018

#download
tmap_save(idn_DT_2018, "domestic_tourist(2018).png", width = 10, height = 8)

# Domestic Tourist 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_DT_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "domestic_tourist", palette = "Greens", n = 7,  
          title = "Domestic Tourist (2019)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_DT_2019
#download
tmap_save(idn_DT_2019, "domestic_tourist(2019).png", width = 10, height = 8)

# Domestic Tourist 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_DT_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "domestic_tourist", palette = "Greens", n = 7,  
          title = "Domestic Tourist (2020)", style = "cont") +
  tm_borders(col = "white", lwd = .01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_DT_2020
#download
tmap_save(idn_DT_2020, "domestic_tourist(2020).png", width = 10, height = 8)

# Population Density 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map
idn_PD_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "pop_density", palette = "Oranges", n = 3,  
          title = "PopDensity (2018)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_PD_2018
#download
tmap_save(idn_PD_2018, "Population Density(2018).png", width = 10, height = 8)

# Population Density 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_PD_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "pop_density", palette = "Blues", n = 5,  
          title = "Pop Density (2019)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_PD_2019
#download
tmap_save(idn_PD_2019, "Population Density(2019).png", width = 10, height = 8)

# Population Density 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_PD_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "pop_density", palette = "Blues", n = 5,  
          title = "Pop Density (2020)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_PD_2020
#download
tmap_save(idn_PD_2020, "Population Density(2020).png", width = 10, height = 8)

# tourism_employment 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_TE_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "tourism_employment", palette = "GnBu", n = 5,  
          title = "Tourism Emplyr(2018)", style = "cont", position = c("center", "upper")) +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_TE_2018
#download
tmap_save(idn_TE_2018, "Tourism Employment(2018).png", width = 10, height = 8)

# tourism_employment 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map with only the province with the highest HDI marked
idn_TE_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "tourism_employment", palette = "Blues", n = 5,  
          title = "Tourism Emplyr (2019)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_TE_2019
#download
tmap_save(idn_TE_2019, "Tourism Employment(2019).png", width = 10, height = 8)

# tourism_employment 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_TE_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "tourism_employment", palette = "Blues", n = 5,  
          title = "Tourism Emplyr (2020)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_TE_2020
#download
tmap_save(idn_TE_2020, "Tourism Employment(2020).png", width = 10, height = 8)

unique(FinalData <- FinalData %>%
        select(Province, pop_density))

# accomodation 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_inf_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "accommodation", palette = "Blues", n = 5,  
          title = "Accommodation (2018)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_inf_2018
#download
tmap_save(idn_inf_2018, "accommodation (2018).png", width = 10, height = 8)

# accomodation 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_inf_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "accommodation", palette = "Blues", n = 5,  
          title = "Accommodation (2019)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_inf_2019
#download
tmap_save(idn_inf_2019, "accommodation (2019).png", width = 10, height = 8)

# accomodation 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_inf_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "accommodation", palette = "Blues", n = 5,  
          title = "Accommodation (2020)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_inf_2020 
#download
tmap_save(idn_inf_2020 , "accommodation (2020).png", width = 10, height = 8)

# Unemployment 2020 ------------------------------------------------------------
FinalData_2020 <- FinalData %>%
  filter(year == 2020)

# Create the map 
idn_un_2020 <- tm_shape(FinalData_2020) +
  tm_fill(col = "unemployment", palette = "YlGnBu", n = 5,  
          title = "Unemployment (2020)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_un_2020
#download
tmap_save(idn_un_2020 , "Unemployment (2020).png", width = 10, height = 8)

# Unemployment 2019 ------------------------------------------------------------
FinalData_2019 <- FinalData %>%
  filter(year == 2019)

# Create the map 
idn_un_2019 <- tm_shape(FinalData_2019) +
  tm_fill(col = "unemployment", palette = "YlGnBu", n = 5,  
          title = "Unemployment (2019)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_un_2019

# Unemployment 2018 ------------------------------------------------------------
FinalData_2018 <- FinalData %>%
  filter(year == 2018)

# Create the map 
idn_un_2018 <- tm_shape(FinalData_2018) +
  tm_fill(col = "unemployment", palette = "YlGnBu", n = 5,  
          title = "Unemployment (2018)", style = "cont") +
  tm_borders(col = "black", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
idn_un_2018


# Map for cluster  ---------------------------------------------------------------------

library("tidycensus")
library("tidyverse")
library("readxl")
library("dplyr")
library("tmap")
library("tmaptools")
library("shiny")
library("shinyjs")
library("spatialreg")
library("splm")
library("tigris")
options(tigris_use_cache = TRUE)
library("sf")
library("spdep")
st_drivers()

#Contiguity-based neighbors
neighbors <- poly2nb(FinalData, queen = TRUE)
summary(neighbors)
neighbors[[1]]
idn.centroid <- st_centroid(st_geometry(FinalData),
                            of_largest_polygon = TRUE)
idn.queenlines <- nb2lines(nb = neighbors,
                           coords = idn.centroid,
                           as_sf = TRUE)

# Plot
FinalData %>%
  ggplot() + geom_sf(alpha=0.2) +
  geom_sf(data = idn.centroid, alpha=0.5) +
  geom_sf(data = idn.queenlines, color="red") +
  labs(
    title = "Contiguity-based Neighbors Map",
    subtitle = "Using Queen's Case Neighbor Definition"
  )  +
  theme_classic()


#Proximity-based neighbors
idn.knn <- knn2nb(knearneigh(idn.centroid, k = 3))
idn.dist <- dnearneigh(idn.centroid, d1=0, d2=5000)
idn.knnlines <- nb2lines(nb = idn.knn, coords = idn.centroid, as_sf = TRUE)
idn.distlines <- nb2lines(nb = idn.dist, coords = idn.centroid, as_sf = TRUE)

#idn.knnlines
FinalData %>%
  ggplot() + geom_sf(alpha=0.2) +
  geom_sf(data = idn.centroid, alpha=0.5) +
  geom_sf(data = idn.knnlines, color="red") +
  labs(
    title = "Proximity-based Neighbors Map",
    subtitle = "Using count k-nearest centroids Case Neighbor Definition"
  )  +
  theme_classic()

#idn.distlines
FinalData %>%
  ggplot() + geom_sf(alpha=0.2) +
  geom_sf(data = idn.centroid, alpha=0.5) +
  geom_sf(data = idn.distlines, color= "red") +
  labs(
    title = "Proximity-based Neighbors Map",
    subtitle = "Using a Fixed range of distance Case Neighbor Definition"
  )  +
  theme_classic()




# Model 1 GRDP with LOG ------------------------------------------------------------
# Spatial weights-------------------------------------------------------------------------
# Spatial weights-------------------------------------------------------------------------

weights <- nb2listw(idn.knn, style = "W")
weights$weights[[1]]
FinalData$lag_GRDP <- lag.listw(weights , FinalData$log_GRDP)

ggplot(FinalData, aes(x = log_GRDP, y = lag_GRDP)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method="lm", color = "red") +
  theme_minimal() +
  labs(title = "GRDP by Provinces, Indonesia",
       x = "GRDP",
       y = "Spatial lag, GRDP",
       caption = "Data source: 2018-2020 GRDP from Statistics Indonesia.
       Spatial relationships based on K-nearest neighbors for each point."
  )

moran.test(FinalData$lag_GRDP, weights)
# Ensure the provinces are in the same order as you intend to map them
province_names <- unique(FinalData$Province)

# Create numeric IDs from 1 to the number of unique provinces
numeric_ids <- 1:102

# Create a named vector for mapping
province_to_id <- setNames(numeric_ids, province_names)

# Add the numeric IDs to your panel data
FinalData <- FinalData %>%
  mutate(ID = province_to_id[Province])

print("Regions in pdata but not in listw after standardization:")
print(regions_in_data_not_in_weights)

print("Regions in listw but not in pdata after standardization:")
print(regions_in_weights_not_in_data)

102 <- read_excel("/Users/ignatiusharry/Downloads/1 to 102.xlsx")

pdata <- pdata.frame(FinalData, index = c("ID", "year"))


# Model formula
formula <- log_GRDP ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density

# Estimating the Spatial Durbin Model
model_sdm <- spml(formula, data = pdata, listw = weights, model = "within", lag = TRUE, spatial.error = "none")
summary(model_sdm)

# Local G statistic -------------------------------------------------------

localg_weights <- nb2listw(include.self(idn.knn))
localG_values <- localG(FinalData$log_GRDP, localg_weights)

#local G* Statistic Map
ggplot(FinalData) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() + labs(fill = "Local Gi* statistic") +
  labs(fill = "Local Gi* statistic", title = "Local G* Statistic Map", subtitle = "Using Local Gi* for GRDP")


FinalData <- FinalData %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"))

ggplot(FinalData) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  labs(title = "Spatial Distribution of Hotspots",
       subtitle = "Using LocalG statistic",
       fill = "Hotspot Level") +  # Legend title
  theme_void()


# Regression with Spatial Durbin Model ------------------------------------

# Prepare the panel data
# Assuming pdata is your panel data frame already set up for plm
pdata <- pdata.frame(FinalData, index = c("Province", "year"))

# Model formula
formula <- log_GRDP ~ HDI + log_accommodation + log_domestic_tourist + log_foreign_tourist + log_FDI + log_DDI + log_pop_density
summary(formula)

# Estimating the Spatial Durbin Model
model_sdm <- spml(formula, data = pdata, listw = hotspot, model = "within", lag = TRUE, spatial.error = "none")
summary(model_sdm)
# Local Indicators of Spatial Association ---------------------------------

set.seed(1983)
FinalData$scaled_GRDP <- as.numeric(scale(FinalData$lag_GRDP))
FinalData_lisa <- localmoran_perm(
  FinalData$scaled_GRDP,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))


FinalData_lisa_df <- FinalData %>%
  select(scaled_GRDP) %>%
  mutate(lagged_GRDP = lag.listw(weights, scaled_GRDP)) %>%
  bind_cols(data.frame(GID_1 = FinalData$GID_1))  # Assuming GEOID is a column in your shapefile data

# Check the structure of FinalData_lisa_df 
str(FinalData_lisa_df)


FinalData_lisa_df <- FinalData %>%
  select(GID_1, scaled_GRDP) %>%
  mutate(lagged_GRDP = lag.listw(weights, scaled_GRDP)) %>%
  bind_cols(FinalData_lisa)
FinalData_lisa_df

FinalData_lisa_clusters <- FinalData_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_GRDP > 0 & local_i > 0 ~ "High-high",
    scaled_GRDP > 0 & local_i < 0 ~ "High-low",
    scaled_GRDP < 0 & local_i > 0 ~ "Low-low",
    scaled_GRDP < 0 & local_i < 0 ~ "Low-high"
  ))
color_values <- c(`High-high` = "red",
                  `High-low` = "pink",
                  `Low-low` = "blue",
                  `Low-high` = "lightblue",
                  `Not significant` = "white")

#LISA Quadrant Plot
ggplot(FinalData_lisa_clusters, aes(x = scaled_GRDP,
                                    y = lagged_GRDP,
                                    fill = lisa_cluster)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Quadrant Plot",  # Naming the plot
       x = "GRDP (z-score)",
       y = "Spatial lag of GRDP (z-score)",
       fill = "Cluster type")

ggplot(FinalData_lisa_clusters, aes(fill = lisa_cluster)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Cluster Map for GRDP",  # Naming the map
       fill = "Cluster type")


# Model 2: Unemployment with Log ---------------------------------------------------

# Spatial weights-------------------------------------------------------------------------
weights2 <- nb2listw(idn.knn, style = "W")
weights2$weights [[1]]
FinalData$lag_unemployment <- lag.listw(weights2 , FinalData$log_unemployment)

ggplot(FinalData, aes(x = log_unemployment, y = lag_unemployment)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method="lm", color = "red") +
  theme_minimal() +
  labs(title = "Unemployment by Provinces, Indonesia",
       x = "Unemployment",
       y = "Spatial lag, Unemployment",
       caption = "Data source: 2018-2020 Unemployment from Statistics Indonesia.
       Spatial relationships based on K-nearest neighbors for each point."
  )

moran.test(FinalData$lag_unemployment, weights2)

# Local G statistic -------------------------------------------------------

localg_weights2 <- nb2listw(include.self(idn.knn))
localG_values2 <- localG(FinalData$log_unemployment, localg_weights2)

#local G* Statistic Map
ggplot(FinalData) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() + labs(fill = "Local Gi* statistic") +
  labs(fill = "Local Gi* statistic", title = "Local G* Statistic Map", subtitle = "Using Local Gi* for Unemployment")


FinalData <- FinalData %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"))

ggplot(FinalData) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  labs(title = "Spatial Distribution of Hotspots",
       subtitle = "Using LocalG statistic",
       fill = "Hotspot Level") +  # Legend title
  theme_void()

# Local Indicators of Spatial Association ---------------------------------


set.seed(1983)
FinalData$scaled_unemployment <- as.numeric(scale(FinalData$lag_unemployment))
FinalData_lisa2 <- localmoran_perm(
  FinalData$scaled_unemployment,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))


FinalData_lisa_df2 <- FinalData %>%
  select(scaled_unemployment) %>%
  mutate(lagged_unemployment = lag.listw(weights2, scaled_unemployment)) %>%
  bind_cols(data.frame(GID_1 = FinalData$GID_1))  # Assuming GEOID is a column in your shapefile data

# Check the structure of FinalData_lisa_df to ensure GEOID column is added correctly
str(FinalData_lisa_df2)


FinalData_lisa_df2 <- FinalData %>%
  select(GID_1, scaled_unemployment) %>%
  mutate(lagged_unemployment = lag.listw(weights2, scaled_unemployment)) %>%
  bind_cols(FinalData_lisa2)
FinalData_lisa_df2

FinalData_lisa_clusters2 <- FinalData_lisa_df2 %>%
  mutate(lisa_cluster2 = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_unemployment > 0 & local_i > 0 ~ "High-high",
    scaled_unemployment > 0 & local_i < 0 ~ "High-low",
    scaled_unemployment < 0 & local_i > 0 ~ "Low-low",
    scaled_unemployment < 0 & local_i < 0 ~ "Low-high"
  ))
color_values2 <- c(`High-high` = "red",
                  `High-low` = "pink",
                  `Low-low` = "blue",
                  `Low-high` = "lightblue",
                  `Not significant` = "white")

#LISA Quadrant Plot
ggplot(FinalData_lisa_clusters2, aes(x = scaled_unemployment,
                                    y = lagged_unemployment,
                                    fill = lisa_cluster2)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Quadrant Plot for Unemployment",  # Naming the plot
       x = "Unemployment (z-score)",
       y = "Spatial lag of Unemployment (z-score)",
       fill = "Cluster type")

#Lisa CLuster Map
ggplot(FinalData_lisa_clusters2, aes(fill = lisa_cluster2)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Cluster Map for Unemployment",  # Naming the map
       fill = "Cluster type")

# Model 2: Unemployment Without Log---------------------------------------------------

# Spatial weights-------------------------------------------------------------------------
weights2 <- nb2listw(idn.knn, style = "W")
weights2$weights [[1]]
FinalData$lag_unemployment <- lag.listw(weights2 , FinalData$unemployment)

ggplot(FinalData, aes(x = log_unemployment, y = lag_unemployment)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method="lm", color = "red") +
  theme_minimal() +
  labs(title = "Unemployment by Provinces, Indonesia",
       x = "Unemployment",
       y = "Spatial lag, Unemployment",
       caption = "Data source: 2018-2020 Unemployment from Statistics Indonesia.
       Spatial relationships based on K-nearest neighbors for each point."
  )

moran.test(FinalData$lag_unemployment, weights2)

# Local G statistic -------------------------------------------------------

localg_weights2 <- nb2listw(include.self(idn.knn))
localG_values2 <- localG(FinalData$log_unemployment, localg_weights2)

#local G* Statistic Map
ggplot(FinalData) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() + labs(fill = "Local Gi* statistic") +
  labs(fill = "Local Gi* statistic", title = "Local G* Statistic Map", subtitle = "Using Local Gi* for Unemployment")


FinalData <- FinalData %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"))

ggplot(FinalData) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  labs(title = "Spatial Distribution of Hotspots",
       subtitle = "Using LocalG statistic",
       fill = "Hotspot Level") +  # Legend title
  theme_void()

# Local Indicators of Spatial Association ---------------------------------


set.seed(1983)
FinalData$scaled_unemployment <- as.numeric(scale(FinalData$lag_unemployment))
FinalData_lisa2 <- localmoran_perm(
  FinalData$scaled_unemployment,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))


FinalData_lisa_df2 <- FinalData %>%
  select(scaled_unemployment) %>%
  mutate(lagged_unemployment = lag.listw(weights2, scaled_unemployment)) %>%
  bind_cols(data.frame(GID_1 = FinalData$GID_1))  # Assuming GEOID is a column in your shapefile data

# Check the structure of FinalData_lisa_df to ensure GEOID column is added correctly
str(FinalData_lisa_df2)


FinalData_lisa_df2 <- FinalData %>%
  select(GID_1, scaled_unemployment) %>%
  mutate(lagged_unemployment = lag.listw(weights2, scaled_unemployment)) %>%
  bind_cols(FinalData_lisa2)
FinalData_lisa_df2

FinalData_lisa_clusters2 <- FinalData_lisa_df2 %>%
  mutate(lisa_cluster2 = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_unemployment > 0 & local_i > 0 ~ "High-high",
    scaled_unemployment > 0 & local_i < 0 ~ "High-low",
    scaled_unemployment < 0 & local_i > 0 ~ "Low-low",
    scaled_unemployment < 0 & local_i < 0 ~ "Low-high"
  ))
color_values2 <- c(`High-high` = "red",
                   `High-low` = "pink",
                   `Low-low` = "blue",
                   `Low-high` = "lightblue",
                   `Not significant` = "white")

#LISA Quadrant Plot
ggplot(FinalData_lisa_clusters2, aes(x = scaled_unemployment,
                                     y = lagged_unemployment,
                                     fill = lisa_cluster2)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Quadrant Plot for Unemployment",  # Naming the plot
       x = "Unemployment (z-score)",
       y = "Spatial lag of Unemployment (z-score)",
       fill = "Cluster type")

ggplot(FinalData_lisa_clusters2, aes(fill = lisa_cluster2)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(title = "LISA Cluster Map for Unemployment",  # Naming the map
       fill = "Cluster type")


