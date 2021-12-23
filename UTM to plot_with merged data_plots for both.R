library(sf)
library(leaflet)
lct <- read.csv("/Users/amilzz/Desktop/Mapping Samples/Metadata_LCT_Rapture samples.csv")


#Conversion of data frame to sf object
lct_sf <- st_as_sf(lct, coords = c("UTM", "UTM.1"),
                   crs = "+proj=utm +zone=11") # Guessing here since it may be UTM 10 or 11 or 10.5

#Projection transformation
lct_sf <- st_transform(lct_sf, crs = "+proj=longlat +datum=WGS84")
lct_sf <- cbind(lct_sf, st_coordinates(lct_sf))

View(lct_sf)


#Leaflet map with simple and imagery basemaps
m <- leaflet(lct_sf) %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Base") %>%
  addLayersControl(baseGroups = c("Base","Imagery"), options = layersControlOptions(collapsed = FALSE))%>% 
  addMarkers(popup = as.character(lct_sf$Genetic.Year), label = as.character(lct_sf$Creek))
m  # Print the map

#ggplot of points 
##Static ggplot map
(ggplot(lct_sf) + geom_sf(aes(color = Basin_Code)) + theme_minimal())
plot<-ggplot(lct_sf) + geom_sf(aes(color = Basin_Code)) + theme_minimal() 
plot   
dev.off()
#print png
png("C:/Users/ejholmes/Downloads/Alanas_awesome_LCT_genetics_data_map.png", 
    units = "in", res = 500, height = 5, width = 5)

##ggplot with map
library(ggmap)
library("rnaturalearth")
library("rnaturalearthdata")

library(ggplot2)  # The grammar of graphics package
library(maps)     # Provides latitude and longitude data for various maps
MainStates <- map_data("state")
NewStates <- filter(MainStates,region ==  "california" | region ==
                      "nevada" | region ==  "oregon")

## sample sites + spatial: Try again from https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
  library("maps")
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  head(states)
  states <- cbind(states, st_coordinates(st_centroid(states)))
  library("tools")
  states$ID <- toTitleCase(states$ID)
  head(states)

##plot mikes rapture samples with map and coords
  ggplot(lct_sf) + geom_sf(aes(color = Basin)) + theme_minimal() +
    geom_sf(data = states, fill = NA) + 
    coord_sf(xlim = c(-121, -113.5), ylim = c(35, 43), expand = FALSE)
  
# Write out your data to a csv with the lat lon data in the x and y columns
write.csv(lct_sf, "/Users/amilzz/Desktop/Mapping Samples/Table_Thetas_hyb_PVA_03062020_withlatlons.csv")

##Add this after the write.csv() step
st_write(lct_sf, "C:/Users/ejholmes/Downloads/LCT.kml", driver='kml', update=TRUE)


##merged data travis and mike's rapture
merged2<-read.csv("/Users/amilzz/Desktop/Mapping Samples/merged_samples_lat_long_try2.csv")
View(merged2)

##plotting the merged data from travis and mike, sf is a spatial feature (12/22/21)
merged_sf <- st_as_sf(merged2, coords = c("X", "Y"),
                   crs = "+proj=longlat +datum=WGS84")

##how to merge columns separated by a comma 
merged$lat_long <- paste(merged$geometry, merged$X.2, sep = ",")


##making a .html with the merged data
m <- leaflet(merged_sf) %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Base") %>%
  addLayersControl(baseGroups = c("Base","Imagery"), options = layersControlOptions(collapsed = FALSE))%>% 
  addMarkers(popup = as.character(merged_sf$Genetic.Year), label = as.character(merged_sf$Creek))
m 

##output .csv
write.csv(merged_sf, "/Users/amilzz/Desktop/Mapping Samples/merged_samples_lat_long.csv")

##making a map using ggplot with lat and long on x & y axis
ggplot(merged_sf) + geom_sf(aes(color = Basin)) + theme_minimal() +
  geom_sf(data = states, fill = NA) + 
  coord_sf(xlim = c(-121, -113.5), ylim = c(35, 43), expand = FALSE)

