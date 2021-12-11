# https://www.r-bloggers.com/how-to-combine-google-maps-with-a-choropleth-shapefile-of-holland-in-r-amsterdam-neighbourhoods-postal-codes-by-number-of-customers/

# install and load packages 
library(ggmap) 
library(RgoogleMaps) 
library(maptools) 
library(rgdal) 
library(ggplot2) 
library(rgeos)

# get the coordinates of the center of the map you want to show
 CenterOfMap <- geocode("Amsterdam")
 CenterOfMap <- geocode("52.374,4.618")

# get the map from google
Amsterdam <- get_map(c(lon = CenterOfMap$lon, lat = CenterOfMap$lat),zoom = 12, maptype = "terrain", source = "google")

# create and plot the map
AmsterdamMap <- ggmap(Amsterdam)
AmsterdamMap

# load the data you want to show in the choropleth
geo_data <- read.csv("Data.csv", header = T, sep = ";", dec = ",")


# load the shapefile
shapedata <- readOGR('.', "ESRI-PC4-2015R1")
 # transform the shapefile to something Google Maps can recognize
shapedata_trans <- spTransform(shapedata, CRS("+proj=longlat +datum=WGS84"))

# add an id to each row
shapedata_trans@data$id <- rownames(shapedata_trans@data)
# merge the data and the shapefile
shape_and_data <- merge(x = shapedata_trans, y = geo_data, by.x = "PC4", by.y = "id")

# repair self-intersections
shape_and_data_wsi <- gBuffer(shape_and_data, width = 0, byid = TRUE)

# fortify the shapefile to make it usable to plot on Google Maps
fort <- fortify(shape_and_data_wsi)
# merge the fortified shapefile with the original shapefile data
PC4 <- merge(fort,shape_and_data_wsi@data, by = "id")

# create the final map with the overlaying choropleth
AmsterdamMap_final <- AmsterdamMap + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), size = .2, color = 'black', data = PC4, alpha = 0.8) +
  coord_map() +
  scale_fill_gradient(low = "red", high = "green") + 
  theme(legend.title = element_blank(), legend.position = "bottom")
AmsterdamMap_final
