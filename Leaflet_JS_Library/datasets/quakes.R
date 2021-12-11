librar(quakes)

leaflet(data=quakes[10:55,]) %>% addTiles() %>% addMarkers(~long,~lat)


head(quakes)
