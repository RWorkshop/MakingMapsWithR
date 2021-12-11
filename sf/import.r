# pryr: Tools for Computing on the Language. 

# Useful tools to pry back the covers of R and understand the language at a deeper level.




install.packages("pryr")

library(pryr)

library(ggplot2)

library(sf)



list.files()


download.file("http://biogeo.ucdavis.edu/data/diva/adm/IRL_adm.zip",
   destfile = "counties.zip")


unzip("counties.zip")



counties <- sf::read_sf("IRL_adm1.shp")

ggplot(counties)+geom_sf()


pryr::object_size(counties)



counties_1k <- sf::st_simplify(counties, preserveTopology = TRUE, dTolerance = 1000)

pryr::object_size(counties_1k)

plot(counties_1k)


counties_10k <- sf::st_simplify(counties, preserveTopology = TRUE, dTolerance = 10000)

pryr::object_size(counties_10k)

plot(counties_10k)




counties_2k <- sf::st_simplify(counties, preserveTopology = TRUE, dTolerance = 2000)

pryr::object_size(counties_2k)

plot(counties_2k)

