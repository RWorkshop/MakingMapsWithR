### Setup

<pre><code>
library(sf)
library(ggplot2)
theme_set(theme_bw())
library("ggspatial")
</code></pre>

### Checking the CRS


<pre><code>

require(sf)

nc = st_read(system.file("shape/nc.shp", package="sf"))

st_crs(nc)

</code></pre>


 - Information about US CRS systems here: https://gisgeography.com/geodetic-datums-nad27-nad83-wgs84/

 - Try out the commands: ``st_proj_info(type = "proj")`` and ``st_proj_info(type = "proj")``




### Changing the CRS


* ``st_transform`` transforms / convert Coordinates Of Simple Feature object (R package: {sf}).


<pre><code>

INPUTPATH <- "C:/WorkArea/Shapefiles/Ireland/"

Ireland <- st_read(INPUTPATH)

Ireland <- st_transform(Ireland, "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=0.99982 +x_0=600000 +y_0=750000 +ellps=GRS80 +units=m +no_defs")

Ireland <- st_transform(Ireland, "+proj=longlat +datum=WGS84 +no_defs")

</code></pre>

### Using ``coord_sf()``

For the following example, we will use the following setup.

<pre><code>
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
</code></pre>

Using an incorrect / poorly chosen CRS for Fiji

<pre><code>
ggplot(data=World)+
  geom_sf()+
  coord_sf(xlim= c(175, 180), ylim=c(-20,-12.0), expand = FALSE)
</code></pre>

Using an appropriate CRS for Fiji (EPSG:3460)

Here, the limits are taken from projected bounds of EPSG:3460. (https://epsg.io/3460)

<pre><code>
ggplot(data=world) +
  geom_sf() +
  coord_sf(
   crs = 3460, 
   xlim = c(1798028.61, 2337149.40), 
   ylim = c(3577110.39, 4504717.19) 
  )
</code></pre>
