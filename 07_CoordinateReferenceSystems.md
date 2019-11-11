### Setup
For the following example, we will use the following setup.
<pre><code>
library(sf)
library(ggplot2)
theme_set(theme_bw())
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
world <- ne_countries(scale = "medium", returnclass = "sf")
</code></pre>


### Using an Incorrect / Poorly Chosen CRS

<pre><code>
ggplot(data=World)+
  geom_sf()+
  coord_sf(xlim= c(175, 180), ylim=c(-20,-12.0), expand = FALSE)
</code></pre>

### Using an Incorrect / Poorly Chosen CRS

Here, the limits are taken from projected bounds of EPSG:3460.

<pre><code>
ggplot(data=world) +
  geom_sf() +
  coord_sf(
   crs = 3460, # https://epsg.io/3460
   xlim = c(1798028.61, 2337149.40), 
   ylim = c(3577110.39, 4504717.19) 
  )
</code></pre>
