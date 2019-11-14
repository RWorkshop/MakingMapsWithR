Irish Transverse Mercator (ITM) 
===================================

Irish Transverse Mercator (ITM) is the geographic coordinate system for Ireland. 
It was implemented jointly by the Ordnance Survey Ireland (OSi) and the Ordnance Survey of Northern Ireland (OSNI) in 2001. 
The name is derived from the Transverse Mercator projection it uses and the fact that it is optimised for the island of Ireland.


* An ITM co-ordinate is generally given as a pair of two six-digit numbers (excluding any digits behind a decimal point which may be used in very precise surveying). 

* The first number is always the easting and the second is the northing. The easting and northing are in metres from the false origin.

* The ITM co-ordinate for the Spire of Dublin on O'Connell Street is: 715830, 734697

  - IRENET95 / Irish Transverse Mercator: EPSG:2157
  
* The most commonly used datum is called WGS84 (World Geodesic System 1984)

  - Colloquially "GPS" co-ordinates
  - The Corresponding WGS84 co-ordinates for the Spire are 53.349995, -6.260308

<pre><code>
library(proj4)
library(maps)
library(mapproj)
</code></pre>

Specify the projection using the projection string.

* Irish Grid (old data)
* Irish TM   (more recent)

<pre><code>

# Irish Grid 
# proj4string <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +datum=ire65 +units=m +no_defs +ellps=mod_airy +towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15"

# Irish TM
proj4string <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=0.99982 +x_0=600000 +y_0=750000 +ellps=GRS80 +units=m +no_defs"
</code></pre>

Convert the Irish TM co-ordinates for the Spire into GPS coordinates.

<pre><code>
project(c(715830 , 734697), proj=proj4string,inverse=TRUE) 
</code></pre>

Another way of doing this is using ``sp::spTransform`` , a function for map projection and datum transformation.

<pre><code>
x = 715830; y = 734697  # Spire Co-ordinates

d <- data.frame(lon=x, lat=y)

coordinates(d) <- c("lon", "lat")

proj4string(d) <- CRS("+init=epsg:2157")   #Specify the co-ordinates as Irish TM

CRS.new <- CRS("+init=epsg:4326")          #Specify the CRS we want the co-ordinates converted to 

spTransform(d, CRS.new)

</code></pre>




