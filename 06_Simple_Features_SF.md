
### A Basic Rendering

<pre><code>
ggplot(data = world) +
    geom_sf()
</code></pre>


### Adding Titles and Subtitles

A title and a subtitle can be added to the map using the function ggtitle, passing any valid character string (e.g. with quotation marks) as arguments. 
Axis names are absent by default on a map, but can be changed to something more suitable (e.g. “Longitude” and “Latitude”), depending on the map.

<pre><code>
ggplot(data = world) +
    geom_sf() +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "with labels")
</code></pre>

### Using Colour

<pre><code>
ggplot(data = world) + 
    geom_sf(color = "black", fill = "lightgreen")
</code></pre>

### chloropleths

<pre><code>
ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) 
</code></pre>


A more elaborate version can be made with additional code (outside the scope of the workshop)
<pre><code>
ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
</code></pre>

### References

* https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
