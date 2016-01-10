
###################################################################
### http://rstudio-pubs-static.s3.amazonaws.com/14465_5249bd98d3094c909457183df00581f8.html



# double-check what type of data we have
class(datasp)
plot(datasp)


## Plotting density
sSp <- as(SpatialPoints(datasp), "ppp")  # convert points to pp class

# create density object
Dens <- density(sSp,
                adjust = 0.02
)  
#class(Dens)  # just for interest: it's got it's of pixel image class

plot(Dens)  # default plot for density
contour(density(sSp, adjust = 0.02), nlevels = 6)  # plot as contours 

###Save the contour lines
## Extract the contours, first as lines and then finally as polygons
##convert the data into two more formats: a spatial grid and then as a raster image.
Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, nlevels = 9)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl, CRS(proj4string(district)))  # convert to SpatialLinesDataFrame
plot(SLDF, col = terrain.colors(8))


##Extract the density polygons
Polyclust <- gPolygonize(SLDF[5, ])
gas <- gArea(Polyclust, byid = T)/10000
Polyclust <- SpatialPolygonsDataFrame(Polyclust, data = data.frame(gas), match.ID = F)
plot(Polyclust)

# Aggregated the points within high density zones
cAg <- aggregate(datasp, by = Polyclust, FUN = length)
#lb <- gBoundary(district)
plot(Dens, main = "")
plot(district, border = "grey", lwd = 2, add = T)
plot(SLDF, col = terrain.colors(8), add = T)
plot(cAg, col = "red", border = "white", add = T)
graphics::text(coordinates(cAg) + 1000
               #, labels = cAg$CODE
)

#Save points inside and outside the cluster
sIn <- datasp[cAg, ]  # select the sites inside the clusters
sOut <- datasp[!row.names(datasp) %in% row.names(sIn), ]  # sites outside the clusters
plot(sOut)  # the more sparsely distributed points - notice the 'holes' of low density
plot(cAg, border = "red", lwd = 3, add = T)

proportion.of.points.in.cluster <- nrow(sIn)/nrow(datasp)  # proportion of points in cluster

saved.space <-gArea(cAg)/gArea(district)


