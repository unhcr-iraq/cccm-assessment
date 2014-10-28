
source("~/unhcr_r_project/cccm-assessment/data.R")

###########################################################################################
#This creates the voronoi line segments
## function for voronoi polygon
# http://stackoverflow.com/questions/12156475/combine-voronoi-polygons-and-maps
###########################################################################################


voronoipolygons <- function(x,poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(district)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)  
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
  return(voronoi)  
}

vorototal <- voronoipolygons(coords,district)
proj4string(vorototal) <- '+proj=longlat'

writeOGR(vorototal,"out","vorototal",driver="ESRI Shapefile", overwrite_layer=TRUE)

vorototalover <- over(vorototal, datasp)
#vorototalover@data$id = as.numeric(rownames(vorototalover@data))
#vorototaloverover$id = as.numeric(rownames(vorototaloverover))
#vorototaloverall <-merge(x=spp, y=vorototaloverover, by="row.names")

#names(vorototaloverpall)


#keep only individual


#writeOGR(vorototal1,"out","vorototal1",driver="ESRI Shapefile", overwrite_layer=TRUE)


#gg <- spRbind(gg1, gg0) 
#gg <- gUnion(gg1, gg0) 
# gg <-rbind(gg,gg0, fix.duplicated.IDs=TRUE)

#rm(gg0)  

########################################################
## loop on each district

poly.data <- district[1,]
uid <- as.numeric("1")
disnumber <- length(district)
for (i in 1:disnumber)
{
  districti <- district[i,]
  assign(paste("gg",i,sep=""), gIntersection(districti,vorototal,byid=TRUE))
  temp.data <- gIntersection(districti,vorototal,byid=TRUE)
  n <- length(slot(temp.data, "polygons"))
  temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
  uid <- as.numeric( uid + n)
  poly.data <- spRbind(poly.data,temp.data)
  i <- i + 1;
}

#plot(poly.data)
#summary(poly.data)
#

## Convert SpatialPolygons in SpatialPolygonsDataFrame
IDs <- sapply(slot(poly.data, "polygons"), function(x) slot(x, "ID"))
df <- data.frame(rep(0, length(IDs)), row.names=IDs)

spp <- SpatialPolygonsDataFrame(poly.data,df)

writeOGR(spp,"out","voronoiall",driver="ESRI Shapefile", overwrite_layer=TRUE)

sppover <- over(spp, datasp)
spp@data$id = as.numeric(rownames(spp@data))
sppover$id = as.numeric(rownames(sppover))
sppall <-merge(x=spp, y=sppover, by="row.names")

names(sppall)
## put 0 instead of NA to ensure that individual will be parsed as numeric
sppall@data$individual[is.na(sppall@data$individual)] <- 0

#keep only individual
sppall1 <- sppall
sppall1 <-sppall1[,-(5)]
sppall1 <-sppall1[,-(1:3)]

sppall1 <-sppall1[,-(2:3)]

#sppall1 <-sppall1[ , c("individual")]

writeOGR(sppall1,"out","voronoi",driver="ESRI Shapefile", overwrite_layer=TRUE)


#writePolyShape(gg, "voronoi")


# Load/run the main Python script
#python.load("regionalise.py")

## Unfinieshed Building -- Abandonned Building OpenSites

sum(data$Under.construction)