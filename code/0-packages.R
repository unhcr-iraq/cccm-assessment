################################################################
### Uncomment to load the packages used in this analysis
#lab.packages <- c("lattice", "gmodels", "car","ggplot2","extrafont","ggthemes","zoo","reshape2",
#"maptools","rgdal","rgeos","ggmap","sp","hexbin",")
#install.packages(pkgs=lab.packages)

packages <- c("ggplot2", # package for elegant data visualization using the Grammar of Graphics
              "Hmisc", # generate a detailled describtion of a given dataset 
              "AER",  # interesting datasets
              "lattice", 
              "MASS", 
              "gvlma",
              "VGAM",
              "aod",
              "fields", 
              "scatterplot3d", "cluster", 
              "ade4",  "psych", 
              "stringr", # manipulation of string data
              "ellipse",
              "pastecs","car","XML",
              "devtools", # package used to load packages hosted in github -- install CURL before and separately
              "plyr",
              "vcd", # Visualisation of categorical data
              "reshape2", # package to easily melt data to long form
              "RColorBrewer", # a package offering color palette from 
              "extrafont", ##" load additional font
              "sp","maptools","rgdal","rgeos","ggmap","PBSmapping", "spatstat", ## packages used for the maps --
              ## install gdal and geos separately before  http://robinlovelace.net/r/2013/11/26/installing-rgdal-on-ubuntu.html
              "raster","classInt","lubridate","date","gdata","gridExtra","scales",
              "ggthemes", ## load different custmised theme: excel, stata, economist, tufte, wall street journal...
              "xkcd", ## Style from the xkcd comics 
              "deldir", ## for voronoi
              "FactoClass", ## package for ward clustering
              "ade4" ## package for multiplace correspodance analysis
              #"XLConnect", ## Read and write excel files
)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

# loads packages into memory
library(lattice)
library(car)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast

library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(plyr)
#gpclibPermit()
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)

library(FactoClass)
library(ade4)
library(deldir)

library(sp)  # add spatial package to load the S4 spatial objects
library(spatstat)  # to calculate field of point density
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
#library(PBSmapping)
library(ggmap) ## get background map from google map

library(raster) ## Managing raster dataset

library(devtools)
#install_git("git://github.com/gsk3/taRifx.geo.git")



library(devtools)
#install_github("cleangeo", "eblondel")
#library(cleangeo)


#
format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}


# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}





### Customised theme
### http://docs.ggplot2.org/dev/vignettes/themes.html
# theme_edouard()

theme_edouard <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.6 , lineheight = 0.9, vjust = 1),element_text(family="Helvetica"),
    axis.text.y =       theme_text(size = base_size * 0.6, lineheight = 0.9, hjust = 1),element_text(family="Helvetica"),
    axis.ticks =        theme_segment(colour = "black", size = 0.2),
    axis.title.x =      theme_text(size = base_size, vjust = 1),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
    
    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = "grey80"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.7),element_text(family="Helvetica"),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),element_text(family="Helvetica"),
    legend.position =   "right",
    
    panel.background =  theme_rect(fill = "white", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  theme_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  theme_rect(fill = "grey80", colour = "grey50"), 
    strip.text.x =      theme_text(size = base_size * 0.6),element_text(family="Helvetica", face="italic"),
    strip.text.y =      theme_text(size = base_size * 0.6, angle = -90),element_text(family="Helvetica", face="italic"),
    
    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2),element_text(family="Helvetica", face="bold"),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

IntersectPtWithPoly <- function(x, y) {
  # Extracts values from a SpatialPolygonDataFrame with
  # SpatialPointsDataFrame, and appends table (similar to
                                             # ArcGIS intersect)
                                             # Args:
                                             #   x: SpatialPoints*Frame
                                             #   y: SpatialPolygonsDataFrame
                                             # Returns:
                                             # SpatialPointsDataFrame with appended table of polygon attributes
                                             
                                             # Set up overlay with new column of join IDs in x
                                             z <- overlay(y, x)
                                             
                                             # Bind captured data to points dataframe
                                             x2 <- cbind(x, z)
                                             
                                             # Make it back into a SpatialPointsDataFrame
                                             # Account for different coordinate variable names
                                             if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) {
                                               coordinates(x2) <- ~coords.x1 + coords.x2
                                             } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) {
                                               coordinates(x2) <- ~x + y
                                             }
                                             
                                             # Reassign its projection if it has one
                                             if(is.na(CRSargs(x@proj4string)) == "FALSE") {
                                               x2@proj4string <- x@proj4string
                                             }
                                             return(x2)
}

getLabelPoint <- # Returns a county-named list of label points
  function(county) {Polygon(county[c('long', 'lat')])@labpt}
