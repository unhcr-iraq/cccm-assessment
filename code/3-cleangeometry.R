#######################################################
## Cleaning geomtry

#get a report of geometry validity & issues for a sp spatial object
report <- clgeo_CollectionReport(area)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]

#get suspicious features (indexes)
nv <- clgeo_SuspiciousFeatures(report)
mysp <- area[nv[-14],]

#try to clean data
mysp.clean <- clgeo_Clean(mysp, print.log = TRUE)

#check if they are still errors
report.clean <- clgeo_CollectionReport(mysp.clean)
summary.clean <- clgeo_SummaryReport(report.clean)
issues.clean <- report[report$valid == FALSE,]

# use checkPolygonsHoles() to make sure that the holes are correctly
# defined (the input file has a single ring defined as a hole
# which is illogical, holes have to be within something else
slot(areadata, "polygons") <- lapply(slot(areadata, "polygons"), checkPolygonsHoles)

# next run unionSpatialPolygons() to merge the Polygons objects that
# belong to the same name
areadata1 <- unionSpatialPolygons(areadata, as.character(areadata$name))