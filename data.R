#######Analysis of  CCM baseline

source("~/unhcr_r_project/cccm-assessment/packages.R")

#download.file(url = "http://ona.io/iraqcccm/exports/baseline/csv", 
#              destfile = "~/unhcr_r_project/cccm-assessment/data/data.csv")
#
rm(data)
data <- read.csv("~/unhcr_r_project/cccm-assessment/data/Baseline.csv")




names(data)

#op  Open Air
#imp	Improvised Shelter
#tent	Tents
#nat	Individual building made from natural materials (mud, simple wooden frame)
#mix	Individual building made from natural and other materials (concrete foundations)
#concr	Individual building made from concrete
#shop	Individual building with shops or an individual house with shops
#big	Big building (apartment block)
#const	Building/ house under construction


## Recognise date -- 2014-10-13T11:40:26.260Z
#data$start <- as.Date(data$start, "%Y-%m-%dT%H:%M:%S")
#data$end <- as.Date(data$end, "%Y-%m-%dT%H:%M:%S")

data$start <- as.POSIXct(data$start, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
data$end <- as.POSIXct(data$end, format="%Y-%m-%dT%H:%M:%S", tz="GMT")

data$geo <-paste(data$descript._coordinates_latitude, data$descript._coordinates_longitude, sep = ",")

data$Open <- as.numeric(ifelse(data[["physicalcondition.shelter.op"]] == "True",1, 0))
data$Improvised.Shelter <- as.numeric(ifelse(data[["physicalcondition.shelter.imp"]] == "True",1, 0))
data$Tent <- as.numeric(ifelse(data[["physicalcondition.shelter.nat"]] == "True",1, 0))
data$Individual.mud.building <- as.numeric(ifelse(data[["physicalcondition.shelter.mix"]] == "True",1, 0))
data$Individual.mix.building <- as.numeric(ifelse(data[["physicalcondition.shelter.concr"]] == "True",1, 0))
data$Individual.concrete.building <- as.numeric(ifelse(data[["physicalcondition.shelter.imp"]] == "True",1, 0))
data$Shop <- as.numeric(ifelse(data[["physicalcondition.shelter.shop"]] == "True",1, 0))
data$Apartment.block <- as.numeric(ifelse(data[["physicalcondition.shelter.big"]] == "True",1, 0))
data$Under.construction <- as.numeric(ifelse(data[["physicalcondition.shelter.const"]] == "True",1, 0))


data$unfinished <-  ifelse(data[["Under.construction"]] ==1,"Yes", "No")

data$abandonned <- ifelse(  data[["Individual.mix.building"]] +
                                 data[["Individual.mud.building"]] +
                                 data[["Shop"]]+ 
                                 data[["Individual.concrete.building"]]+
                                 data[["Apartment.block"]] >= 1,"Yes", "No")

data$opensite <- ifelse(    data[["Improvised.Shelter"]] +
                                 data[["Tent"]] +
                                 data[["Open"]] >= 1,"Yes", "No")

data$accom <- paste(
  ifelse(data[["physicalcondition.shelter.const"]] == "True","Unfinished", ""),
  ifelse(data[["physicalcondition.shelter.imp"]] == "True","Improvised", "") ,
  ifelse(data[["physicalcondition.shelter.tent"]] == "True","Tent", ""),
  ifelse(data[["physicalcondition.shelter.op"]] == "True","Open", ""),
  ifelse(data[["physicalcondition.shelter.nat"]] == "True","Mud", ""),
  ifelse(data[["physicalcondition.shelter.mix"]] == "True","Mix", ""),
  ifelse(data[["physicalcondition.shelter.concr"]] == "True","Concrete", ""),
  ifelse(data[["physicalcondition.shelter.shop"]] == "True","Shop", ""),
  ifelse(data[["physicalcondition.shelter.big"]] == "True","Appart", ""), sep=" ")

##/data$accom <- paste(ifelse(data[["physicalcondition.shelter.const"]] == "True","Unfinished", ""), sep="")

data$accom <- gsub("  ", " ", data$accom)
data$accom <- gsub("  ", " ", data$accom)
data$accom <- gsub("  ", " ", data$accom)
data$accom <- gsub("  ", " ", data$accom)


data$water_access <-as.numeric(revalue(data$physicalcondition.conditions.water_access, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$water_quality <-as.numeric(revalue(data$physicalcondition.conditions.water_quality, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$openelement <-as.numeric(revalue(data$physicalcondition.conditions.openelement, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$damage <-as.numeric(revalue(data$physicalcondition.conditions.damage, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$mines <-as.numeric(revalue(data$physicalcondition.conditions.mines, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$hazards <-as.numeric(revalue(data$physicalcondition.conditions.hazards, c("maj"="4","mod"="3", "min"="2","no"="1")))
data$fighting <-as.numeric(revalue(data$physicalcondition.conditions.fighting, c("maj"="4","mod"="3", "min"="2","no"="1")))

data$score <- data$water_access + data$water_quality +
              data$openelement + data$damage + data$mines+ data$hazards + data$fighting

data$scoreclass <- as.factor(findCols(classIntervals(data$score, n=5, style="jenks")))
data$scoreclass <-revalue(data$scoreclass, c("5"="5.None", "4"="4.Low", "3"="3.Medium", "2"="2.High", "1"="1.Extreme"))


data$class <- as.factor(findCols(classIntervals(data$descript.individual, n=6, style="fixed",fixedBreaks=c(0, 50, 100, 250, 500, 1000, 100000))))

data$class <-revalue(data$class, c("1"="a. 0-49", "2"="b. 50-99", "3"="c. 100-249", "4"="d. 250-499", "5"="e. 500-1000", "6"="f. >1000"))

data$housenu <- data$descript.individual/data$descript.population.household
data$householdnum <- as.factor(findCols(classIntervals(data$housenu,style="fixed",fixedBreaks=c(0, 3, 5, 7, 10, 50))))

data$householdnum <- revalue(data$householdnum, c("1"="1-3", "2"="4-5", "3"="6-7", "4"="8-10", "5"=" >10"))



#data$duration <- data$end - data$start 


####################
## Creation of cluster based on main observation
## http://gastonsanchez.com/blog/how-to/2012/10/13/MCA-in-R.html

acm <- dudi.acm(data[ , c( # "class" ,
                           "physicalcondition.conditions.water_access", "physicalcondition.conditions.water_quality",
                           "physicalcondition.conditions.electric",    "physicalcondition.conditions.openelement", 
                           "physicalcondition.conditions.damage",  "physicalcondition.conditions.hazards",
                           "physicalcondition.conditions.mines","physicalcondition.conditions.fighting"#, 
                         #  "physicalcondition.shelter.op" , "physicalcondition.shelter.imp" ,            
                         #  "physicalcondition.shelter.tent" ,"physicalcondition.shelter.nat",
                         #  "physicalcondition.shelter.mix", "physicalcondition.shelter.concr",
                         #  "physicalcondition.shelter.shop", "physicalcondition.shelter.big"  ,"physicalcondition.shelter.const"
                  )], scannf = FALSE, nf = 2)


chiDist <- dist.dudi(acm, amongrow = TRUE)
clustered <- ward.cluster(chiDist,
                          #peso = apply(d, 1, sum),
                          plots = TRUE, h.clust = 1)

data$cluster <- paste("Cluster", cutree(clustered, k = 5))


## Remove line carriage
## http://dodata.wordpress.com/2013/03/08/some-new-gsub-and-grep-in-r-for-irritating-carriage-returns-and-line-feed-cr-lf-crlf/
data$neighbourhood <- data$descript.neighbourhood
grep('\\R\\n', x=data$neighbourhood,value=TRUE)
gsub('\\R\\n', '', x=data$neighbourhood) -> data$neighbourhood
grep("\\n\\n", x=data$neighbourhood,value=TRUE)
gsub('\\n\\n', '', x=data$neighbourhood) -> data$neighbourhood
grep("\\n", x=data$neighbourhood,value=TRUE)
gsub('\\n', '', x=data$neighbourhood) -> data$neighbourhood

data$sitear <- data$descript.sitear
grep('\\R\\n', x=data$sitear,value=TRUE)
gsub('\\R\\n', '', x=data$sitear) -> data$sitear
grep("\\n\\n", x=data$sitear,value=TRUE)
gsub('\\n\\n', '', x=data$sitear) -> data$sitear
grep("\\n", x=data$sitear,value=TRUE)
gsub('\\n', '', x=data$sitear) -> data$sitear


data$namekey <- data$descript.namekey
grep('\\R\\n', x=data$namekey,value=TRUE)
gsub('\\R\\n', '', x=data$namekey) -> data$namekey
grep("\\n\\n", x=data$namekey,value=TRUE)
gsub('\\n\\n', '', x=data$namekey) -> data$namekey
grep("\\n", x=data$namekey,value=TRUE)
gsub('\\n', '', x=data$namekey) -> data$namekey


##
data$pcode <- data$Row.names
data$site <- paste(data$neighbourhood, data$sitear, data$namekey,  data$descript.phonekey, sep=" / ")


############################ Start correcting the gov and district

dataviz1 <-data[ , c( "descript.individual" , "descript._coordinates_longitude", "descript._coordinates_latitude")]

## get shorten version of the column to decrease dataviz size
dataviz1 <-rename(dataviz1, c( "descript._coordinates_longitude"="longitude", "descript._coordinates_latitude"="latitude",                 
                               "descript.individual"="individual"))

datasp <- dataviz1

coords <- cbind(datasp$longitude, datasp$latitude)
datasp <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

writeSpatialShape(datasp, "datasp")

district <- readShapePoly('~/unhcr_r_project/cccm-assessment/data/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))

datasp1 <- IntersectPtWithPoly(datasp, district)

correct <- datasp1@data[ ,c("A1NameEn","HRname")]
data <- merge(x=data, y=correct, by="row.names")

#data$govchk <- as.numeric(ifelse(data$A1NameEn == data$descript.governorate,0, 1))
#data$govchk <- as.numeric(ifelse(data[["A1NameEn"]] == data[["descript.governorate"]],0, 1))
#data$districtchk <- as.numeric(ifelse(data[["HRname"]] == data[["descript.district"]],0, 1))


##########################################################################################
### tentative sectorisation/redistricting of IDPS area -- cf infra vornoi.R

area <- readShapePoly('~/unhcr_r_project/cccm-assessment/out/area.shp', proj4string=CRS("+proj=longlat"))

#gplot(area)
#plot(datasp)

datasparea <- IntersectPtWithPoly(datasp, area)

datasparea1 <- datasparea@data[ ,c("name")]
#data <- merge(x=data, y=datasparea1, by="row.names")

#plot(datasparea)

areasp <- aggregate(cbind(individual ) ~ name, data = datasparea@data, FUN = sum, na.rm = TRUE)
View(areasp)

areadata <-merge(x=area, y=areasp, by="name")
writeOGR(areadata,"out","areadata",driver="ESRI Shapefile", overwrite_layer=TRUE)

####################################################################################
# Create a summary column to facilitate revsion in phase 2

data$Summary <- paste(
                    data$neighbourhood,
                    data$A1NameEn, data$HRname,
                    data$descript.population.household, data$descript.population.men, data$descript.population.women,
                    data$descript.population.boys, data$descript.population.girls, data$descript.individual,
                    data$descript.environment,
                    data$accom,                     
                    sep='\n') 

pcode <- data[ ,c("pcode", "A1NameEn","HRname","site", "sitear","neighbourhood")]
pcode <- pcode[order(pcode$A1NameEn, pcode$HRname, pcode$site, pcode$neighbourhood,pcode$sitear),]


write.table(pcode, file='out/pcode.csv', sep=';', col.names = T, row.names = F)



##################################################################
####### Create export for dataviz  ###############################
##################################################################
rm(dataviz)
dataviz <- data
names(dataviz)
#dataviz <- dataviz[,-(1:10,12,16,17,29:37,46:50),drop=FALSE]

## select the column of interest for the dataviz
dataviz <-dataviz[ , c( "sitear", "neighbourhood", "descript.organisat" ,"descript.governorate", "descript.district" ,
              "descript.photo.photoreceiver"  , "descript.environment" ,      "A1NameEn", "HRname",              
 "descript.phonekey", "householdnum",        
  "descript.population.household", "descript.population.men",
"descript.population.women" , "descript.population.boys" , "descript.population.girls",                 
 "descript.individual" , "physicalcondition.conditions.water_access", "physicalcondition.conditions.water_quality",
"physicalcondition.conditions.electric",    "physicalcondition.conditions.openelement",   "physicalcondition.conditions.damage",       
"physicalcondition.conditions.hazards","physicalcondition.conditions.mines","physicalcondition.conditions.fighting", "geo",                                       
 "Improvised.Shelter","Tent", "Individual.mud.building",                   
"Individual.mix.building","Individual.concrete.building", "Shop" ,                                     
 "Apartment.block", "Under.construction","opensite","abandonned","unfinished", "class","accom","cluster","scoreclass", "descript._coordinates_longitude", "descript._coordinates_latitude"
  )]

## get shorten version of the column to decrease dataviz size
dataviz <-rename(dataviz, c("descript.organisat"="organisat" ,
                            "descript.governorate" = "governorateor", 
                            "descript.district" = "districtor",
                            "A1NameEn"= "governorate", "HRname" = "district",  
                            
                  "descript.photo.photoreceiver"="photo"  , "descript.environment" ="environment" ,                     
                  "descript.phonekey"="phone",      "descript._coordinates_longitude"="longitude", "descript._coordinates_latitude"="latitude",   
                  "descript.population.household"="household", "descript.population.men"="men",
                  "descript.population.women"="women"  , "descript.population.boys"= "boys" , "descript.population.girls"="girls",                 
                  "descript.individual"="individual" , "physicalcondition.conditions.water_access"="water_access",
                  "physicalcondition.conditions.water_quality"="water_quality",
                  "physicalcondition.conditions.electric"="electric",    "physicalcondition.conditions.openelement"="openelement", 
                  "physicalcondition.conditions.damage"="damage",       
                  "physicalcondition.conditions.hazards"="hazards","physicalcondition.conditions.mines"="mines",
                  "physicalcondition.conditions.fighting"="fighting",                                       
                  "Improvised.Shelter"="Improvised_Shelter", "Individual.mud.building"= "Individual_mud_building",                   
                  "Individual.mix.building"="Individual_mix_building","Individual.concrete.building"="Individual_concrete_building",                                      
                  "Apartment.block"="Apartment_block", "Under.construction"="Under_construction"))

##
#levels(dataviz$water_access)

### Rename the level so that they are correctly ordered in the dc.js viz
dataviz$water_access <-revalue(dataviz$water_access, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$water_quality <-revalue(dataviz$water_quality, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$electric <-revalue(dataviz$electric, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$openelement <-revalue(dataviz$openelement, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$damage <-revalue(dataviz$damage, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$hazards <-revalue(dataviz$hazards, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$mines <-revalue(dataviz$mines, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
dataviz$fighting <-revalue(dataviz$fighting, c("maj"="1.Major","mod"="2.Moderate", "min"="3.Minor","no"="4.None"))
                              

#names(dataviz)

## write in a tsv file for the dataviz -- reason for the tsv is to keep the formatting of the column where coordinates are stored
write.table(dataviz, file='out/dataviz.tsv', quote=FALSE, sep='\t', col.names = T, row.names = F)




