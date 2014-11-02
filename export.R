
source("~/unhcr_r_project/cccm-assessment/data.R")

##########################################################################################
############################ Start correcting the gov and district

dataviz1 <-data[ , c("pcode", "descript.individual" ,  "descript.organisat", "descript._coordinates_longitude", "descript._coordinates_latitude")]

## get shorten version of the column to decrease dataviz size
dataviz1 <-rename(dataviz1, c( "descript.organisat"="organisat" , "descript._coordinates_longitude"="longitude", "descript._coordinates_latitude"="latitude",                 
                               "descript.individual"="individual"))

datasp <- dataviz1

coords <- cbind(datasp$longitude, datasp$latitude)
datasp <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

writeSpatialShape(datasp, "out/datasp")

district <- readShapePoly('~/unhcr_r_project/cccm-assessment/data/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))

datasp1 <- IntersectPtWithPoly(datasp, district)

correct <- datasp1@data[ ,c("pcode","A1NameEn","HRname","A1Code","A2Code" )]
data <- merge(x=data, y=correct, by="pcode")

#data$govchk <- as.numeric(ifelse(data$A1NameEn == data$descript.governorate,0, 1))
#data$govchk <- as.numeric(ifelse(data[["A1NameEn"]] == data[["descript.governorate"]],0, 1))
#data$districtchk <- as.numeric(ifelse(data[["HRname"]] == data[["descript.district"]],0, 1))


##########################################################################################
### tentative sectorisation/redistricting of IDPS area -- cf infra vornoi.R

area <- readShapePoly('~/unhcr_r_project/cccm-assessment/out/area2.shp', proj4string=CRS("+proj=longlat"))

#gplot(area)
#plot(datasp)

datasparea <- IntersectPtWithPoly(datasp, area)

datasparea1 <- datasparea@data[ ,c("name","pcode")]
data <- merge(x=data, y=datasparea1, by="pcode")

#plot(datasparea)





####################################################################################
# Create a summary column to facilitate revision in phase 2


data$pcoded <- data$Summary <- paste(
  data$A1Code,
  data$name,   data$pcode, 
  sep='-') 



pcode <- as.data.frame(data[ , c("pcoded","scoreclass", "A1NameEn","HRname", "name", "site","descript.individual","descript.population.household", "sitear","neighbourhood", "descript._coordinates_longitude", "descript._coordinates_latitude")])

pcode <- pcode[order(pcode$A1NameEn, pcode$HRname, pcode$site, pcode$neighbourhood,pcode$sitear),]

pcode <-rename(pcode, c( "scoreclass"="criticallity" , "descript._coordinates_longitude"="longitude",
                               "descript._coordinates_latitude"="latitude",                 
                               "descript.individual"="individual", "descript.population.household"= "household",
                               "name"="OpeArea", "A1NameEn"="Governorate","HRname"="District"
                               ))



#########################################################
## Define profile of OpeArea
areasp <- aggregate(cbind(individual ) ~ name, data = datasparea@data, FUN = sum, na.rm = TRUE)
#View(areasp)

pcode.melt <- melt(pcode, id=c(5,2), measure=c(7,8))
pcode.cast <- dcast(pcode.melt,  OpeArea ~ variable+criticallity, sum)

pcode.cast <-rename(pcode.cast, c( "individual_1.Extreme"="extremeI",  "individual_2.High"= "highI","individual_3.Medium"="mediumI",
                         "individual_4.Low"="lowI",     "individual_5.None"="noneI",    "household_1.Extreme"="extremeH", 
                          "household_2.High"="highH" ,    "household_3.Medium"="mediumH",   "household_4.Low"="lowH",
                         "household_5.None"="noneH"))

pcode.sum <- aggregate(individual ~ OpeArea, data=pcode,  FUN= sum)
pcode.cast <- merge(x=pcode.sum, y=pcode.cast, by="OpeArea")  

rm(pcode.count)
pcode.count <- ddply(pcode, c("OpeArea"), summarise, count = length(criticallity) )
pcode.count <- count(pcode, c("OpeArea", "criticallity"))
pcode.count <- dcast(pcode.count,  OpeArea ~ criticallity, sum)
pcode.count1 <- count(pcode, c("OpeArea"))
pcode.count <- merge(x=pcode.count1, y=pcode.count, by="OpeArea")  
  #psum( pcode.count$1.Extreme,  pcode.count$2.High,	pcode.count$3.Medium,	pcode.count$4.Low, pcode.count$5.None, na.rm=na.rm)


#write.table(pcode, file='out/pcode.csv', sep=';', col.names = T, row.names = F)
write.csv(pcode, "out/pcode.csv", row.names=FALSE, na="")


pcode.cast$classextreme <- as.factor(findCols(classIntervals(pcode.cast$extremeI, n=6, style="fixed",fixedBreaks=c(0, 250, 500, 1000, 2000, 4000, 100000))))
#pcode.cast$classextreme <- -revalue(pcode.cast$classextreme, c("1"="a. 0-250", "2"="b. 250-500", "3"="c. 500-1000", "4"="d. 1000-2000", "5"="e. 2000-4000", "6"="f. >4000"))

pcode.cast$classhigh <- as.factor(findCols(classIntervals(pcode.cast$highI, n=6, style="fixed",fixedBreaks=c(0, 250, 500, 1000, 2000, 4000, 100000))))
#pcode.cast$classhigh <- -revalue(pcode.cast$classhigh, c("1"="a. 0-250", "2"="b. 250-500", "3"="c. 500-1000", "4"="d. 1000-2000", "5"="e. 2000-4000", "6"="f. >4000"))

pcode.cast$classmedium <- as.factor(findCols(classIntervals(pcode.cast$mediumI, n=6, style="fixed",fixedBreaks=c(0, 250, 500, 1000, 2000, 4000, 100000))))
#pcode.cast$classmedium <- -revalue(pcode.cast$classmedium, c("1"="a. 0-250", "2"="b. 250-500", "3"="c. 500-1000", "4"="d. 1000-2000", "5"="e. 2000-4000", "6"="f. >4000"))

pcode.cast$classlow <- as.factor(findCols(classIntervals(pcode.cast$lowI, n=6, style="fixed",fixedBreaks=c(0, 250, 500, 1000, 2000, 4000, 100000))))
#pcode.cast$classlow <- -revalue(pcode.cast$classlow, c("1"="a. 0-250", "2"="b. 250-500", "3"="c. 500-1000", "4"="d. 1000-2000", "5"="e. 2000-4000", "6"="f. >4000"))

pcode.cast$classnone <- as.factor(findCols(classIntervals(pcode.cast$noneI, n=6, style="fixed",fixedBreaks=c(0, 250, 500, 1000, 2000, 4000, 100000))))
#pcode.cast$classnone <- -revalue(pcode.cast$classnone, c("1"="a. 0-250", "2"="b. 250-500", "3"="c. 500-1000", "4"="d. 1000-2000", "5"="e. 2000-4000", "6"="f. >4000"))

names(pcode.cast)

pcode.cast <-merge (x=pcode.cast, y=pcode.count, by="OpeArea")  

write.csv(pcode.cast, "out/area.csv", row.names=FALSE, na="")

areadata <-merge(x=area, y=areasp, by="name")
areadata <-merge(x=areadata, y=pcode.cast, by.x="name", by.y="OpeArea")


writeOGR(areadata,"out","areadata",driver="ESRI Shapefile", overwrite_layer=TRUE)





##################################################################
####### Create export for dataviz  ###############################
##################################################################
rm(dataviz)
dataviz <- data
names(dataviz)
#dataviz <- dataviz[,-(1:10,12,16,17,29:37,46:50),drop=FALSE]

## select the column of interest for the dataviz
dataviz <-dataviz[ , c("pcode", "name", "sitear", "neighbourhood", "descript.organisat" ,"descript.governorate", "descript.district" ,
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

rm(areasp)
rm(coords)
   rm(correct)
#      rm(data)
         rm(datasparea1)
            rm(dataviz1)
               
rm(area)
# rm(areadata)
rm(datasp)
   rm(datasp1)
  rm(datasparea)
 rm(district)
