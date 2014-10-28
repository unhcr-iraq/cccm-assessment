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



data$site <- paste(data$neighbourhood, data$sitear, data$namekey,  data$descript.phonekey, sep=" / ")



data$pcode <- row.names(data)


data$Summary <- paste(
  data$neighbourhood,
  data$A1NameEn, data$HRname,
  data$descript.population.household, data$descript.population.men, data$descript.population.women,
  data$descript.population.boys, data$descript.population.girls, data$descript.individual,
  data$descript.environment,
  data$accom,                     
  sep='\n') 