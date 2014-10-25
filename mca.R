#  clear graphics settings so that it works with multiple windows
#dev.off()
#options(device="x11")

#### Loading the required R module


library(dynGraph)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)
library(EnQuireR) ## This will launch RCmdr automatically...

library(ade4)

## select the column of interest for the dataviz
df <-data[ , c( "descript.governorate", "descript.district" ,        
                     #   "descript.population.household", "descript.population.men",
                     #   "descript.population.women" , "descript.population.boys" , "descript.population.girls",                 
                     #   "descript.individual" ,
                        "physicalcondition.conditions.water_access", "physicalcondition.conditions.water_quality",
                        "physicalcondition.conditions.electric",    "physicalcondition.conditions.openelement", 
                        "physicalcondition.conditions.damage",  "physicalcondition.conditions.hazards",
                        "physicalcondition.conditions.mines","physicalcondition.conditions.fighting", 
                        "physicalcondition.shelter.op" , "physicalcondition.shelter.imp" ,            
                         "physicalcondition.shelter.tent" ,"physicalcondition.shelter.nat",
                        "physicalcondition.shelter.mix", "physicalcondition.shelter.concr",
                        "physicalcondition.shelter.shop", "physicalcondition.shelter.big"  ,"physicalcondition.shelter.const"
)]

d <-data[ , c( # "class" ,
  "physicalcondition.conditions.water_access", "physicalcondition.conditions.water_quality",
  "physicalcondition.conditions.electric",    "physicalcondition.conditions.openelement", 
  "physicalcondition.conditions.damage",  "physicalcondition.conditions.hazards",
  "physicalcondition.conditions.mines","physicalcondition.conditions.fighting"#, 
  #  "physicalcondition.shelter.op" , "physicalcondition.shelter.imp" ,            
  #  "physicalcondition.shelter.tent" ,"physicalcondition.shelter.nat",
  #  "physicalcondition.shelter.mix", "physicalcondition.shelter.concr",
  #  "physicalcondition.shelter.shop", "physicalcondition.shelter.big"  ,"physicalcondition.shelter.const"
)]

str(d)

acm <- dudi.acm(d, scan = FALSE)

scatter(acm, col = rep(c("black", "red3", "darkblue"), 2))

summaryacm <- data.frame(
  EIG = acm$eig,
  PCTVAR = 100 * acm$eig / sum(acm$eig),
  CUMPCTVAR = cumsum(100 * acm$eig / sum(acm$eig))
)
barplot(summaryacm$PCTVAR,
        xlab = "Componants",
        ylab = "Pourcentage de la variance (inertie)",
        names = paste("C", seq(1, nrow(summaryacm), 1)),
        col = "black",
        border = "white")

plot(acm$li, pch = 20, col = "grey40")
abline(h=0, v=0)
#points(acm$co, type = "o", pch = 18, col = "black")
text(acm$co,
     labels = row.names(acm$co),
     cex = 0.8,
     pos = c(rep(4, times = 3), 1, rep(4, times = 4), 3))

contribacm <- inertia.dudi(acm, row.inertia = TRUE, col.inertia = TRUE)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 0))

barplot(acm$cr[, 1], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(d),
        las = 1, main = "First factor", col = "lightblue", xlab = "Correlation")
barplot(acm$cr[, 2], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(d),
        las = 1, main = "Second factor", col = "lightblue", xlab = "Correlation")

boxplot(dudi.acm(d, scan = FALSE))

##############################


chiDist <- dist.dudi(acm, amongrow = TRUE)
clustered <- ward.cluster(chiDist,
                       #peso = apply(d, 1, sum),
                       plots = TRUE, h.clust = 1)

d$cluster <- paste("Cluster", cutree(clustered, k = 5))



### testing with FactomineR

for (i in 1:dim(d)[2]) d[,i]<-paste(colnames(d)[i],d[,i], sep=".")
par(mfrow=c(3,1))
MCA(d)


summary(datamca)

datamca.mca<-MCA(d)
dimdesc(datamca.mca,axes=1:2)
dimdesc(datamca.mca,axes=1:2,proba=0.30)
dimdesc(datamca.mca,axes=1:2,proba=0.50)
datamca.hcpc<-HCPC(datamca.mca ,nb.clust=-1,consol=TRUE,min=3,max=6,graph=TRUE)
datamca.hcpc$desc.var
datamca.hcpc$desc.axes
datamca.hcpc$desc.ind



ind1.enmca<-ENMCA(ind1, report=FALSE)


ind3.semantic <- ENmarking(ind3,1)