source("~/unhcr_r_project/cccm-assessment/packages.R")

#########################################################
### Printout of the list
rm(printTable)
printTable <- pcode.cast[,-(8:21),drop=FALSE]
rownames(printTable) <- NULL
listarea <- qplot(1:10, 1:10, geom = "blank") +
  theme_tufte(base_family="Helvetica")+ theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(printTable, show.colnames = TRUE, gpar.coltext = gpar(cex = 1.2), gpar.rowtext = gpar(cex = 1.2)),
                    xmin = -Inf, xmax = Inf,  ymin = -Inf, ymax = Inf)
ggsave("map/listarea.png", listarea, width=9, height=7,units="in", dpi=300, bg = "transparent")

rm(printTable2)
printTable2 <- pcode.count
rownames(printTable2) <- NULL
listarea2 <- qplot(1:10, 1:10, geom = "blank") +
  theme_tufte(base_family="Helvetica")+ theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(printTable2, show.colnames = TRUE, gpar.coltext = gpar(cex = 1.2), gpar.rowtext = gpar(cex = 1.2)),
                    xmin = -Inf, xmax = Inf,  ymin = -Inf, ymax = Inf)
ggsave("map/listarea2.png", listarea2, width=9, height=7,units="in", dpi=300, bg = "transparent")




##################################################################################
# Fortify them
areadata@data$id = rownames(areadata@data)
centroids.areadata <- as.data.frame(coordinates(areadata))
names(centroids.areadata) <- c("Longitude", "Latitude")
areadata@data <- merge (x=centroids.areadata, y=areadata@data, by="row.names")

areadata_f <- fortify(areadata, region="id")
areadata_f <-join(areadata_f, areadata@data, by="id")

## add centroid for label



rm(mapareadata)
mapareadata <-  ggplot(areadata_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data = areadata_f, aes(x = long, y = lat, group = group), alpha = 0.5) +
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Operational Area")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
ggsave("map/mapareadata.png", mapareadata, width=8, height=6,units="in", dpi=300)

rm(mapareadatae)
mapareadatae <-  ggplot(areadata_f, aes(x = long, y = lat, group = group, fill=classextreme)) + 
  geom_polygon(data = areadata_f, aes(fill=classextreme), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Extreme") +   
  coord_equal()+
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="grey")+
  ggtitle("Criticallity Ranking - Extreme- OpArea ranked by number of Ind.")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
ggsave("map/mapareadatae.png", mapareadatae, width=8, height=6,units="in", dpi=300, bg = "transparent")

rm(mapareadatah)
mapareadatah <-  ggplot(areadata_f, aes(x = long, y = lat, group = group, fill=classhigh)) + 
  geom_polygon(data = areadata_f, aes(fill=classhigh), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Extreme") +   
  coord_equal()+
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="grey")+
  ggtitle("Criticallity Ranking - High- OpArea ranked by number of Ind.")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
ggsave("map/mapareadatah.png", mapareadatah, width=8, height=6,units="in", dpi=300, bg = "transparent")

rm(mapareadatam)
mapareadatam <-  ggplot(areadata_f, aes(x = long, y = lat, group = group, fill=classmedium)) + 
  geom_polygon(data = areadata_f, aes(fill=classmedium), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Extreme") +   
  coord_equal()+
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="grey")+
  ggtitle("Criticallity Ranking - Medium- OpArea ranked by number of Ind.")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
ggsave("map/mapareadatam.png", mapareadatam, width=8, height=6,units="in", dpi=300, bg = "transparent")

rm(mapareadatan)
mapareadatan <-  ggplot(areadata_f, aes(x = long, y = lat, group = group, fill=classnone)) + 
  geom_polygon(data = areadata_f, aes(fill=classnone), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Extreme") +   
  coord_equal()+
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="grey")+
  ggtitle("Criticallity Ranking - None - OpArea ranked by number of Ind.")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
ggsave("map/mapareadatan.png", mapareadatan, width=8, height=6,units="in", dpi=300, bg = "transparent")

rm(mapareadatal)
mapareadatal <-  ggplot(areadata_f, aes(x = long, y = lat, group = group, fill=classlow)) + 
  geom_polygon(data = areadata_f, aes(fill=classlow), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Extreme") +   
  coord_equal()+
  # geom_text(data = areadata_f, aes(label = name, x = Longitude, y = Latitude, group = group) ) + #add labels at centroids
  geom_path(data = areadata_f, aes(x = long, y = lat, group = group), color="grey")+
  ggtitle("Criticallity Ranking Low- OpArea ranked by number of Ind.")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")
mapareadatal <-mapareadatal + annotate("text", x = 39, y = 37.5,  size = 3, label = "Operational Areas defined through population cluster of ~ 5000 IDPs", hjust = 0)

ggsave("map/mapareadatal.png", mapareadatal, width=8, height=6,units="in", dpi=300, bg = "transparent")


