
## Plot per month of Arrival
rm(data.accomodation)

data.accomodation <- melt(data, id=c(7,8,11,13,14), measure=c(29:37))
data.accomodation.sum <- dcast(data.accomodation,  
                               descript.governorate+descript.organisat +
                                 descript._coordinates_latitude + 
                                 descript._coordinates_longitude ~ variable, count)

data.accomodation.sum <- dcast(data.accomodation,  
                               descript.governorate ~ variable, count)

#master.month <- master.month[order(-master.month$Month.Displacement),]

rm(plotmonth1)
plotmonth1 <- ggplot(data=master.month, aes(x=Month.Displacement , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Displacement Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per month of reported displacement")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

# Save this!
ggsave("~/unhcr_r_project/cccm-assessment/plot/test.png", plotmonth1, width=8, height=6,units="in", dpi=300)
