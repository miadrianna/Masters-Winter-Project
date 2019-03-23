
library(dplyr)
Proportion<- read.csv("Ao_Proportion_Dead.csv", header = TRUE)

#Here we are creating an svg file because the images are of god quality and showing the number of live and dead individuals from 2010-2018
svg("Number Dead.svg", width=11,height=7) 
Death<- select(Proportion, Live, Dead)
Death<- as.matrix(Death)
coll<- c("black", "white")
par(xpd=FALSE,mar=c(5.1,4.1,1,1),cex = 1.8)
barplot(t(Death), names.arg= c("2010", "2011", "2012", "2013", "2014", "2015", "2017", "2018"),beside= TRUE, ylim = c(0,600), xlab = "Year", ylab = "No.individuals", col = coll, legend.text = c("Live", "Dead"))

dev.off()
