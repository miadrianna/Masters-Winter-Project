library(dplyr)
library(lubridate)

#Here are all the cleaned up datasets 
Arl2_07_15<- read.csv("Arlet_07_15.csv", stringsAsFactors = FALSE)
Arl2_07_15<- select(Arl2_07_15, Date, Temp)
Arl2_15_16<- read.csv("Arl2_15_16.csv", stringsAsFactors = FALSE)
Arl2_16_17<- read.csv("Arl2_16_17.csv", stringsAsFactors = FALSE)
Arl2_17_18<- read.csv("Arl2_17_18.csv", stringsAsFactors = FALSE) 

#First it is easier to work with one dataset rather than multiple so we are ging to combine all the datasets together 
Arl2_07_15<- Arl2_07_15[!is.na(Arl2_07_15$Temp),]

Combined_Data <- Arl2_15_16[!is.na(Arl2_15_16$Temp),] %>% select(., Date, Temp) %>% rbind(.,Arl2_07_15)
Combined_Data <- Arl2_16_17[!is.na(Arl2_16_17$Temp),] %>% select(., Date, Temp) %>% rbind(.,Combined_Data)
Combined_Data<- Arl2_17_18[!is.na(Arl2_17_18$Temp), ] %>% select(., Date, Temp) %>%rbind(., Combined_Data)

#Now we are going to get our unique dates and then get the average temperatures and variance of the days so that we have one record per day 
UniD<- unique(Combined_Data$Date)
Temp<- lapply(UniD, function(x) mean(filter(Combined_Data, Date == x) %>% select(Temp) %>% unlist()))
Var<- lapply(UniD, function(x) var(filter(Combined_Data, Date == x) %>% select(Temp) %>% unlist()))
Temp_df<- data.frame("Date" = UniD, "Avg_Temp"= unlist(Temp), "Variance" = unlist(Var))

#Because its easier to read on a graph we are going to take the monthly temperature averages
Temp_df$Date = dmy(Temp_df$Date)
df<- Temp_df %>% group_by(Year=year(Date), Month=month(Date)) %>% summarise(mean=mean(Avg_Temp))
######################################################################################################################################

new_df <- data.frame(Temp_df$Avg_Temp,Temp_df$Variance,t(as.data.frame(strsplit(as.character(Temp_df$Date),"/"))))
colnames(new_df) <- c("Avg_Temp","Varience","Day","Month","Year")
new_df <- new_df[order(new_df$Year,new_df$Month,new_df$Day),]

new_df$Date <- paste(new_df$Day,new_df$Month,sep = "/")
days <- data.frame("day"=new_df[new_df$Year==2008,"Date"],"values"=NA)
Col2<- data.frame("colour"= as.character(grey.colors(12)), "Year" = c("2007","2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
Year_<- new_df[grep("2008", new_df$Year), ]

plot(1: nrow(Year_), Year_$Avg_Temp, type = "o", col= Col2[Col2$Year == "2008", 1], xlim = c(110,200), cex = 0.1, xaxt= "n", ylab = "Monthly Average Temperature", xlab = "Month")
for(x in c("2007","2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017","2018")){
  Year_ <- new_df[grep(x, new_df$Year),]
  tmp <- days
  for(t in tmp$day){
    if(any(Year_$Date == t)){
      tmp[tmp$day == t,"values"] <- Year_[Year_$Date == t, "Avg_Temp"]
    }
  }
  lines(tmp$values, type = "o", col = Col2[Col2$Year == x,1], cex = 0.4)
}
legend(x = 1, y = 18,legend = Col2$Year, col = Col2$colour, lty=1:12, cex=0.9)


#####################################################################################################################################################################################
#This is the colour palette that would be used to colour the liines for the line plots 
colours <- c("darkgreen","gold3","darkturquoise","deeppink","firebrick4","red","snow4","royalblue4", "orange2","purple", "green","navy","slategray2")

#Now we are going to start plotting the graph
#par is there for the purpose of putting temperature data for all four lakes side by side so we can have 4 graphs together
svg("Arl.Temp.svg", width=14,height=9)
par(xpd=TRUE,mar=c(5.1,4.1,1.3,5.5), mfrow=c(2,2), cex = 1.3)
correct_order <- data.frame("month"=c(1,2,3,4,5,6,7,8,9,10,11,12),"values"=NA)
Col2<- data.frame("colour"= as.character(colours[1:12]), "Year" = c("2007","2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), stringsAsFactors = FALSE)
Year_<- df[grep("2017", df$Year), ]
plot(1: nrow(Year_), Year_$mean, type = "o", col= Col2[Col2$Year == "2017", 1], lwd= 2, cex = 0.1, xaxt= "n", ylab = "Monthly Average Temperature", xlab = "Month", ylim = c(0,25))
for(x in c("2007","2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2018")){
  Year_ <- df[grep(x, df$Year),]
  tmp <- correct_order
  for(t in tmp$month){
    if(any(grepl(paste0("\\b",t,"\\b"), Year_$Month))){
      tmp[tmp$month == t,"values"] <- Year_[grep(paste0("\\b",t,"\\b"), Year_$Month), "mean"]
    }
  }
  lines(tmp$values, type = "o", col = Col2[Col2$Year == x,1], lwd=1.8, cex = 0.1)
}
legend(12.8,26,legend = Col2$Year, col = Col2$colour[1:12], lty=1, lwd = 2, cex = 0.8)
axis(side=1, at=(seq(1,12,1)), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
text(1,27,"(a)",cex=0.9)
#dev.off()
###############################################################################################################################################################

#This code was used to identify any warming trends using a linear model 
AVG<- read.csv("Arlet_Yearly_AVg.csv", header = TRUE)
mod<- lm(AVG$Mean.Temp~AVG$Year)
summary(mod)

