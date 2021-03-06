library(dplyr)
library(lubridate)

#This code was used to identify any warming trends using a linear model 
AVG<- read.csv("../Ansabere First Plot/Yearly_Avg_Temp07-17.csv", header = TRUE)
mod<- lm(AVG$Avg_Temp~AVG$Year)
summary(mod)

#Here are all the cleaned up datasets 
Ans2_13_14<- read.csv("../Ansabere First Plot/Ans2_13_14.csv", stringsAsFactors = FALSE) 
Ans2_14_15<- read.csv("../Ansabere First Plot/Ans2_14_15.csv", stringsAsFactors = FALSE)
Ans2_15_16<- read.csv("../Ansabere First Plot/Ans2_15_16.csv", stringsAsFactors = FALSE)
Ans2_16_17<- read.csv("../Ansabere First Plot/Ans2_16_17.csv", stringsAsFactors = FALSE)
Ans2_07_14<- read.csv("../Ansabere First Plot/Ansabere_07_14.csv", stringsAsFactors = FALSE)
Ans2_07_14<- select(Ans2_07_14, Date, Temp)

#Here I have removed the NAs from the datasets and then combined all the individual datasets together to make one master dataset
Ans2_07_14<- Ans2_07_14[!is.na(Ans2_07_14$Temp),]
Combined_Data <- Ans2_14_15[!is.na(Ans2_14_15$Temp),] %>% select(., Date, Temp) %>% rbind(.,Ans2_07_14)
Combined_Data <- Ans2_15_16[!is.na(Ans2_15_16$Temp),] %>% select(., Date, Temp) %>% rbind(.,Combined_Data)
Combined_Data<- Ans2_16_17[!is.na(Ans2_16_17$Temp), ] %>% select(., Date, Temp) %>%rbind(., Combined_Data)

#Now we are going to get our unique dates and then get the average temperatures and variance of the days so that we have one record per day 
UniD<- unique(Combined_Data$Date)
Temp<- lapply(UniD, function(x) mean(filter(Combined_Data, Date == x) %>% select(Temp) %>% unlist()))
Var<- lapply(UniD, function(x) mean(filter(Combined_Data, Date == x) %>% select(Temp) %>% unlist()))
Temp_df<- data.frame("Date"= UniD, "Avg_Temp"= unlist(Temp), "Variance" = unlist(Var))

#Because its easier to read on a graph we are going to take the monthly temperature averages maing sure there were no NAs
Temp_df$Date = dmy(Temp_df$Date)
df<- Temp_df %>% group_by(Year = year(Date), Month = month(Date)) %>% summarise(mean = mean(Avg_Temp))
df<- df[!is.na(df$Year), ]
df<- df[!is.na(df$Month), ]

##################################################################################################################################################################################

#This is the colour palette that would be used to colour the liines for the line plots 
colours <- c("darkgreen","gold3","darkturquoise","deeppink","firebrick4","red","snow4","royalblue4", "orange2","purple", "green","navy","slategray2")

#Now we are going to start plotting the graph and this will be added to the svg image which was started in the first file 'ArletData2'
#svg("Ans.Temp.svg", width=12,height=9)
correct_order <- data.frame("month"=c(1,2,3,4,5,6,7,8,9,10,11,12),"values"=NA)
Col2<- data.frame("colour"= as.character(colours[1:11]), "Year" = c("2007","2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), stringsAsFactors = FALSE)
Year_<- df[grep("2008", df$Year), ]
plot(1: nrow(Year_), Year_$mean, type = "o", col= Col2[Col2$Year == "2008", 1], cex = 0.1, xaxt= "n", ylab = "Monthly Average Temperature", xlab = "Month", ylim = c(0,25))
for(x in c("2007","2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")){
  Year_ <- df[grep(x, df$Year),]
  tmp <- correct_order
  for(t in tmp$month){
    if(any(grepl(paste0("\\b",t,"\\b"), Year_$Month))){
      tmp[tmp$month == t,"values"] <- Year_[grep(paste0("\\b",t,"\\b"), Year_$Month), "mean"]
    }
  }
  lines(tmp$values, type = "o", col = Col2[Col2$Year == x,1], cex = 0.1, lwd = 1.8)
}
legend(12.8,26,legend = Col2$Year, col = Col2$colour[1:12], lty=1, lwd = 2, cex = 0.8)
axis(side=1, at=(seq(1,12,1)), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
text(1,27,"(b)",cex=0.9)
#dev.off()
