
Date<- read.csv("Yearly_Temp_Avg.csv", header = TRUE, row.names = 1)
Proportion<- read.csv("Ao_Proportion_Dead.csv", header = TRUE, row.names = 1)

#This is the proportion of live and dead individuals 
P<- cbind(Proportion$Live, Proportion$Dead)

#Files are read in and then the number of individuals with disease is associated with the corresponding year 
Arl.date<- Date[c("2010", "2011", "2012", "2013", "2014", "2015", "2017", "2018"), , drop= TRUE ]
Arl.date
names(Arl.date)<- c("2010", "2011", "2012", "2013", "2014", "2015", "2017", "2018")
Arl.date

#Here we run a quasi binomial glm because the model is over dispersed and because we are using proportions
Proportion_<- glm(P~Arl.date, family = quasibinomial)
summary(Proportion_)

#To get the pseudo R squared
(Proportion_$null.deviance - Proportion_$deviance) / Proportion_$null.deviance

#To get the chi squared
1-pchisq(Proportion_$deviance, Proportion_$df.residual)

#To compare model against the null
pchisq(Proportion_$null.deviance-Proportion_$deviance, Proportion_$df.null-Proportion_$df.residual)

#To check for dispersion
Proportion_$deviance/ Proportion_$df.residual
