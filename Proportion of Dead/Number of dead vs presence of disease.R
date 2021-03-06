
Proportion<- read.csv("Ao_Proportion_Dead.csv", header = TRUE)
Present<- read.csv("Present.csv", header = TRUE, row.names = 1)

#Files are read in and then the number of individuals with disease is associated with a year 
Present<- Present[c("2010", "2011", "2012", "2013", "2014", "2015", "2017", "2018"), , drop= TRUE ]
Present
names(Present)<- c("2010","2011", "2012", "2013", "2014", "2015", "2017", "2018")
Present

#This is the protion of live and dead individuals 
P<- cbind(Proportion$Live, Proportion$Dead)

#Here we run a quasi binomial glm because the model is over dispersed and because we are using proportions
Death_<- glm(P~Present, family = quasibinomial)
summary(Death_)

#To get the pseudo R squared
(Death_$null.deviance - Death_$deviance) / Death_$null.deviance

#To get the chi squared
1-pchisq(Death_$deviance, Death_$df.residual)

#To compare model against the null
pchisq(Death_$null.deviance-Death_$deviance, Death_$df.null-Death_$df.residual)

#To check for dispersion
Death_$deviance/ Death_$df.residual

###################################################################################################################################################

Proportion<- read.csv("Ao_Proportion_Dead_Lag.csv", header = TRUE, row.names = 1)
Lag<- read.csv("Present_Lag.csv", header = TRUE, row.names = 1)

#Files are read in and then the number of individuals with disease is associated with the corresponding year
#In this case we are seeing if the number infected in the previous year are having an effect 
Lag<- Lag[c("2011", "2012", "2013", "2014", "2015", "2017", "2018"), , drop= TRUE ]
Lag
names(Lag)<- c("2011", "2012", "2013", "2014", "2015", "2017", "2018")
Lag

#This is the proportion of live and dead individuals 
P<- cbind(Proportion$Live, Proportion$Dead)

#Here we run a quasi binomial glm because the model is over dispersed and because we are using proportions
Dead_<- glm(P~Lag, family = quasibinomial)
summary(Death_)

#To get the pseudo R squared
(Proportion_$null.deviance - Proportion_$deviance) / Proportion_$null.deviance

#To get the chi squared
1-pchisq(Proportion_$deviance, Proportion_$df.residual)

#To compare model against the null
pchisq(Proportion_$null.deviance-Proportion_$deviance, Proportion_$df.null-Proportion_$df.residual)

#To check for dispersion
Proportion_$deviance/ Proportion_$df.residual

