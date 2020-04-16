#Jiayu Yan
solarInfo <- read.csv("solarInfoAdj.csv")

set.seed(8);
n <-dim(solar)[1] 
#runif() randomly draws numbers between 0 and 1, 
#each with equal probability 
#n generates n numbers 
#about 75% will be TRUE 
train_ind<-runif(n) < 0.75 
solar_train<-solar[train_ind,] 
solar_test<-solar[!train_ind,] 

#Our predictors are Project Cost and incentive, target variable is Expected KWh Annual Production.
#Rewrite as numeric
solarInfo$Project.Cost <- as.numeric(solarInfo$Project.Cost)
solarInfo$Expected.KWh.Annual.Production<- as.numeric(solarInfo$Expected.KWh.Annual.Production)
solarInfo$X.Incentive<- as.numeric(solarInfo$X.Incentive)

#plot the graph for project cost and expected kwh annual production
plot(solarInfo$Project.Cost, solarInfo$Expected.KWh.Annual.Production)

#plot the graph for incentive and expected kwh annual production
plot(solarInfo$X.Incentive, solarInfo$Expected.KWh.Annual.Production)
#-----------------------------------------------------------------------
#As plot graph suggests, out target variable increases as the predictors increase.
#It means the solar energy output is directly proportional to the project cost.


#Binning Project.Cost
solarInfo$Project.Cost <- cut(x=solarInfo$Project.Cost,
                              breaks=c(0,2000000,4000000,6000000),right=FALSE, 
                              labels=c("Under 2000000","4000000 to 6000000","Over 6000000"))

#Binning Incentive
solarInfo$X.Incentive <- cut(x=solarInfo$X.Incentive,
                             breaks=c(0,400000,600000,1000000),right=FALSE, 
                             labels=c("Under 400000","600000 to 1000000","Over 1000000"))

write.csv(solarInfo, "solarInfoAdj2.csv")
