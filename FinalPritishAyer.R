
#Pritish AYer
#R11597423

install.packages('dplyr')
install.packages('ggplot2')
library(dplyr)
library(ggplot2)

#Reading in dataset 
#eleminating the wanting colums
solarInfo = read.csv ('Solar.csv')
View(solarInfo)
solarInfo=solarInfo [, -c(1,4,5,6,8,9,13,15,16,27,28,30)]

#Giving solarInfo an index
n <- dim(solarInfo)[1]
solarInfo$Index <- c(1:n)

#Anuual KWH is our most important data
#so lets look at it first

#Changing Expected.KWh to numeric becuase of thousand seperator
solarInfo$Expected.KWh.Annual.Production <- as.numeric(solarInfo$Expected.KWh.Annual.Production)

#Printing the summary data and making the histogram
summary (solarInfo$Expected.KWh.Annual.Production)

var(solarInfo$Expected.KWh.Annual.Production)
sd(solarInfo$Expected.KWh.Annual.Production)

#Histogram of Expected.Kwh
hist(solarInfo$Expected.KWh.Annual.Production, main = 'Expected Annual KWh', 
     xlab = 'Projects')


#Adding Z-Values for kWh.annual
solarInfo$kWh.Annual.z <- scale(x=solarInfo$Expected.KWh.Annual.Production)
#Printing outliers (Z-values > 3 or < -3)
print(solarInfo$kWh.Annual.z[which(solarInfo$kWh.Annual.z  > 3 |
                                     solarInfo$kWh.Annual.z < -3)])
#No outliers in kwh.annual it is clean

#Next most important data is Sector
#By using summary we can easily tell that Sector is clean
summary(solarInfo$Sector)


#Binning kwh.Annual
solarInfo$kwh_annual_binned <- cut(x=solarInfo$Expected.KWh.Annual.Production,
                                   breaks=c(0,10000,100000,100000000),right=FALSE, 
                                   labels=c("Under 10000","10000 to 100000","Over 100000"))

#Plotting binned KWh.annual with Sector overlay
ggplot(solarInfo, aes(kwh_annual_binned)) + geom_bar(aes(fill=Sector), position="fill") + coord_flip()

#Contigency table of kwh_annual_binned and sector
t2.v1 <- table(solarInfo$Sector,solarInfo$kwh_annual_binned)
t2.v2 <- addmargins(A=t2.v1,FUN=list(total=sum),quiet=TRUE)
t2.v2
t2.rnd <- round(prop.table(t2.v1,margin=2)*100,1)
t2.rnd

#Saving data set with new info
write.csv(solarInfo, "solarInfoNew.csv", row.names= FALSE)
View(solarInfoNew)

#-----------------------------------------------------------------------
#Exploratory
#-----------------------------------------------------------------------
solarInfo <- read.csv("solarInfoNew.csv")
View(solarInfo)
set.seed(8);
n <-dim(solarInfo)[1] 
#runif() randomly draws numbers between 0 and 1, 
#each with equal probability 
#n generates n numbers 
#about 75% will be TRUE 
train_ind<-runif(n) < 0.75 
solar_train<-solarInfo[train_ind,] 
solar_test<-solarInfo[!train_ind,] 
#train set and test set for future refernece and model uses.

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

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#Setup Phase
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

#creating partitions

solar_setup <- read.csv("solarInfoAdj2.csv")
View(solar_setup)
set.seed(7);
n <- dim(solar_setup)[1]
#runif() randomly draws numbers between 0 and 1,
#each with equal probability
#n generates n numbers
#about 75% will be TRUE
train_ind <- runif(n) < 0.75
solar_setup_train <- solar_setup[train_ind,]
solar_setup_test <- solar_setup[!train_ind,]
solar_setup_train <- as.data.frame(solar_setup_train)
solar_setup_test <- as.data.frame(solar_setup_test)
summary (solar_setup_train)
summary (solar_setup_test)
solar_setup_train <- na.omit(solar_setup_train)
solar_setup_test <- na.omit(solar_setup_test)

is.recursive(solar_setup)
is.atomic(solar_setup)

#Validating the partitions
#two-sample t-test
t.test(solar_setup_train$Expected.KWh.Annual.Production,solar_setup_test$Expected.KWh.Annual.Production)
#two-sample z-test
p1<-sum(solar_setup_train$Sector=="Non-Residential")/dim(solar_setup_train)[1]
p2<-sum(solar_setup_test$Sector=="Non-Residential")/dim(solar_setup_test)[1]
p_pooled<-(sum(solar_setup_train$Sector=="Non-Residential")+sum(solar_setup_test$Sector=="Non-Residential"))/(dim(solar_setup_train)[1]+dim(solar_setup_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) *(1/dim(solar_setup_train)[1]+1/dim(solar_setup_test)[1]))



# Balancing the Data in R
#balancing the data - increase "Non-Residential" to 30%
table(solar_setup_train$Sector)
x<-(.3*76461-4934)/.7
to.resample<-which(solar_setup_train$Sector=="Non-Residential")
to.resample
our.resample<-sample(x=to.resample,size=25720,replace=TRUE)
our.resample.records<-solar_setup_train[our.resample,]
train_solar_setup_rebal<-rbind(solar_setup_train,our.resample.records)
table(train_solar_setup_rebal$Sector)
t.v1<-table(train_solar_setup_rebal$Sector)
t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Sector==Residential","Sector==Non-Residential")
rownames(t.v2) <- c("count","proportion")
t.v1
t.v2


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#MOdeling
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#Multiple Regression Modeling in R 

solar_setup_train$Sector<-as.factor(solar_setup_train$Sector)
solar_setup_train$Project.Cost<-as.factor(solar_setup_train$Project.Cost)
solar_setup_test$Sector<-as.factor(solar_setup_train$Sector)
solar_setup_test$Project.Cost<-as.factor(solar_setup_train$Project.Cost)
summary(solar_setup_train)
summary(solar_setup_test)
#build the regression model on the training set 
model01 <-lm(formula = Expected.KWh.Annual.Production~ Total.Inverter.Quantity + Sector + Project.Cost, data = solar_setup_train) 
summary(model01)
#build the regression model on the test set
model01_test <-lm(formula = Expected.KWh.Annual.Production~ Total.Inverter.Quantity + Sector + Project.Cost, data = solar_setup_test)
summary(model01_test)

#Perform Estimation and Model Evaluation
#create one customer
cust01 <-data.frame(Sector = as.factor(solar_setup_train$Sector), Total.Inverter.Quantity = 10, Project.Cost = as.factor(solar_setup_train$Project.Cost))
predict(object = model01, newdata= cust01) 
#View Residual Standard Error, s, through summary(model02_test) and summary(model02)
summary(model01)
summary(model01_test)
#using the test data set
X_test<-data.frame(Total.Inverter.Quantity = solar_setup_test$Total.Inverter.Quantity, Sector = solar_setup_test$Sector, Project.Cost = solar_setup_test$Project.Cost)

ypred_02 <-predict(object = model01, newdata= X_test)
ytrue<-solar_setup_test$Expected.KWh.Annual.Production
install.packages("MLmetrics")
library(MLmetrics) 
#Mean Absolute Error
MAE(y_pred= ypred_02, y_true= ytrue)


#stepwise regression
install.packages("Mass")
library(MASS)
model01_step <-stepAIC(object=model01)
summary(model01_step)
