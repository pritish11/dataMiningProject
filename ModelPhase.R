
#-----------------------------------------------
#Jiayu Yan
#-----------------Modeling Phase----------------
#install nnetand NeuralNetToolspackages and open both 
install.packages("nnet")
install.packages("NeuralNetTools")
install.packages("stats")
library(nnet)
library(NeuralNetTools)
library(stats)

#read trainning set from setup pahse

summary(solar_setup_train)
#convert binary and ordinal variables cost and sector to factors 
solar_setup_train$Project.Cost<-as.factor(solar_setup_train$Project.Cost)
solar_setup_train$Sector<-as.factor(solar_setup_train$Sector)
#perform min-max standardization on the Expected annual production variable 
solar_setup_train$Expected.KWh.Annual.Production.mm <-((solar_setup_train$Expected.KWh.Annual.Productionmin((solar_setup_train$Expected.KWh.Annual.Production))/(max((solar_setup_train$Expected.KWh.Annual.Production) -min((solar_setup_train$Expected.KWh.Annual.Production))
summary(solar_setup_train)
#nnet our target is expected annual production based on cost and sector
#size=1 specifies 1 unit in the hidden layer 
nnet01 <-nnet(Sector~Expected.KWh.Annual.Production+Project.Cost, data=solar_setup_train, size=1) 
#plot the network
plotnet(nnet01)
#obtain the weights 
nnet01$wts
#-------------------------------------------------------------------------------
#weights for multiple runs
#[1] -0.5275817  0.2347443  0.6477832 -0.3090919  1.4280951  1.2459409
#[2]  0.3354390 -0.3483611 -0.6108872  0.3403708  2.6740346  0.2456710
#[3]  0.4528409 -0.1880313  0.5762697  0.5559369  2.6740351 -0.6808321
#[4]  0.4946884  0.2869887  0.2695025 -0.5656303  1.6683544  1.0056820
#-------------------------------------------------------------------------------
#Example:
#For run [2]
#first 4 weights are B1, I1, I2, I3
#B1 = 0.3354390  
#I1 = -0.3483611  
#I2 = -0.6108872 
#I3 = 0.3403708
#next 2 weights are B2H1
#B2 = 2.6740346
#H1 = 1.7888629
#analyze datas
#W(H101) = 1.7888629 
#When H1 outputs a high number, we can expect a high annual production
#W(I1H1) = -0.3483611  
#Non-residential are more likely to have a higher electricity usage, thus we can expect a high annual production
#W(I2H1) = -0.6108872  
#Higher project cost(over 6000000) will result in a high annual production
#W(I3H1) = 0.3403708
#lower project(under 2000000) cost will result in a low annual production
#W(B1H1)=0.3354390 and W(B2O1)=2.6740346
#result is favor our predictions. Residential sectos have low annual production, and higher project cost have high annual production.


#Clustering
summary(solar_setup_train)
#create a subset of the predictor variables
#convert to numeric
solar_setup_train$Project.Cost<-as.numeric(solar_setup_train$Project.Cost)
solar_setup_train$Sector<-as.numeric(solar_setup_train$Sector)
#Sector 1 means Non-Residential, 2 means Residential
X <-subset (solar_setup_train,select=c("Sector","Project.Cost")) 
#scale both predictor variables using the Z-score and make the predictor variables a data frame 
Xs<-as.data.frame(scale(X))
colnames(Xs) <-c("Sector_z","Project.Cost_z")
#if kmeanscommand is not found use
#create 2 clusters with kmeans
kmeans01 <-kmeans(Xs,centers=2) 
#extract the clusters for each record
cluster <-as.factor(kmeans01$cluster)
#separate clusters into Cluster1 and Cluster2 
Cluster1 <-Xs[which(cluster==1),] 
Cluster2 <-Xs[which(cluster==2),]
#descriptive statistics for each cluster
summary(Cluster1)
summary(Cluster2)

#validate the clusters with solar_setup_test
summary(solar_setup_test) 
#convert to numeric
solar_setup_test$Project.Cost<-as.numeric(solar_setup_test$Project.Cost)
solar_setup_test$Sector<-as.numeric(solar_setup_test$Sector)
#Sector 1 means Non-Residential, 2 means Residential
X_test<-subset(solar_setup_test,select=c("Sector","Project.Cost"))
Xs_test<-as.data.frame(scale(X_test))
colnames(Xs_test) <-c("Sector_z","Project.Cost_z") 
kmeans01_test <-kmeans(Xs_test,centers=2)
cluster_test<-as.factor(kmeans01_test$cluster) 
Cluster1_test <-Xs_test[which(cluster_test==1),] 
Cluster2_test <-Xs_test[which(cluster_test==2),] 
summary(Cluster1_test)
summary(Cluster2_test)
