
#-----------------------------------------------
#Jiayu Yan
#-----------------Modeling Phase----------------
#install nnetand NeuralNetToolspackages stats and c50
install.packages("nnet")
install.packages("NeuralNetTools")
install.packages("stats")
install.packages("C50")
library(nnet)
library(NeuralNetTools)
library(stats)
library(C50)

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


#-------------------------------------------------------------------------------
#ModelEvaluation
#-------------------------------------------------------------------------------
#read in the training and test data sets from setup phase
summary(solar_setup_train)
summary(solar_setup_test)
solar_setup_train$Sector<-factor(solar_setup_train$Sector) 
solar_setup_test$Sector<-factor(solar_setup_test$Sector) 
solar_setup_train$Project.Cost<-as.factor(solar_setup_train$Project.Cost)
solar_setup_test$Project.Cost<-as.factor(solar_setup_test$Project.Cost)
C5 <-C5.0(formula = Sector ~ Expected.KWh.Annual.Production+Project.Cost+X.Incentive, data = solar_setup_train) 
#build the test data set of only predictor variables
test.X<-subset(x=solar_setup_test,select= c("Expected.KWh.Annual.Production","Project.Cost","X.Incentive")) 
#run the model on test.X
ypred<-predict(object=C5,newdata=test.X)
#build the confusion matrix (contingency table)
t1 <-table(solar_setup_test$Sector,ypred)
row.names(t1) <-c("Actual: 0:","Actual: 1") 
colnames(t1) <-c("Predicted: 0","Predicted: 1") 
t1 <-addmargins(A = t1, FUN=list(Total = sum), quiet = TRUE) 
t1
#compute the evaluation measures
accuracy <-(t1[1,1] + t1[2,2])/t1[3,3] 
error_rate<-1-accuracy 
sensitivity <-t1[2,2]/t1[2,3]
specifity<-t1[1,1]/t1[1,3] 
precision <-t1[2,2]/t1[3,2] 
recall <-sensitivity 
f1 <-2*(precision * recall)/(precision + recall) 
f2 <-5*(precision * recall)/(4*precision+recall)
f0_5 <-1.25*(precision*recall)/(0.25*precision+recall)
# accuracy
#[1] 0.9700678
#accuracy
#[1] 0.9700678
# error_rate
#[1] 0.02993224
# sensitivity
#[1] 0.9974919
# specifity
#[1] 0.5581098
# precision
#[1] 0.9713541
# recall
#[1] 0.9974919
