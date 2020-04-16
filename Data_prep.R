#Author: Nicholas Lovera
#Date: 4/13/2020
#Version: 1


#Reading in dataset 
#eleminating the wanting colums

setwd("~/solar R Project")
solarInfo = read.csv ('Solar.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
View(solarInfo)
install.packages('dplyr')
install.packages('ggplot2')
library(dplyr)
library(ggplot2)
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
write.csv(solarInfo, "solarInfoNew", row.names= FALSE)
View(solarInfoNew)
