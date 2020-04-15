#Author: Nicholas Lovera
#Date: 4/13/2020
#Version: 1


#Reading in dataset 
#eleminating the wanting colums
> setwd("~/solar R Project")
> solarInfo = read.csv ('Solar.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
> View(solarInfo)
> install.packages('dplyr')
> install.packages('ggplot2')
> library(dplyr)
> library(ggplot2)
> solarInfo=solarInfo [, -c(1,4,5,6,8,9,13,15,16,27,28,30)]

# giving it an index
> n <- dim(solarInfo)[1]
solarInfo$Index <- c(1:n)

#Anuual KWH is our most important data
#so lets look at it first

#Changing Expected.KWh to numeric becuase of thousand seperator
solarInfo$Expected.KWh.Annual.Production <- as.numeric(solarInfo$Expected.KWh.Annual.Production)

#Histogram of Expected.Kwh
hist(solarInfo$Expected.KWh.Annual.Production, main = 'Project Cost', 
     xlab = 'Price')

#There are some low values but it does seem reasonable
#if say it's someones vactation home so no missleading
summary(solarInfo$Expected.KWh.Annual.Production)


#Adding Z-Values for kWh.annual
solarInfo$kWh.Annual.z <- scale(x=solarInfo$Expected.KWh.Annual.Production)
#Printing outliers (Z-values > 3 or < -3)
print(solarInfo$kWh.Annual.z[which(solarInfo$kWh.Annual.z  > 3 |
                                     solarInfo$kWh.Annual.z < -3)])
#No outliers in kwh.annual it is clean

#Next most important data is Sector
#By using summary we can easily tell that Sector is clean
summary(solarInfo$Sector)

library(ggplot2)

#Binning kwh.Annual
solarInfo$kwh_annual_binned <- cut(x=solarInfo$Expected.KWh.Annual.Production,
      breaks=c(0,1000,3000,5000),right=FALSE, 
      labels=c("Under 1000","1000 to 3000","Over 3000"))

#Plotting binned KWh.annual with Sector overlay
ggplot(solarInfo, aes(kwh_annual_binned)) + geom_bar(aes(fill=Sector), position="fill") + coord_flip()

#Contigency table of kwh_annual_binned and sector
t2.v1 <- table(solarInfo$Sector,solarInfo$kwh_annual_binned)
t2.v2 <- addmargins(A=t2.v1,FUN=list(total=sum),quiet=TRUE) 
t2.rnd <- round(prop.table(t2.v1,margin=2)*100,1)

write.csv(solarInfo, "solarInfoAdJ.csv")
