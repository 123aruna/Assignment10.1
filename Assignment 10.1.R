install.packages("readr")
library(readr)
mydata<-read_csv("AirqualityUCI.zip") 
library(readr)
AirQualityUCI <- read_delim("AirQualityUCI.zip", ";", escape_double = FALSE, trim_ws = TRUE) 
View(AirQualityUCI)
#	Create Univariate for all the columns.
AirQualityUCI[AirQualityUCI==-200.0]<-NA
for(i in 1:ncol(AirQualityUCI))
{
  AirQualityUCI[is.na(AirQualityUCI[,i]),i] <- mean(AirQualityUCI[,i], na.rm = TRUE)
}
summary(AirQualityUCI) 
AirQualityUCI[7:14,]
hist(AirQualityUCI$`NOx(GT)`,col="red") 
dotchart(AirQualityUCI$PT08.S2(NMHC),labels = row.names(AirQualityUCI$PT08.S1(CO)),cex=0.5,color = "blue") pairs(AirQualityUCI[7:14]) Date Time CO(GT) PT08.S1(CO) NMHC(GT) C6H6(GT) PT08.S2(NMHC)
#Check for missing values in all columns.
colSums(is.na(AirQualityUCI))
#Pattern of missing values
library(mice)
md.pattern(AirQualityUCI)
summary(AirQualityUCI)
#create missing map
#missmap(AirQualityUCI, col=c("black", "grey"), legend=FALSE)
colSums(is.na(AirQualityUCI)) 
#Impute the missing values using appropriate methods
colSums(is.na(AirQualityUCI))
# Number of missing per column/variable #filling the missing values by NA
library(plyr)
AirQualityUCI[AirQualityUCI==-200.0]<-NA
#Replacing the NA by mean of each columns
for(i in 1:ncol(AirQualityUCI))
{
  AirQualityUCI[is.na(AirQualityUCI[,i]),i] <- mean(AirQualityUCI[,i], na.rm = TRUE)
} 
summary(AirQualityUCI)
#Create bi-variate analysis for all relationships.
summary(AirQualityUCI) 
plot(AirQualityUCI$`NOx(GT)`~AirQualityUCI$`PT08.S2(NMHC)`) 
plot(AirQualityUCI$`PT08.S1(CO)`~AirQualityUCI$`PT08.S3(NOx)`) 
plot(AirQualityUCI$`NO2(GT)`~AirQualityUCI$`PT08.S4(NO2)`) 
plot(AirQualityUCI$`PT08.S5(O3)`~AirQualityUCI$T)
#Test relevant hypothesis for valid relations
plot(AirQualityUCI$`PT08.S1(CO)`,AirQualityUCI$T) 
lm(formula=AirQualityUCI$`PT08.S3(NOx)`~AirQualityUCI$`NOx(GT)`)
lm(formula = AirQualityUCI$`PT08.S1(CO)`~AirQualityUCI$T) 
lm(formula = AirQualityUCI$`NMHC(GT)`~AirQualityUCI$`PT08.S2(NMHC)`) 
plot(AirQualityUCI$`PT08.S5(O3)`,AirQualityUCI$`NOx(GT)`)
lm(formula =AirQualityUCI$`PT08.S5(O3)`~AirQualityUCI$`NOx(GT)`) 
pnorm(1.49)
pnorm(1.097)
qnorm(0.9318879) 
qnorm(0.8636793)
lm(formula = AirQualityUCI$`PT08.S3(NOx)` ~ AirQualityUCI$`NOx(GT)`)
lm(formula = AirQualityUCI$`PT08.S1(CO)` ~ AirQualityUCI$T)
lm(formula = AirQualityUCI$`NMHC(GT)` ~ AirQualityUCI$`PT08.S2(NMHC)`)
lm(formula = AirQualityUCI$`PT08.S5(O3)` ~ AirQualityUCI$`NOx(GT)`)
library(car)
mod=lm(AirQualityUCI$`PT08.S5(O3)` ~ AirQualityUCI$`NOx(GT)`)
summary(mod)
predict(mod)
lm(formula = AirQualityUCI$`PT08.S5(O3)` ~ AirQualityUCI$`NOx(GT)`)
#Create cross tabulations with derived variables
mydata<-AirQualityUCI 
utils::View(mydata)
attach(mydata)
mytable <- table(A,B)
print(table)
mytable
margin.table(mytable, 1)#a
#Check for trends and patterns in time series. 
ts (AirQualityUCI, frequency = 4, start = c(1959, 2))
# frequency 4 => Quarterly Data 
ts (1:10, frequency = 12, start = 1990)
# freq 12 => Monthly data. 
ts (AirQualityUCI, start=c(2009), end=c(2014), frequency=1) 
# Yearly Data 
ts (1:1000, frequency = 365, start = 1990)
# freq 365 => daily data. 
tsAirqualityUCI <- EuStockMarkets[, 1] 
#plot time series 
tsAirqualityUCI <- EuStockMarkets[, 1]
decomposedRes <- decompose(tsAirqualityUCI, type="mult") 
#Find out the most polluted time of the day and the name of the chemical compound 
tsAirqualityUCI <- EuStockMarkets[, 1] 
decomposedRes <- decompose(tsAirqualityUCI, type="mult")
plot (decomposedRes)
stlRes <- stl(tsAirqualityUCI, s.window = "periodic") 
plot(AirQualityUCI$T, type = "l")
#PT08.S4(NO2) is the highest pollution at 18.00 hrs PTO*s4