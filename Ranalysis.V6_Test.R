# Author: Julian Woessner

# Date: 10.03.2019

# Version 6.0

# This File contains the statistical analysis of the monetary and return data for the research seminar in Financial Economics
getwd()
setwd("C:/Users/Lars Stauffenegger/Documents/MBF Unisg/Research Seminar/ResSem19")
# setting working directory

# installing packages
library(PerformanceAnalytics)
library(AER)
library(C50)

library(Hmisc)
library(jtools)
library(huxtable)
library(zoo)
library(ggplot2)


#### Loading in the source data file

data <- read.csv2("Monetary_Data_Handcollected_and_Datastream.csv", header = TRUE , sep = ";") #reading in data

data <- transform(data, Date = as.Date(Date, format = "%Y-%m-%d") ,SDofDomBanks = as.numeric(as.character(SDofDomBanks)), OtherSD = as.numeric(as.character(OtherSD)), 
                     TotalSD = as.numeric(as.character(TotalSD)), M1Switzerland = as.numeric(as.character(M1Switzerland)), SNBFXinv = as.numeric(as.character(SNBFXinv)), SPIEX = as.numeric(as.character(SPIEX)),
                     SMI = as.numeric(as.character(SMI)), SMIMid = as.numeric(as.character(SMIMid)), Gov10yr = as.numeric(as.character(Gov10yr)), Gov3yr = as.numeric(as.character(Gov3yr)), Libor3M_CHF = as.numeric(as.character(Libor3M_CHF)), OvernightLendingRate = as.numeric(as.character(OvernightLendingRate)), CHFUSD = as.numeric(as.character(CHFUSD)),
                     CHFEUR = as.numeric(as.character(CHFEUR)))# Converting numbers into numeric format and date column to date format
str(data)



## 1) Data cleaning and preparation: Creating percentage change columns for the indices and the SNB assets
# Stock data
RetSMI <-  diff(log(data$SMI)) # Weekly returns of the SMI
RetSPIEX <- diff(log(data$SPIEX)) # Weekly returns of the SPIEX
RetSMIMid <- diff(log(data$SMIMid)) # Weekly returns of the SMIMid

# SNB Data
ChgSDdomBanks <- diff(log(data$SDofDomBanks)) # Sight deposits of dom. Banks
ChgOtherSD <- diff(log(data$OtherSD)) # Other Sight Deposits
ChgTotalSD <- diff(log(data$TotalSD)) # Total Sight Deposits
ChgM1Switzerland <- diff(log(data$M1Switzerland)) # M1 Switzerland
ChgSNBFXinv <- diff(log(data$SNBFXinv)) # SNB investements in foreign currency

dataret <- data[-1,] # Deleting the first row
dataret <- cbind(dataret, RetSMI, RetSPIEX, RetSMIMid, ChgSDdomBanks, ChgOtherSD, ChgTotalSD, ChgM1Switzerland, ChgSNBFXinv)

# Creating the directions columns indicating whether the stocks or SNB data went up, down or stayed the same

# For the stock indices
dataret$SMIDir <- as.factor(ifelse(dataret$RetSMI > 0, "up", ifelse(dataret$RetSMI < 0, "down", "no change" ))) # Direction of the SMI in the respective week
dataret$SPIEXDir <- as.factor(ifelse(dataret$RetSPIEX > 0, "up", ifelse(dataret$RetSPIEX < 0, "down", "no change" ))) # Direction of the SPI Extra in the respective week
dataret$SMIMidDir <- as.factor(ifelse(dataret$RetSMIMid > 0, "up", ifelse(dataret$RetSMIMid < 0, "down", "no change" ))) # Direction of the SMI Mid in the respective week

# For the SNB data
dataret$SDofDomBanksDir <- as.factor(ifelse(dataret$ChgSDdomBanks > 0, "up", ifelse(dataret$ChgSDdomBanks < 0, "down", "no change" ))) # Direction of the Sight Deposits of dom. Banks in the respective week
dataret$OtherSDDir <- as.factor(ifelse(dataret$ChgOtherSD > 0, "up", ifelse(dataret$ChgOtherSD < 0, "down", "no change" ))) # Direction of the Ohter Sight Deposits in the respective week
dataret$TotalSDDir <- as.factor(ifelse(dataret$ChgTotalSD > 0, "up", ifelse(dataret$ChgTotalSD < 0, "down", "no change" ))) # Direction of the Total Sight Deposits in the respective week


# Creating the forcast columns for the SMI, SPIEX and SMIMid

# SMI
dataret$SMIForCast <- as.factor(ifelse(dataret$RetSMI > 0, "up", ifelse(dataret$RetSMI < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
dataret$SMIForCast[1:nrow(dataret)-1] <- dataret$SMIForCast[2:nrow(dataret)] # Lag the series to produce forecasts

# SPIEX
dataret$SPIEXForCast <- as.factor(ifelse(dataret$RetSPIEX > 0, "up", ifelse(dataret$RetSPIEX < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
dataret$SPIEXForCast[1:nrow(dataret)-1] <- dataret$SPIEXForCast[2:nrow(dataret)] # Lag the series to produce forecasts

# SMIMid
dataret$SMIMidForCast <- as.factor(ifelse(dataret$RetSMIMid > 0, "up", ifelse(dataret$RetSMIMid < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
dataret$SMIMidForCast[1:nrow(dataret)-1] <- dataret$SMIMidForCast[2:nrow(dataret)] # Lag the series to produce forecasts


dataret <- dataret[-nrow(dataret),] # delete the last row to remove the NAs in the last row


#### 2) Plotting the indices

par(mfrow = c(2,2))
  
plot(data$Date, data$SMI, type = "l", xlab = "Date", ylab = "Index", main = "SMI") # SMI
plot(data$Date, data$SPIEX,  type = "l", xlab = "Date", ylab = "Index", main = "SPI Extra") # SPI Extra
plot(data$Date, data$SMIMid,  type = "l", xlab = "Date", ylab = "Index", main = "SMI Mid") # SMI Mid
plot(data$Date, data$SDofDomBanks,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Sight Deposits of Dom. Banks") # SNB Sight Deposits
plot(data$Date, data$OtherSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Ohter Sight Deposits") # SNB Other Sight Deposits
plot(data$Date, data$TotalSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Total Sight Deposits") # SNB Total Sight Deposits
plot(data$Date, data$M1Switzerland,  type = "l", xlab = "Date", ylab = "Index", main = "M1 Switzerland") # SNB M1 Switzerland
plot(data$Date, data$SNBFXinv,  type = "l", xlab = "Date", ylab = "Index", main = "SNBFXinv") # SNB Investments in foreign currencies
plot(data$Date, data$Gov10yr,  type = "l", xlab = "Date", ylab = "Index", main = "10 year Treasury yield") # Ten year treasury yield
plot(data$Date, data$Gov3yr,  type = "l", xlab = "Date", ylab = "Index", main = "3 year Treasury yield") # Three year treasury yield
plot(data$Date, data$Libor3M_CHF,  type = "l", xlab = "Date", ylab = "Index", main = "LIBOR 3 M") # Three months LIBOR yield
plot(data$Date, data$OvernightLendingRate,  type = "l", xlab = "Date", ylab = "Index", main = "Overnight Lending Rate") # Overnight lending rate
plot(data$Date, data$CHFUSD,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/USD Exchange rate") # CHF/USD Exchange rate
plot(data$Date, data$CHFEUR,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/EUR Exchange rate") # CHF/EUR Exchange rate

dev.off()

# 3) Correlation analysis: Correlation matrix
corr.mat <- cor(dataret[,-1], method = "spearman")


# Splitting up the data in High and low intervention phase. 
HighInt <- dataret[1:229,] # Data set from start to 01.01.2016
LowInt <- dataret[230:392,] # Data set from 08.01.2016 - end


# Correlation matrix in high intervention phase
corr.mat.high <- cor(HighInt[,-1], method = "spearman")

# Correlation phase in low intervention phase

corr.mat.low <- cor(LowInt[,-1], method = "spearman")

# Result corresponding to correlation: We see that the SMI, SPIEX and SMI Mid are generally strongly correlated with
# the sight deposits of domestic banks. If we split the data, we see that during the intervention phase, the correlation between
# stocks and the Sight deposits is much stronger than in the after phase.


## 4) Creating decision trees and prediction analysis

### Analyzing the SMI
SMIData <- dataret[, -c(1,7,9,17, 18, 25, 26, 31, 32)]
TrainingDataSMI <- SMIData[1:300,] # Defining the training data from beginnig to 01.01.2016
TestDataSMI <- SMIData[301:391,] # Defining the test data from 08.01.2016 - end

modelSMI <- C5.0(TrainingDataSMI[-23], TrainingDataSMI$SMIForCast, rules = TRUE) 
summary(modelSMI)
predictSMI <- predict(modelSMI, newdata = TestDataSMI)
summary(predict)
summary(TestDataSMI$SMIForCast)
accuracySMI <- (sum( predictSMI == TestDataSMI$SMIForCast ) / length( predictSMI ))*100 # Calculating the accuracy of the predicitons.
accuracySMI

## Analyzing the SMI Mid
#SMIMidData <- dataret[,-c(1,7,8,10, 11, 12, 13, 14, 15, 16,17,24,25,30,31)]
SMIMidData <- dataret[,-c(1:15, 16, 17,24, 25, 30, 31 )]
TrainingDataSMIMid<- SMIMidData[1:300,] # Defining the training data from beginnig to 01.01.2016
TestDataSMIMid <- SMIMidData[301:391,] # Defining the test data from 08.01.2016 - end

modelSMIMid <- C5.0(TrainingDataSMIMid[-11], TrainingDataSMIMid$SMIMidForCast, rules = TRUE) 
predictSMIMid <- predict(modelSMIMid, newdata = TestDataSMIMid)
summary(modelSMIMid)
summary(predictSMIMid)
summary(TestDataSMIMid$SMIMidForCast)
accuracySMIMid <- (sum( predictSMIMid == TestDataSMIMid$SMIMidForCast ) / length( predictSMIMid ))*100 # Calculating the accuracy of the predicitons.
accuracySMIMid

# The strange thing is, when we compute the prediction only on the percentage changes for the SMI Mid, we get an accuracy of 96%,
# If we compute the precision on the basis of the data in this version, we only get 50%



