# Titel:        C5.0 Decision Tree Analysis: Effect of SNB Policy on SMI Mid (2011 - 2018)
# Course:       Financial Economics Reserach Seminar
# Institute:    University of St. Gallen
# Authors:      Julian Woessner, Lars Stauffenegger
# Date:         March 2019
# Version:      5.1
# Description:  This File contains the statistical analysis of the monetary and
#               financial market data for the research seminar in Financial Economics

# installing packages
library(PerformanceAnalytics)
library(AER)
library(C50)
library(Hmisc)
library(jtools)
library(huxtable)
library(zoo)
library(ggplot2)

# setting working directory
getwd()
#setwd("C:/Users/Lars Stauffenegger/Documents/MBF Unisg/Research Seminar/ResSem19")
setwd("C:/Users/LST/Documents/Uni/Research Seminar/ResSem19")


# Data Import --------------------------------------
# Index Data
dataind <- read.csv2("Indices from 04.csv", header = TRUE , sep = ";") #reading in data
dataind <- transform(dataind, Date = as.Date(Date, format = "%Y-%m-%d") ,SNBSD = as.numeric(as.character(SNBSD)), SMI = as.numeric(as.character(SMI)), 
                     SPIEX = as.numeric(as.character(SPIEX)), SMI.Mid = as.numeric(as.character(SMI.Mid)), CHF.USD = as.numeric(as.character(CHF.USD)), CHF.EUR = as.numeric(as.character(CHF.EUR))) # Converting numbers into numeric format and date column to date format
# SNB Data
dataret <- read.csv2("More data test.csv", header = TRUE , sep = ";") #reading in data


## Data Cleaning --------------------------------------
# Transforming the columns of the data frame into numeric
dataret <- transform(dataret, Date = as.Date(Date, format = "%Y-%m-%d") ,SMI = as.numeric(as.character(SMI)), DomSD = as.numeric(as.character(DomSD)), 
                     MinRes = as.numeric(as.character(MinRes)), TotalSD = as.numeric(as.character(TotalSD)), SDdomBanks = as.numeric(as.character(SDdomBanks)), OtherSDwkl = as.numeric(as.character(OtherSDwkl)), 
                     SNBFXinv = as.numeric(as.character(SNBFXinv)), M1 = as.numeric(as.character(M1)), SPIEX = as.numeric(as.character(SPIEX)), SMIM = as.numeric(as.character(SMIM)), Gov10yr = as.numeric(as.character(Gov10yr)),
                     Gov3yr = as.numeric(as.character(Gov3yr)), Gov1yr = as.numeric(as.character(Gov1yr)), LIBOR3M = as.numeric(as.character(LIBOR3M)), BankIntchgrate = as.numeric(as.character(BankIntchgrate)), CHFUSD = as.numeric(as.character(CHFUSD)),
                     CHFEUR = as.numeric(as.character(CHFEUR))) # Converting numbers into numeric format and date column to date format

dataret[is.na(dataret)] <- 0 # Replacing NA with 0, NA indruduced due to #DIV/0! Entry in excel.
str(dataret)

# Inclusions
SMIdata <- dataret[,-c(1,10,11)]  # Includes SMI as only stock indice
SPIEXdata <- dataret[,-c(1,2,11)] # Includes SPI Extra as only stock indice
SMIMdata <- dataret[, -c(1,2,10)] # Includes the SMI Mid as only stock indice
SNBdata <- dataret[, -c(1,12:18)] # Includes all indices but only the SNB figures
SNB.SMIdata <- SNBdata[,-c(9,10)] # Includes only the SMI as stock indice and all SNB samples
SNB.SPIEXdata <- SNBdata[, -c(1,10)] #Includes only the SPI extra as stock indice and all SNB samples
SNB.SMIMdata <- SNBdata[, -c(1,9)] #Includes only the SMI Mid as stock indice and all SNB samples

## Classification: Up / Down --------------------------------------
SNB.SMIMdata$SMIMdir <- as.factor(ifelse(SNB.SMIMdata$SMIM > 0, "up", ifelse(SNB.SMIMdata$SMIM < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$DomSDdir <- as.factor(ifelse(SNB.SMIMdata$DomSD > 0, "up", ifelse(SNB.SMIMdata$DomSD < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$MinResdir <- as.factor(ifelse(SNB.SMIMdata$MinRes > 0, "up", ifelse(SNB.SMIMdata$MinRes < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$TotalSDdir <- as.factor(ifelse(SNB.SMIMdata$TotalSD > 0, "up", ifelse(SNB.SMIMdata$TotalSD < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$SDdomBanksdir <- as.factor(ifelse(SNB.SMIMdata$SDdomBanks > 0, "up", ifelse(SNB.SMIMdata$SDdomBanks < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$OtherSDwkldir <- as.factor(ifelse(SNB.SMIMdata$OtherSDwkl > 0, "up", ifelse(SNB.SMIMdata$OtherSDwkl < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$SNBFXinvdir <- as.factor(ifelse(SNB.SMIMdata$SNBFXinv > 0, "up", ifelse(SNB.SMIMdata$SNBFXinv < 0, "down", "no change" ))) # Including a column with up, down factors
SNB.SMIMdata$SMIM.FC <- as.factor(ifelse(SNB.SMIMdata$SMIM > 0, "up", ifelse(SNB.SMIMdata$SMIM < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
SNB.SMIMdata$SMIM.FC[1:350] <- SNB.SMIMdata$SMIM.FC[2:351] # Lag the series to produce forecasts
SNB.SMIMdata <- SNB.SMIMdata[-351,] # delete the last row

# Two Periods: During and Post Cap (row 137 is the 16th of Jan, i.e. 1 day after the removal of the Cap)
CapPeriod <- SNB.SMIMdata[1:137, ] # Data set from 2012 - 06 - 08 to 2016 - 01 - 01 (CapPeriod phase)
PostCapPeriod <- SNB.SMIMdata[138:350, ] # Data set from 2016 - 01 - 08 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


## Plotting --------------------------------------
# Indices
par(mfrow = c(3,2))
plot(dataind$Date, dataind$SMI, type = "l", xlab = "Date", ylab = "Index", main = "SMI") # SMI
plot(dataind$Date, dataind$SPIEX,  type = "l", xlab = "Date", ylab = "Index", main = "SPI Extra") # SPI Extra
plot(dataind$Date, dataind$SMI.Mid,  type = "l", xlab = "Date", ylab = "Index", main = "SMI Mid") # SMI Mid
plot(dataind$Date, dataind$SNBSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Sight Deposits") # SNB Sight Deposits
plot(dataind$Date, dataind$CHF.USD,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/USD Exchange rate") # CHF/USD Exchange rate
plot(dataind$Date, dataind$CHF.EUR,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/EUR Exchange rate") # CHF/EUR Exchange rate
dev.off()

# Various Timeseries
plot(dataret$Date, log(1+dataret$SMI), type = "l", main = "SMI weekly change", xlab = "Date", ylab = "LogReturns") # SMI
plot(dataret$Date, dataret$DomSD ,type = "l", main = "Sight Deposits weekly change", xlab = "Date", ylab = "Change") # Domestic Sight Deposits
plot(dataret$Date, dataret$MinRes ,type = "l", main = "Minimum Reserves weekly change", xlab = "Date", ylab = "Change") # Minimum Reserves
plot(dataret$Date, dataret$TotalSD ,type = "l", main = "Total Sight Deposits weekly change", xlab = "Date", ylab = "Change") # Total Sight Deposits
plot(dataret$Date, dataret$SDdomBanks ,type = "l", main = "Sight Deposits of Domestic Banks weekly change", xlab = "Date", ylab = "Change") # Sight Deposits of domestic Banks
plot(dataret$Date, dataret$OtherSDwkl ,type = "l", main = "Other Sight Deposits weekly change", xlab = "Date", ylab = "Change") # Ohter Sight Deposits of domestic Banks weekly
plot(dataret$Date, dataret$SNBFXinv ,type = "l", main = "Investment in Foreign Currencies weekly change", xlab = "Date", ylab = "Change") # Investments of the SNB into Foreign Currencies
plot(dataret$Date, dataret$M1 ,type = "l", main = "M1 Money amount weekly change", xlab = "Date", ylab = "Change") # M1 Money amount
plot(dataret$Date, log(1+dataret$SPIEX) ,type = "l", main = "SPI Extra weekly change", xlab = "Date", ylab = "LogReturns") # SPI Extra
plot(dataret$Date, log(1+dataret$SMIM) ,type = "l", main = "SMI Mid weekly change", xlab = "Date", ylab = "LogReturns") # SMI Mid
plot(dataret$Date, dataret$Gov10yr ,type = "l", main = "Treasuries Interest rate 10 years weekly change", xlab = "Date", ylab = "Change") # Long term interest rate on Swiss Government Bonds 10 years
plot(dataret$Date, dataret$Gov3yr ,type = "l", main = "Treasuries Interest rate 3 years weekly change", xlab = "Date", ylab = "Change") # Long term interest rate on Swiss Government Bonds 3 years
plot(dataret$Date, dataret$LIBOR3M ,type = "l", main = "Treasuries Interest rate 1 year weekly change", xlab = "Date", ylab = "Change") # LIBOR 3 Months
plot(dataret$Date, dataret$BankIntchgrate ,type = "l", main = "Bank Overnight Lending Interest rate weekly change", xlab = "Date", ylab = "Change") # Interest rate banks charge for lending overnight
plot(dataret$Date, dataret$CHFUSD ,type = "l", main = "CHF / USD weekly change", xlab = "Date", ylab = "Change") # CHF / USD exchange rate
plot(dataret$Date, dataret$CHFEUR ,type = "l", main = "CHF / EUR weekly change", xlab = "Date", ylab = "Change") # CHF/EUR exchange rate


## Correlations --------------------------------------
# Indices & SNB Sight Deposits
cor.ind <- cor(dataind[,-1], method = "spearman")

# All Variables
DataWoDates <- dataret[,-1] # Delete the Date column
Cor <- cor(DataWoDates, method = "spearman") # calculate the Correlation Matrix Spearman

# During CapPeriods
CapPeriod.cor <- cor(DataWoDates[1:137, ])

# PostCapPeriod CapPeriods
PostCapPeriod.cor <- cor(DataWoDates[138:350,])


## Data extention & C50 algorithm --------------------------------------
# Using C5.0 algorithm
# Implement the model for the CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# CapPeriod Model describing SMIM Forecast (column 16)
CapPeriod.model <- C5.0(CapPeriod[-16], CapPeriod$SMIM.FC, rules = TRUE)
CapPeriod.model
summary(CapPeriod.model)

# PostCapPeriod Model describing SMIM Forecast (column 16)
PostCapPeriod.model <- C5.0(PostCapPeriod[-16], PostCapPeriod$SMIM.FC, rules = TRUE)
PostCapPeriod.model
summary(PostCapPeriod.model)

# Predict up and down movements of SMIM during the Cap Period
CapPeriod.train <- CapPeriod[1:99,] # Defining the training data set
CapPeriod.test <- CapPeriod[100:137,] # Defining the test data set
CapPeriod.model <- C5.0(CapPeriod.train[-16], CapPeriod.train$SMIM.FC, rules = TRUE, trials = 100) # Induce the decision tree from the training data, using rules and boosting
CapPeriod.predict <- predict(CapPeriod.model, newdata= CapPeriod.test[,-16]) # Predict the up/down movements of the SMIM with the induced decision tree

summary(CapPeriod.model)
summary(CapPeriod.predict)
summary(CapPeriod.test$SMIM.FC)

accuracyCapPeriod <- (sum( CapPeriod.predict == CapPeriod.test$SMIM.FC ) / length( CapPeriod.predict ))*100 # Calculating the accuracy of the predicitons.
accuracyCapPeriod # 55%
## TODO: may be add a variable, this predictions does not really work


# Predict up and down movements of SMIM during the Post Cap Period
PostCapPeriod.train <- PostCapPeriod[1:159,] # Defining the training data set
PostCapPeriod.test <- PostCapPeriod[160:213,] # Defining the test data set
PostCapPeriod.model <- C5.0(PostCapPeriod.train[,-16], PostCapPeriod.train$SMIM.FC, rules = TRUE, trials = 100) # Induce the decision tree from the training data, using rules and boosting
PostCapPeriod.predict <- predict(PostCapPeriod.model, newdata= PostCapPeriod.test[,-16]) # Predict the up/down movements of the SMIM with the induced decision tree

summary(PostCapPeriod.model)
summary(PostCapPeriod.predict)
summary(PostCapPeriod.test$SMIM.FC)

accuracyPostCapPeriod <- (sum( PostCapPeriod.predict == PostCapPeriod.test$SMIM.FC ) / length( PostCapPeriod.predict ))*100 # Calculating the accuracy of the predicitons.
accuracyPostCapPeriod # 53%
## TODO: may be add a variable, this predictions does not really work


# Predict up and down movements for whole dataset

# Shorter Training - 56%
SMIM.train <- SNB.SMIMdata[1:150,]
SMIM.test <- SNB.SMIMdata[151:350,]

# Longer training - 54%
SMIM.train <- SNB.SMIMdata[1:300,]
SMIM.test <- SNB.SMIMdata[301:350,]

# train = Cap, test = PostCap - 50%
SMIM.train <- SNB.SMIMdata[1:137,]
SMIM.test <- SNB.SMIMdata[138:350,]

# Model and Prediction for SMI Mid Cap
model <- C5.0(SMIM.train[-16], SMIM.train$SMIM.FC, rules = TRUE, trials = 100) 
predict <- predict(model, newdata = SMIM.test[,-16])
summary(model)
summary(predict)
summary(test$SMIM.FC)
accuracySMIMid <- (sum( predict == SMIM.test$SMIM.FC ) / length( predict ))*100 # Calculating the accuracy of the predicitons.
accuracySMIMid
## TODO: carefully add 1 or 2 variables, See if we can get rid of data n/a before '12, Understand rules