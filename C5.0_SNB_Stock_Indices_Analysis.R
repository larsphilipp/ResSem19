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
setwd("/Users/julianwossner/Desktop/MBF/Vorlesungen_2._Semester/Research_Seminar_Financial_Economics/Daten/ResSem19")
#setwd("C:/Users/LST/Documents/Uni/Research Seminar/ResSem19")


# Data Import --------------------------------------
# Index Data
dataind04 <- read.csv2("Indices from 04.csv", header = TRUE , sep = ";") #reading in data
dataind04 <- transform(dataind04, Date = as.Date(Date, format = "%Y-%m-%d") ,SNBSD = as.numeric(as.character(SNBSD)), SMI = as.numeric(as.character(SMI)), 
                     SPIEX = as.numeric(as.character(SPIEX)), SMI.Mid = as.numeric(as.character(SMI.Mid)), CHF.USD = as.numeric(as.character(CHF.USD)), CHF.EUR = as.numeric(as.character(CHF.EUR))) # Converting numbers into numeric format and date column to date format
# SNB Data
dataind08 <- read.csv2("Data_for_import_Indices_from_08.csv", header = TRUE , sep = ";") #reading in data


## Data Cleaning --------------------------------------
# Transforming the columns of the data frame into numeric
dataind08 <- transform(dataind08, Date = as.Date(Date, format = "%Y-%m-%d") ,SMI = as.numeric(as.character(SMI)), SDofDomBanks = as.numeric(as.character(SDofDomBanks)), 
                       SPIEX = as.numeric(as.character(SPIEX)), SMIMid = as.numeric(as.character(SMIMid)), Gov10yr = as.numeric(as.character(Gov10yr)),
                     Gov3yr = as.numeric(as.character(Gov3yr)), Libor3M_CHF = as.numeric(as.character(Libor3M_CHF)), CHFUSD = as.numeric(as.character(CHFUSD)),
                     CHFEUR = as.numeric(as.character(CHFEUR))) # Converting numbers into numeric format and date column to date format

dataind08[is.na(dataind08)] <- 0 # Replacing NA with 0, NA indruduced due to #DIV/0! Entry in excel.
str(dataind08)

## Return calculations --------------
# Stock data
RetSMI <-  diff(log(dataind08$SMI)) # Weekly returns of the SMI
RetSPIEX <- diff(log(dataind08$SPIEX)) # Weekly returns of the SPIEX
RetSMIMid <- diff(log(dataind08$SMIMid)) # Weekly returns of the SMIMid

# SNB Data
ChgSDdomBanks <- diff(dataind08$SDofDomBanks)/dataind08$SDofDomBanks[-length(dataind08$SDofDomBanks)] # Sight deposits of dom. Banks

# Creating Returns data set
dataret <- dataind08[-1, -c(2:5)]
dataret <- cbind(dataret, RetSMI, RetSPIEX, RetSMIMid, ChgSDdomBanks)

# Inclusions
SMIdata <- dataret[,-c(8,9)]  # Includes SMI as only stock indice
SPIEXdata <- dataret[,-c(7,9)] # Includes SPI Extra as only stock indice
SMIMdata <- dataret[, -c(7,8)] # Includes the SMI Mid as only stock indice


## Classification: Up / Down --------------------------------------

# SMI data
SMIdata$SMIdir <- as.factor(ifelse(SMIdata$RetSMI > 0, "up", ifelse(SMIdata$RetSMI < 0, "down", "no change" ))) # Including a column with up, down factors
SMIdata$SDdomBanksdir <- as.factor(ifelse(SMIdata$ChgSDdomBanks > 0, "up", ifelse(SMIdata$ChgSDdomBanks < 0, "down", "no change" )))
SMIdata$SMI.FC <- as.factor(ifelse(SMIdata$RetSMI > 0, "up", ifelse(SMIdata$RetSMI < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
SMIdata$SMI.FC[1:574] <- SMIdata$SMI.FC[2:575] # Lag the series to produce forecasts
SMIdata <-SMIdata[-575,] # delete the last row

# SPIEX data
SPIEXdata$SPIEXdir <- as.factor(ifelse(SPIEXdata$RetSPIEX > 0, "up", ifelse(SPIEXdata$RetSPIEX < 0, "down", "no change" ))) # Including a column with up, down factors
SPIEXdata$SDdomBanksdir <- as.factor(ifelse(SPIEXdata$ChgSDdomBanks > 0, "up", ifelse(SPIEXdata$ChgSDdomBanks < 0, "down", "no change" )))
SPIEXdata$SPIEX.FC <- as.factor(ifelse(SPIEXdata$RetSPIEX > 0, "up", ifelse(SPIEXdata$RetSPIEX < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
SPIEXdata$SPIEX.FC[1:574] <- SPIEXdata$SPIEX.FC[2:575] # Lag the series to produce forecasts
SPIEXdata <-SPIEXdata[-575,] # delete the last row

# SMImid data
SMIMdata$SMIdir <- as.factor(ifelse(SMIMdata$RetSMIMid > 0, "up", ifelse(SMIMdata$RetSMIMid < 0, "down", "no change" ))) # Including a column with up, down factors
SMIMdata$SDdomBanksdir <- as.factor(ifelse(SMIMdata$ChgSDdomBanks > 0, "up", ifelse(SMIMdata$ChgSDdomBanks < 0, "down", "no change" )))
SMIMdata$SMIM.FC <- as.factor(ifelse(SMIMdata$RetSMIMid > 0, "up", ifelse(SMIMdata$RetSMIMid < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
SMIMdata$SMIM.FC[1:574] <- SMIMdata$SMIM.FC[2:575] # Lag the series to produce forecasts
SMIMdata <-SMIMdata[-575,] # delete the last row








## Plotting --------------------------------------
# Indices
par(mfrow = c(3,2))
plot(dataind04$Date, dataind04$SMI, type = "l", xlab = "Date", ylab = "Index", main = "SMI") # SMI
plot(dataind04$Date, dataind04$SPIEX,  type = "l", xlab = "Date", ylab = "Index", main = "SPI Extra") # SPI Extra
plot(dataind04$Date, dataind04$SMI.Mid,  type = "l", xlab = "Date", ylab = "Index", main = "SMI Mid") # SMI Mid
plot(dataind04$Date, dataind04$SNBSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Sight Deposits") # SNB Sight Deposits
plot(dataind04$Date, dataind04$CHF.USD,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/USD Exchange rate") # CHF/USD Exchange rate
plot(dataind04$Date, dataind04$CHF.EUR,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/EUR Exchange rate") # CHF/EUR Exchange rate
dev.off()


## Correlations --------------------------------------
# Indices & SNB Sight Deposits
cor.ind <- cor(dataind04[,-1], method = "spearman")

# All Variables
DataWoDates <- dataret[,-1] # Delete the Date column
Cor <- cor(DataWoDates, method = "spearman") # calculate the Correlation Matrix Spearman
# PreCapPeriods
PreCapPeriod.cor <- cor(DataWoDates[1:227,])
# During CapPeriods
CapPeriod.cor <- cor(DataWoDates[228:413, ])

# PostCapPeriod CapPeriods
PostCapPeriod.cor <- cor(DataWoDates[414:575,])



## Data extention & C50 algorithm --------------------------------------
# Using C5.0 algorithm

# SMI Stock indice
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 137 is the 16th of Jan, i.e. 1 day after the removal of the Cap)
PreCapPeriod <- SMIdata[1:227,-1] # Data set from 2008 - 02 - 08 to 2012 - 06 - 01 (PreCapPeriod phase)
CapPeriod <- SMIdata[228:413,-1] # Data set from 2012 - 06 - 08 to 2016 - 01 - 01 (CapPeriod phase)
PostCapPeriod <- SMIdata[414:575,-1] # Data set from 2016 - 01 - 08 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 10)
PreCapPeriod.model <- C5.0(PreCapPeriod[-10], PreCapPeriod$SMI.FC, rules = TRUE, trials = 100)
PreCapPeriod.model
summary(PreCapPeriod.model)

# CapPeriod Model describing SMI Forecast (column 10)
CapPeriod.model <- C5.0(CapPeriod[-10], CapPeriod$SMI.FC, rules = TRUE, trials = 100)
CapPeriod.model
summary(CapPeriod.model)

# PostCapPeriod Model describing SMI Forecast (column 10)
PostCapPeriod.model <- C5.0(PostCapPeriod[-10], PostCapPeriod$SMI.FC, rules = TRUE, trials = 100)
PostCapPeriod.model
summary(PostCapPeriod.model)


# Julian: I imported a new csv file with data from 08 and split up the analysis into tree parts, a precap period, a cap period and a postcap period.
# ToDo: Implement the C5.0 for the SMIM dataset and the SPIEX data set. Maybe further analysis. TBD


