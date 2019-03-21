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
dataind04 <- dataind04[-c(1:196),]
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
pdf("plot_SD_ExchangeRate.pdf", height = 10, width = 15)
par(mfrow = c(2,1))
#plot(dataind04$Date, dataind04$SMI, type = "l", xlab = "Date", ylab = "Index", main = "SMI") # SMI
#plot(dataind04$Date, dataind04$SPIEX,  type = "l", xlab = "Date", ylab = "Index", main = "SPI Extra") # SPI Extra
#plot(dataind04$Date, dataind04$SMI.Mid,  type = "l", xlab = "Date", ylab = "Index", main = "SMI Mid") # SMI Mid
plot(dataind04$Date, dataind04$SNBSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Sight Deposits (in Mio. CHF)") # SNB Sight Deposits
plot(dataind04$Date, dataind04$CHF.EUR,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/EUR Exchange rate") # CHF/EUR Exchange rate
dev.off()


## Correlations --------------------------------------
# Indices & SNB Sight Deposits
cor.ind <- cor(dataind04[,-1], method = "spearman")

# All Variables
DataWoDates <- dataret[,-1] # Delete the Date column
Cor <- cor(DataWoDates, method = "spearman") # calculate the Correlation Matrix Spearman
# PreCapPeriods
PreCapPeriod.cor <- cor(DataWoDates[1:186,])
# During CapPeriods
CapPeriod.cor <- cor(DataWoDates[187:363, ])

# PostCapPeriod CapPeriods
PostCapPeriod.cor <- cor(DataWoDates[364:575,])



## Data extention & C50 algorithm --------------------------------------
# Using C5.0 algorithm

# SMI Stock indice --------------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SMI <- SMIdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SMI <- SMIdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SMI <- SMIdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 10)
PreCapPeriod.SMI.model <- C5.0(PreCapPeriod.SMI[-10], PreCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
PreCapPeriod.SMI.model
summary(PreCapPeriod.SMI.model) # SDdomBanksdir 9.14%

# CapPeriod Model describing SMI Forecast (column 10)
CapPeriod.SMI.model <- C5.0(CapPeriod.SMI[-10], CapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
CapPeriod.SMI.model
summary(CapPeriod.SMI.model) # 100% SDdomBanksdir, 41.24% ChgSDdomBanks

# PostCapPeriod Model describing SMI Forecast (column 10)
PostCapPeriod.SMI.model <- C5.0(PostCapPeriod.SMI[-10], PostCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
PostCapPeriod.SMI.model
summary(PostCapPeriod.SMI.model) # 6.6% SDdomBanksdir


# SPI Extra Stock indice --------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SPIEX <- SPIEXdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SPIEX <- SPIEXdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SPIEX <- SPIEXdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 10)
PreCapPeriod.SPIEX.model <- C5.0(PreCapPeriod.SPIEX[-10], PreCapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
PreCapPeriod.SPIEX.model
summary(PreCapPeriod.SPIEX.model) # 54.84% SDdomBanksdir, 52.15% ChgSDdomBanks

# CapPeriod Model describing SMI Forecast (column 10)
CapPeriod.SPIEX.model <- C5.0(CapPeriod.SPIEX[-10], CapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
CapPeriod.SPIEX.model
summary(CapPeriod.SPIEX.model) # 48.59% ChgSDdomBanks, 2.82% SDdomBanksdir

# PostCapPeriod Model describing SMI Forecast (column 10)
PostCapPeriod.SPIEX.model <- C5.0(PostCapPeriod.SPIEX[-10], PostCapPeriod.SPIEX$SPIEX.FC,rules = TRUE, trials = 100)
PostCapPeriod.SPIEX.model
summary(PostCapPeriod.SPIEX.model) # No usage of SNB data



# SMI Mid Stock indice --------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SMIM <- SMIMdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SMIM <- SMIMdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SMIM <- SMIMdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 10)
PreCapPeriod.SMIM.model <- C5.0(PreCapPeriod.SMIM[-10], PreCapPeriod.SMIM$SMIM.FC, rules = TRUE, trials = 100)
PreCapPeriod.SMIM.model
summary(PreCapPeriod.SMIM.model) # 14.52% SDdomBanksdir

# CapPeriod Model describing SMI Forecast (column 10)
CapPeriod.SMIM.model <- C5.0(CapPeriod.SMIM[-10], CapPeriod.SMIM$SMIM.FC, rules = TRUE, trials = 100)
CapPeriod.SMIM.model
summary(CapPeriod.SMIM.model) # 50.28% SDdomBanksdir, 2.82% ChgSDdomBanks

# PostCapPeriod Model describing SMI Forecast (column 10)
PostCapPeriod.SMIM.model <- C5.0(PostCapPeriod.SMIM[-10], PostCapPeriod.SMIM$SMIM.FC,rules = TRUE, trials = 100)
PostCapPeriod.SMIM.model
summary(PostCapPeriod.SMIM.model) # 14.15% ChgSDdomBanks, 11.32% SDdomBanksdir




