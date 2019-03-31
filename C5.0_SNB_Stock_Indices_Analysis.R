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
library(psych)
library(xtable)

# setting working directory
getwd()
setwd("C:/Users/Lars Stauffenegger/Documents/MBF Unisg/Research Seminar/ResSem19")
#setwd("/Users/julianwossner/Desktop/MBF/Vorlesungen_2._Semester/Research_Seminar_Financial_Economics/Daten/ResSem19")
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
RetCHFEUR <- diff(log(dataind08$CHFEUR)) # Weekly returns of the SMIMid

# SNB Data
ChgSDdomBanks <- diff(dataind08$SDofDomBanks)/dataind08$SDofDomBanks[-length(dataind08$SDofDomBanks)] # Sight deposits of dom. Banks

# Creating Returns data set
dataret <- dataind08[-1,]
dataret <- cbind(dataret, RetSMI, RetSPIEX, RetSMIMid, ChgSDdomBanks, RetCHFEUR) # adding percent changes to the data set

# Inclusions
SMIdata    <- dataret[,-c(3,5,12,13,15)]   # Includes SMI as only stock indice
SPIEXdata  <- dataret[,-c(4,5,11,13,15)] # Includes SPI Extra as only stock indice
SMIMdata   <- dataret[,-c(3,4,11,12,15)]  # Includes the SMI Mid as only stock indice
#CHFEURdata <- dataret[,-c(3,5,12,13)] # including SMI
#CHFEURdata <- dataret[,-c(3,4,11,12)] # including SMIM
CHFEURdata <- dataret # including SMI and SMIM

## Data --------------------------------------
allData <- dataret

# Directions: Up / Down
allData$SMIdir <- as.factor(ifelse(allData$RetSMI > 0, "up", ifelse(allData$RetSMI < 0, "down", "no change" ))) # Including a column with up, down factors
allData$SMIMdir <- as.factor(ifelse(allData$RetSMIM > 0, "up", ifelse(allData$RetSMIM < 0, "down", "no change" ))) # Including a column with up, down factors
allData$SPIEXdir <- as.factor(ifelse(allData$RetSPIEX > 0, "up", ifelse(allData$RetSPIEX < 0, "down", "no change" ))) # Including a column with up, down factors
allData$SDdomBanksdir <- as.factor(ifelse(allData$ChgSDdomBanks > 0, "up", ifelse(allData$ChgSDdomBanks < 0, "down", "no change" )))
allData$CHFEURdir <- as.factor(ifelse(allData$RetCHFEUR > 0, "up", ifelse(allData$RetCHFEUR < 0, "down", "no change" ))) # Including a column with up, down factors

# Previous Week
allData$CHFEURprev <- allData$CHFEUR
allData$CHFEURprev[2:575] <- allData$CHFEURprev[1:574]
allData$SMIprev <- allData$SMI
allData$SMIprev[2:575] <- allData$SMIprev[1:574]
allData$SMIMprev <- allData$SMIMid
allData$SMIMprev[2:575] <- allData$SMIMprev[1:574]
allData$SPIEXprev <- allData$SPIEX
allData$SPIEXprev[2:575] <- allData$SPIEXprev[1:574]

# Next Week & Lag the series to produce forecasts
allData$SMInext <- allData$SMIdir
allData$SMInext[1:574] <- allData$SMInext[2:575]
allData$SMIMnext <- allData$SMIMdir
allData$SMIMnext[1:574] <- allData$SMIMnext[2:575]
allData$SPIEXnext <- allData$SPIEXdir
allData$SPIEXnext[1:574] <- allData$SPIEXnext[2:575]
allData$CHFEURnext <- allData$CHFEURdir
allData$CHFEURnext[1:574] <- allData$CHFEURdir[2:575] # CHFEURdata$CHFEURdir[1:574]

# delete first row and the last rows
allData <-allData[2:556,] 


# List of Colums
basicColumnsIndex <- c("SDofDomBanks","ChgSDdomBanks","SDdomBanksdir","CHFUSD","RetCHFEUR","CHFEUR","CHFEURdir","Gov3yr","Gov10yr","Libor3M_CHF")
basicColumnsFX    <- c("SDofDomBanks","ChgSDdomBanks","SDdomBanksdir","CHFUSD","Gov3yr","Gov10yr","Libor3M_CHF")

# Indices
currentSMIColumns   <- c(basicColumnsIndex,"SMIprev","SMIdir")
currentSMIMColumns  <- c(basicColumnsIndex,"SMIMprev","SMIMdir")
currentSPIEXColumns <- c(basicColumnsIndex,"SPIEXprev","SPIEXdir")

forecastSMIColumns   <- c(basicColumnsIndex,"SMI","SMInext")
forecastSMIMColumns  <- c(basicColumnsIndex,"SMIMid","SMIMnext")
forecastSPIEXColumns <- c(basicColumnsIndex,"SPIEX","SPIEXnext")

# Fx
currentFxSMIColumns   <- c(basicColumnsFX,"RetSMI","SMI","SMIdir","CHFEURprev","CHFEURdir")
currentFxSMIMColumns  <- c(basicColumnsFX,"RetSMIMid","SMIMid","SMIMdir","CHFEURprev","CHFEURdir")
currentFxSPIEXColumns <- c(basicColumnsFX,"RetSPIEX","SPIEX","SPIEXdir","CHFEURprev","CHFEURdir")

forecastFxSMIColumns   <- c(basicColumnsFX,"RetSMI","SMI","SMIdir","CHFEUR","CHFEURnext")
forecastFxSMIMColumns  <- c(basicColumnsFX,"RetSMIMid","SMIMid","SMIMdir","CHFEUR","CHFEURnext")
forecastFxSPIEXColumns <- c(basicColumnsFX,"RetSPIEX","SPIEX","SPIEXdir","CHFEUR","CHFEURnext")

### -------------------------------------------

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
SMIMdata$SMIMdir <- as.factor(ifelse(SMIMdata$RetSMIMid > 0, "up", ifelse(SMIMdata$RetSMIMid < 0, "down", "no change" ))) # Including a column with up, down factors
SMIMdata$SDdomBanksdir <- as.factor(ifelse(SMIMdata$ChgSDdomBanks > 0, "up", ifelse(SMIMdata$ChgSDdomBanks < 0, "down", "no change" )))
SMIMdata$SMIM.FC <- as.factor(ifelse(SMIMdata$RetSMIMid > 0, "up", ifelse(SMIMdata$RetSMIMid < 0, "down", "no change" ))) # Reproduce the column SMIM.Dir
SMIMdata$SMIM.FC[1:574] <- SMIMdata$SMIM.FC[2:575] # Lag the series to produce forecasts
SMIMdata <-SMIMdata[-575,] # delete the last row

# CHFEUR data
CHFEURdata$SMIdir <- as.factor(ifelse(CHFEURdata$RetSMI > 0, "up", ifelse(CHFEURdata$RetSMI < 0, "down", "no change" ))) # Including a column with up, down factors
CHFEURdata$SMIMdir <- as.factor(ifelse(CHFEURdata$RetSMIM > 0, "up", ifelse(CHFEURdata$RetSMIM < 0, "down", "no change" ))) # Including a column with up, down factors
CHFEURdata$SPIEXdir <- as.factor(ifelse(CHFEURdata$RetSPIEX > 0, "up", ifelse(CHFEURdata$RetSPIEX < 0, "down", "no change" ))) # Including a column with up, down factors
CHFEURdata$SDdomBanksdir <- as.factor(ifelse(CHFEURdata$ChgSDdomBanks > 0, "up", ifelse(CHFEURdata$ChgSDdomBanks < 0, "down", "no change" )))
CHFEURdata$CHFEURdir <- as.factor(ifelse(CHFEURdata$RetCHFEUR > 0, "up", ifelse(CHFEURdata$RetCHFEUR < 0, "down", "no change" ))) # Including a column with up, down factors
CHFEURdata$CHFEURprev <- CHFEURdata$CHFEUR # CHFEURdata$CHFEURdir
CHFEURdata$CHFEURprev[2:575] <- CHFEURdata$CHFEUR[1:574] # CHFEURdata$CHFEURdir[1:574]
CHFEURdata$CHFEURnext <- CHFEURdata$CHFEURdir
CHFEURdata$CHFEURnext[1:574] <- CHFEURdata$CHFEURdir[2:575] # CHFEURdata$CHFEURdir[1:574]
CHFEURdata <-CHFEURdata[2:556,] # delete the last row

## Plotting and descriptive statistics --------------------------------------
# Indices
pdf("plot_SD_ExchangeRate.pdf", height = 20, width = 15)
par(mfrow = c(5,1))
plot(dataind04$Date, dataind04$SNBSD,  type = "l", xlab = "Date", ylab = "Index", main = "SNB Sight Deposits (in Mio. CHF)") # SNB Sight Deposits
plot(dataind04$Date, dataind04$CHF.EUR,  type = "l", xlab = "Date", ylab = "Index", main = "CHF/EUR Exchange rate") # CHF/EUR Exchange rate
plot(dataind04$Date, dataind04$SMI, type = "l", xlab = "Date", ylab = "Index", main = "SMI") # SMI
plot(dataind04$Date, dataind04$SPIEX,  type = "l", xlab = "Date", ylab = "Index", main = "SPI Extra") # SPI Extra
plot(dataind04$Date, dataind04$SMI.Mid,  type = "l", xlab = "Date", ylab = "Index", main = "SMI Mid") # SMI Mid
dev.off()

# Descriptive statistics
# All Variables
DataWoDates <- dataret[,-1] # Delete the Date column

# PreCap period
descr.PreCap <- describe(DataWoDates[1:186,]) # summary statistics for PreCap period
mean(DataWoDates[1:186,]$RetSMI)
mean(DataWoDates[1:186,]$RetSPIEX)
mean(DataWoDates[1:186,]$RetSMIMid)
min(DataWoDates[1:186,]$Libor3M_CHF)
min(DataWoDates[1:186,]$ChgSDdomBanks)

# Cap period
descr.Cap <- describe(DataWoDates[187:363,]) # summary statistics for Cap period
mean(DataWoDates[187:363,]$RetSMI)
mean(DataWoDates[187:363,]$RetSPIEX)
mean(DataWoDates[187:363,]$RetSMIMid)
mean(DataWoDates[187:363,]$ChgSDdomBanks)

# PostCap period
descr.PostCap <- describe(DataWoDates[364:575,]) # summary statistics for PostCap period
mean(DataWoDates[364:575,]$RetSMI)
mean(DataWoDates[364:575,]$RetSPIEX)
mean(DataWoDates[364:575,]$RetSMIMid)
mean(DataWoDates[364:575,]$ChgSDdomBanks)


## Correlations --------------------------------------
# Indices & SNB Sight Deposits
cor.ind <- cor(dataind04[,-1], method = "spearman") # Correlation for whole time period
Cor <- cor(DataWoDates, method = "spearman") # calculate the Correlation Matrix Spearman

# PreCapPeriods
PreCapPeriod.cor <- cor(DataWoDates[1:186,], method = "spearman") # spearman correlation for PreCap period

# During CapPeriods
CapPeriod.cor <- cor(DataWoDates[187:363, ], method = "spearman") # spearman correlation for Cap period

# PostCapPeriod CapPeriods
PostCapPeriod.cor <- cor(DataWoDates[364:575,], method = "spearman") # spearman correlation for PostCap period



## Data extention & C50 algorithm --------------------------------------

# Functions and Obejcts BEGIN ------------------------------------------

samplingC5 <- function(independentVariables, targetVariable) {
    
    sumVariableImportance = setNames(data.frame(matrix(ncol = length(names(independentVariables)), nrow = 1)), names(independentVariables))
    boostTrain = vector() 
    boostTest = vector()
  
    for (i in 1:10) {
      
      model <- C5.0(independentVariables, targetVariable,
                    rules = TRUE, trials = 10, 
                    control = C5.0Control(sample=0.7))
      #summary(model)
      
      # parsing Boost Error Value
      output <- strsplit(model[["output"]], "\n")[[1]]
      boostRow <- grep("^boost\t", output)
      boostTrain[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[1])])
      boostTest[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[2])])
      print(boostTrain[i])
      
      # parsing Variable Importance 
#      if (is.na(b) == FALSE) {
        variableImportance <- C5imp(model)
      
        for (var in model[["predictors"]]) {
          sumVariableImportance[i,var] = variableImportance[var,]
        }
#      }
    }
    orderOfImportance <- sort(colMeans(sumVariableImportance), decreasing = TRUE)
    boostTrain <- as.numeric(sub("%", "", boostTrain[!is.na(boostTrain)]))
    boostTest  <- as.numeric(sub("%", "", boostTest[!is.na(boostTest)]))
    meanBoostTrain <- mean(boostTrain)
    meanBoostTest  <- mean(boostTest)
    
    results <- c(trainError = meanBoostTrain, testError = meanBoostTest, varImp = orderOfImportance) 
    attr(results, "class") <- "samplingC5"
    results
}

# -----------------------------------------------------------------

allPeriodsC5 <- function(inputData, dependentVariable) {
  
  # Variable to be determined by C5.0
  preTarget <- inputData[1:185, dependentVariable]
  capTarget <- inputData[186:362, dependentVariable]
  postTarget <- inputData[363:555, dependentVariable]
  
  # Delete Target variable from input Data
  inputData[,dependentVariable] <- NULL
  
  # Define input Data for C5.0
  preData <- inputData[1:185,]
  capData <- inputData[186:362,]
  postData <- inputData[363:555,]
  
  # Run C5.0 Sampling for different Periods
  preCap <- samplingC5(preData,preTarget)
  cap <- samplingC5(capData, capTarget)
  postCap <- samplingC5(postData, postTarget) 
  
  # Output of Object
  results <- c(PreCap = preCap, Cap = cap, PostCap = postCap) 
  attr(results, "class") <- "allPeriodsC5"
  results
}

# ---------------------------------------------------------------------

outputPrint <- function(output) {
  print(output[c("PreCap.trainError","Cap.trainError","PostCap.trainError")])
  print(output[c("PreCap.testError","Cap.testError","PostCap.testError")])
  print(output[c("PreCap.varImp.ChgSDdomBanks","Cap.varImp.ChgSDdomBanks","PostCap.varImp.ChgSDdomBanks")])
  print(output[c("PreCap.varImp.SDdomBanksdir","Cap.varImp.SDdomBanksdir","PostCap.varImp.SDdomBanksdir")])
  print(output[c("PreCap.varImp.SDofDomBanks","Cap.varImp.SDofDomBanks","PostCap.varImp.SDofDomBanks")])
}

# Functions and Obejcts END ------------------------------------------

## Execution

# Indices

currentSMI   <- allPeriodsC5(allData[currentSMIColumns], "SMIdir")
currentSMIM  <- allPeriodsC5(allData[currentSMIMColumns], "SMIMdir")
currentSPIEX <- allPeriodsC5(allData[currentSPIEXColumns], "SPIEXdir")

forecastSMI   <- allPeriodsC5(allData[forecastSMIColumns], "SMInext")
forecastSMIM  <- allPeriodsC5(allData[forecastSMIMColumns], "SMIMnext")
forecastSPIEX <- allPeriodsC5(allData[forecastSPIEXColumns], "SPIEXnext")

outputPrint(currentSMIM)

# FX Rate

currentFxSMI <- ....


# SMI Stock indice --------------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SMI <- SMIdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SMI <- SMIdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SMI <- SMIdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)

# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 12)
PreCapPeriod.SMI.model <- C5.0(PreCapPeriod.SMI[-12], PreCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
PreCapPeriod.SMI.model
summary(PreCapPeriod.SMI.model) # SDdomBanksdir 9.14%, SDdomBanks 9.14%

# CapPeriod Model describing SMI Forecast (column 12)
CapPeriod.SMI.model <- C5.0(CapPeriod.SMI[-12], CapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
CapPeriod.SMI.model
summary(CapPeriod.SMI.model) # 100% SDdomBanksdir, 100% ChgSDdomBanks, 18.08% SDofDomBanks

# PostCapPeriod Model describing SMI Forecast (column 12)
PostCapPeriod.SMI.model <- C5.0(PostCapPeriod.SMI[-12], PostCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
PostCapPeriod.SMI.model
summary(PostCapPeriod.SMI.model) # 9.91% SDdomBanksdir


# SPI Extra Stock indice --------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SPIEX <- SPIEXdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SPIEX <- SPIEXdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SPIEX <- SPIEXdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)
allDataSPIEX <- SPIEXdata[,-1]
## Test SPIEX PreCap with random Sample
modelSMI <- C5.0(CapPeriod.SMI[-c(7,12)], CapPeriod.SMI$CHFEUR, 
                         rules = TRUE, trials = 10, 
                         control = C5.0Control(sample=0.7))
summary(modelSMI)
predict <- predict(modelPreCapSPIEX, newdata = SMIM.test[,-16])
summary(modelPreCapSPIEX)
summary(predict)
summary(test$SMIM.FC)
accuracySMIMid <- (sum( predict == SMIM.test$SMIM.FC ) / length( predict ))*100 # Calculating the accuracy of the predicitons.
accuracySMIMid
# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SPIEX Forecast (column 12)
PreCapPeriod.SPIEX.model <- C5.0(PreCapPeriod.SPIEX[-12], PreCapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
PreCapPeriod.SPIEX.model
summary(PreCapPeriod.SPIEX.model) # 52.15% ChgSDdomBanks, 17.20% SDdomBanksdir, 2.69% SDofDomBanks

# CapPeriod Model describing SPIEX Forecast (column 12)
CapPeriod.SPIEX.model <- C5.0(CapPeriod.SPIEX[-12], CapPeriod.SPIEX$SPIEX.FC, rules = TRUE)
CapPeriod.SPIEX.model
summary(CapPeriod.SPIEX.model) # 48.59% ChgSDdomBanks, 2.82% SDdomBanksdir

# PostCapPeriod Model describing SPIEX Forecast (column 12)
PostCapPeriod.SPIEX.model <- C5.0(PostCapPeriod.SPIEX[-12], PostCapPeriod.SPIEX$SPIEX.FC,rules = TRUE, trials = 100)
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

# PreCapPeriod Model describing SMI mid Forecast (column 12)
PreCapPeriod.SMIM.model <- C5.0(PreCapPeriod.SMIM[-12], PreCapPeriod.SMIM$SMIM.FC, rules = TRUE)
PreCapPeriod.SMIM.model
summary(PreCapPeriod.SMIM.model) # 100% SDofDomBanks, 19.89% SDdomBanksdir

# CapPeriod Model describing SMI mid Forecast (column 12)
CapPeriod.SMIM.model <- C5.0(CapPeriod.SMIM[-12], CapPeriod.SMIM$SMIM.FC, rules = TRUE, trials = 100)
CapPeriod.SMIM.model
summary(CapPeriod.SMIM.model) # 48.02% SDdomBanksdir, 2.82% ChgSDdomBanks

# PostCapPeriod Model describing SMI mid Forecast (column 12)
PostCapPeriod.SMIM.model <- C5.0(PostCapPeriod.SMIM[-12], PostCapPeriod.SMIM$SMIM.FC,rules = TRUE, trials = 100)
PostCapPeriod.SMIM.model
summary(PostCapPeriod.SMIM.model) # 7.08% ChgSDdomBanks, 7.08% SDdomBanksdir


