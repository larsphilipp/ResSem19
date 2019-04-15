# Titel:        C5.0 Decision Tree Analysis: Effect of SNB Policy on SMI Mid (2011 - 2018)
# Course:       Financial Economics Reserach Seminar
# Institute:    University of St. Gallen
# Authors:      Julian Woessner, Lars Stauffenegger
# Date:         March 2019
# Version:      5.2
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
RetCHFEUR <- diff(log(dataind08$CHFEUR)) # Weekly returns of the SMIMid

# SNB Data
ChgSDdomBanks <- diff(dataind08$SDofDomBanks)/dataind08$SDofDomBanks[-length(dataind08$SDofDomBanks)] # Sight deposits of dom. Banks

# Creating Returns data set
dataret <- dataind08[-1,]
dataret <- cbind(dataret, RetSMI, RetSPIEX, ChgSDdomBanks, RetCHFEUR) # adding percent changes to the data set
CHFEURdata <- dataret # including SMI and SMIM

## Data --------------------------------------
allData <- dataret

# Directions: Up / Down
allData$SMIdir <- as.factor(ifelse(allData$RetSMI > 0, "up", ifelse(allData$RetSMI < 0, "down", "no change" ))) # Including a column with up, down factors
allData$SPIEXdir <- as.factor(ifelse(allData$RetSPIEX > 0, "up", ifelse(allData$RetSPIEX < 0, "down", "no change" ))) # Including a column with up, down factors
allData$SDdomBanksdir <- as.factor(ifelse(allData$ChgSDdomBanks > 0, "up", ifelse(allData$ChgSDdomBanks < 0, "down", "no change" )))
allData$CHFEURdir <- as.factor(ifelse(allData$RetCHFEUR > 0, "up", ifelse(allData$RetCHFEUR < 0, "down", "no change" ))) # Including a column with up, down factors

# Previous Week
allData$CHFEURprev <- allData$CHFEUR
allData$CHFEURprev[2:575] <- allData$CHFEURprev[1:574]
allData$SMIprev <- allData$SMI
allData$SMIprev[2:575] <- allData$SMIprev[1:574]
allData$SPIEXprev <- allData$SPIEX
allData$SPIEXprev[2:575] <- allData$SPIEXprev[1:574]

# Next Week & Lag the series to produce forecasts
allData$SMInext <- allData$SMIdir
allData$SMInext[1:574] <- allData$SMInext[2:575]
allData$SPIEXnext <- allData$SPIEXdir
allData$SPIEXnext[1:574] <- allData$SPIEXnext[2:575]
allData$CHFEURnext <- allData$CHFEURdir
allData$CHFEURnext[1:574] <- allData$CHFEURdir[2:575] # CHFEURdata$CHFEURdir[1:574]

# delete first row and the last rows
allData <-allData[2:556,] 

## List of Colums --------------------------------------------
basicColumnsIndex <- c("SDofDomBanks","ChgSDdomBanks","SDdomBanksdir","CHFUSD","RetCHFEUR","CHFEUR","CHFEURdir","Gov3yr","Gov10yr","Libor3M_CHF")

# Same Week
currentSMIColumns   <- c(basicColumnsIndex,"SMIprev","SMIdir")
currentSPIEXColumns <- c(basicColumnsIndex,"SPIEXprev","SPIEXdir")

# Forecast
forecastSMIColumns   <- c(basicColumnsIndex,"SMI","SMInext")
forecastSPIEXColumns <- c(basicColumnsIndex,"SPIEX","SPIEXnext")

## Data extention & C50 algorithm --------------------------------------

# Functions and Obejcts BEGIN ------------------------------------------

samplingC5 <- function(independentVariables, targetVariable, sampleSize, typeOfImportance) {
    
    sumVariableImportance = setNames(data.frame(matrix(ncol = length(names(independentVariables)), nrow = 1)), names(independentVariables))
    boostTrain = vector() 
    boostTest = vector()
  
    for (i in 1:100) {
      
      model <- C5.0(independentVariables, targetVariable,
                    rules = TRUE, trials = 100, 
                    control = C5.0Control(sample=sampleSize))
      #summary(model)
      
      # Only count model if attributes have been used (zero classifications don't count)
      usageIndex <- grep("Attribute usage:", model$output, 
                         fixed = TRUE)
      if (length(usageIndex) != 0)
      {
        # parsing Boost Error Value
        output <- strsplit(model[["output"]], "\n")[[1]]
        boostRow <- grep("^boost\t", output)
        boostTrain[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[1])])
        boostTest[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[2])])
        #print(boostTest[i])
      
        # parsing Variable Importance 
        variableImportance <- C5imp(model,metric = typeOfImportance) # splits usage
      
        for (var in model[["predictors"]])
        {
          sumVariableImportance[i,var] = variableImportance[var,]
        }
      }
    }
    orderOfImportance <- sort(colMeans(sumVariableImportance, na.rm = TRUE), decreasing = TRUE)
    boostTrain <- as.numeric(sub("%", "", boostTrain[!is.na(boostTrain)]))
    boostTest  <- as.numeric(sub("%", "", boostTest[!is.na(boostTest)]))
    
    meanBoostTrain <- mean(boostTrain)
    meanBoostTest  <- mean(boostTest)
  
    tTestTest <- t.test(boostTest, mu = 50)
    
    results <- c(trainError = meanBoostTrain, testError = meanBoostTest, varImp = orderOfImportance, tTestTestSet = tTestTest["p.value"]) # arr <- boostTest
    attr(results, "class") <- "samplingC5"
    results
}

# -----------------------------------------------------------------

periodsC5 <- function(inputData, dependentVariable, sampleSize, typeOfImportance) {
  
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
  

  if (sampleSize == 0)
  {
    # Run C5.0
    preCap <- C5.0(preData, preTarget, rules = TRUE, trials = 100)
    cap <- C5.0(capData, capTarget, rules = TRUE, trials = 100)
    postCap <- C5.0(postData, postTarget, rules = TRUE, trials = 100)

    print(summary(preCap))
    print(summary(cap))
    print(summary(postCap))
  }
  else
  {
    # Run C5.0 Sampling for different Periods
    preCap <- samplingC5(preData,preTarget, sampleSize, typeOfImportance)
    cap <- samplingC5(capData, capTarget, sampleSize, typeOfImportance)
    postCap <- samplingC5(postData, postTarget, sampleSize, typeOfImportance) 
  }
  # Output of Object
  results <- c(PreCap = preCap, Cap = cap, PostCap = postCap) 
  attr(results, "class") <- "allPeriodsC5"
  results
}

# -----------------------------------------------------------------

interventionC5 <- function(inputData, dependentVariable, interventionThreshold, sampleSize, typeOfThreshold, typeOfImportance){
  
  # Data set for Intervention & no Intervention weeks
  if (typeOfThreshold == "relativeSD")
  {
  intervention  <- subset(inputData, abs(ChgSDdomBanks) > interventionThreshold)
  noIntervention <- subset(inputData, abs(ChgSDdomBanks) <= interventionThreshold)
  }
  if (typeOfThreshold == "nominalSD")
  {
    intervention  <- subset(inputData, abs(ChgSDdomBanks*SDofDomBanks/(1+ChgSDdomBanks)) > interventionThreshold)
    noIntervention <- subset(inputData, abs(ChgSDdomBanks*SDofDomBanks/(1+ChgSDdomBanks)) <= interventionThreshold)
  }
  if (typeOfThreshold == "FX")
  {
    if (dependentVariable == "CHFEURdir")
    {
      intervention  <- subset(inputData, CHFEURprev > interventionThreshold)
      noIntervention <- subset(inputData, CHFEURprev <= interventionThreshold)
    }
    else
    {
      intervention  <- subset(inputData, CHFEUR > interventionThreshold)
      noIntervention <- subset(inputData, CHFEUR <= interventionThreshold)
    }
  }
  
  # Variable to be determined by C5.0
  interventionTarget <- intervention[,dependentVariable]
  noInterventionTarget <- noIntervention[,dependentVariable]
  
  # Remove target variable from input set 
  intervention[,dependentVariable]   <- NULL
  noIntervention[,dependentVariable] <- NULL
  
  # Remove Chaange of Side deposits
  intervention[,"ChgSDdomBanks"]   <- NULL
  noIntervention[,"ChgSDdomBanks"] <- NULL
  
  if (sampleSize == 0)
  {
    # Run C5.0
    interventionResults <- C5.0(intervention, interventionTarget,
                                  rules = TRUE, trials = 100)
    noInterventionResults <- C5.0(noIntervention, noInterventionTarget,
                                  rules = TRUE, trials = 100)
    print(summary(interventionResults))
    print(summary(noInterventionResults))
  }
  else
  {
    # Run C5.0 Sampling
    interventionResults   <- samplingC5(intervention,interventionTarget, sampleSize, typeOfImportance)
    noInterventionResults <- samplingC5(noIntervention, noInterventionTarget, sampleSize, typeOfImportance)
  }
  
  # Output of Object
  results <- c(Int = interventionResults, NoInt = noInterventionResults) 
  attr(results, "class") <- "interventionC5"
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

# -----------------------------------------------------------------

interventionOutputPrint <- function(output) {
  print(output[c("Int.trainError","NoInt.trainError")])
  print(output[c("Int.testError","NoInt.testError")])
  print(output[c("Int.tTestTestSet.p.value","NoInt.tTestTestSet.p.value")])
  print(output[c("Int.varImp.ChgSDdomBanks","NoInt.varImp.ChgSDdomBanks")])
  print(output[c("Int.varImp.SDdomBanksdir","NoInt.varImp.SDdomBanksdir")])
  print(output[c("Int.varImp.SDofDomBanks","NoInt.varImp.SDofDomBanks")])
}

### Execution ---------------------------------------------------------------------------------------
sampleSize <- 0.7
sampleSize <- 0
typeOfImportance <- "splits" # splits usage

## SMI
dependentVariable <- "SMIdir"
# Periods
currentSMI   <- periodsC5(allData[currentSMIColumns], dependentVariable, sampleSize, typeOfImportance)
§forecastSMI   <- periodsC5(allData[forecastSMIColumns], "SMInext", sampleSize, typeOfImportance)
# Intervention
InterventionCurrentSMI_Fx1.20 <- interventionC5(allData[currentSMIColumns], dependentVariable, 0.8333, sampleSize, "FX", typeOfImportance)
InterventionCurrentSMI_Sd75 <- interventionC5(allData[currentSMIColumns], dependentVariable, 3586, sampleSize, "nominalSD", typeOfImportance)

## SPIEX
dependentVariable <- "SPIEXdir"
# Periods
currentSPIEX <- allPeriodsC5(allData[currentSPIEXColumns], dependentVariable, sampleSize, typeOfImportance)
forecastSPIEX <- allPeriodsC5(allData[forecastSPIEXColumns], "SPIEXnext", sampleSize, typeOfImportance)
# Intervention
InterventionCurrentSPIEX_Fx1.20 <- interventionC5(allData[currentSPIEXColumns], dependentVariable, 0.83, sampleSize, "FX", typeOfImportance)
InterventionCurrentSMI_Sd75 <- interventionC5(allData[currentSPIEXColumns], dependentVariable, 3586, sampleSize, "nominalSD", typeOfImportance)


## Plotting and descriptive statistics ---------------------------------------------------------------------------------------
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
