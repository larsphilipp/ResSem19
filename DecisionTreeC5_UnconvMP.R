# Titel:        C5.0 Decision Tree Analysis: Effect of SNB Policy on SMI Mid (2008 - 2018)
# Course:       Financial Economics Research Seminar
# Institute:    University of St. Gallen
# Authors:      Julian Woessner, Lars Stauffenegger
# Date:         April 2019
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


# Data Import --------------------------------------
# Index Data
dataind04 <- read.csv2("Indices from 04.csv", header = TRUE , sep = ";") #reading in data
dataind04 <- transform(dataind04, Date = as.Date(Date, format = "%Y-%m-%d") ,SNBSD = as.numeric(as.character(SNBSD)), SMI = as.numeric(as.character(SMI)), 
                                       SPIEX = as.numeric(as.character(SPIEX)), SMI.Mid = as.numeric(as.character(SMI.Mid)), CHF.USD = as.numeric(as.character(CHF.USD)), CHF.EUR = as.numeric(as.character(CHF.EUR))) # Converting numbers into numeric format and date column to date format
dataind04 <- dataind04[-c(1:196),]

# SNB Data
dataind08 <- read.csv2("Data_for_import_Indices_from_08.csv", header = TRUE , sep = ",") #reading in data


## Data Cleaning --------------------------------------
# Transforming the columns of the data frame into numeric
dataind08 <- transform(dataind08, Date = as.Date(Date, format = "%d/%m/%Y") ,SMI = as.numeric(as.character(SMI)), SDofDomBanks = as.numeric(as.character(SDofDomBanks)), 
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
allData <-allData[2:568,] 

## List of Colums --------------------------------------------
# Same Week
currentSMIColumns <- c("SDofDomBanks","Gov10yr","Gov3yr","Libor3M_CHF","CHFUSD","CHFEUR","ChgSDdomBanks","SMIdir","SDdomBanksdir","SMIprev")
currentSPIEXColumns <- c("SDofDomBanks","Gov10yr","Gov3yr","Libor3M_CHF","CHFUSD","CHFEUR","ChgSDdomBanks","SPIEXdir","SDdomBanksdir","SPIEXprev")
currentFxSMIColumns   <- c(basicColumnsFX,"RetSMI","SMI","SMIdir","CHFEURprev","CHFEURdir")

# Forecast
forecastSMIColumns <- c("SDofDomBanks","SMI","Gov10yr","Gov3yr","Libor3M_CHF","CHFUSD","CHFEUR","RetSMI","ChgSDdomBanks","SMIdir","SDdomBanksdir","SMInext")
forecastSPIEXColumns <- c("SDofDomBanks","SPIEX","Gov10yr","Gov3yr","Libor3M_CHF","CHFUSD","CHFEUR","RetSPIEX","ChgSDdomBanks","SPIEXdir","SDdomBanksdir","SPIEXnext")


## Data extention & C50 algorithm --------------------------------------
independentVariables <- intervention
targetVariable <- interventionTarget
independentVariables <- noIntervention
targetVariable <- noInterventionTarget


# Functions and Obejcts BEGIN ------------------------------------------

samplingC5 <- function(independentVariables, targetVariable, sampleSize, typeOfImportance) {
    
    sumVariableImportance = setNames(data.frame(matrix(ncol = length(names(independentVariables)), nrow = 1)), names(independentVariables))
    errorTrain = vector() 
    errorTest = vector()
  
    for (i in 1:2000) {
      
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
        
        if (length(boostRow) == 0)
        {
          errorRow <- grep("Errors", output)
          errorTrain[i] <- gsub(".*\\(|\\).*", "", output[(errorRow[1]+2)])
          errorTest[i] <- gsub(".*\\(|\\).*", "", output[(errorRow[2]+2)])
        }
        else
        {
          errorTrain[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[1])])
          errorTest[i] <- gsub(".*\\(|\\).*", "", output[(boostRow[2])])
          
        }

        # parsing Variable Importance 
        variableImportance <- C5imp(model,metric = typeOfImportance) # splits usage
      
        for (var in model[["predictors"]])
        {
          sumVariableImportance[i,var] = variableImportance[var,]
        }
      }
    }
    orderOfImportance <- sort(colMeans(sumVariableImportance, na.rm = TRUE), decreasing = TRUE)
    errorTrain <- as.numeric(sub("%", "", errorTrain[!is.na(errorTrain)]))
    errorTest  <- as.numeric(sub("%", "", errorTest[!is.na(errorTest)]))
    
    meanErrorTrain <- mean(errorTrain)
    meanErrorTest  <- mean(errorTest)
  
    tTestTest <- t.test(errorTest, mu = 50)
    impSDDir  <- sumVariableImportance[,"SDdomBanksdir"][!is.na(sumVariableImportance[,"SDdomBanksdir"])]
    impSD     <- sumVariableImportance[,"SDofDomBanks"][!is.na(sumVariableImportance[,"SDofDomBanks"])]
    
    results <- list(trainError = meanErrorTrain, testError = meanErrorTest, 
                 varImp = orderOfImportance, tTestTestSet = tTestTest["p.value"],
                 ImpSDDir = impSDDir, ImpSD = impSD)
    
    attr(results, "class") <- "samplingC5"
    results
}

# -----------------------------------------------------------------

periodsC5 <- function(inputData, dependentVariable, sampleSize, typeOfImportance) {
  
  # Variable to be determined by C5.0
  preTarget <- inputData[1:185, dependentVariable]
  capTarget <- inputData[186:361, dependentVariable]
  postTarget <- inputData[361:567, dependentVariable]
  
  # Delete Target variable from input Data
  inputData[,dependentVariable] <- NULL
  
  # Define input Data for C5.0
  preData <- inputData[1:185,]
  capData <- inputData[186:361,]
  postData <- inputData[361:567,]
  

  if (sampleSize == 0)
  {
    # Run C5.0
    preCap <- C5.0(preData, preTarget, rules = TRUE, trials = 100, metric=typeOfImportance)
    cap <- C5.0(capData, capTarget, rules = TRUE, trials = 100, metric=typeOfImportance)
    postCap <- C5.0(postData, postTarget, rules = TRUE, trials = 100, metric=typeOfImportance)

    print(summary(preCap))
    print(C5imp(preCap,metric = typeOfImportance))
    print(summary(cap))
    print(C5imp(cap,metric = typeOfImportance))
    print(summary(postCap))
    print(C5imp(postCap,metric = typeOfImportance))
  }
  else
  {
    # Run C5.0 Sampling for different Periods
    preCap <- samplingC5(preData,preTarget, sampleSize, typeOfImportance)
    cap <- samplingC5(capData, capTarget, sampleSize, typeOfImportance)
    postCap <- samplingC5(postData, postTarget, sampleSize, typeOfImportance)
    
    welchPreCapSDDir  <- t.test(preCap$ImpSDDir, cap$ImpSDDir)
    welchPostCapSDDir <- t.test(postCap$ImpSDDir, cap$ImpSDDir)
    welchPreCapSD  <- t.test(preCap$ImpSD, cap$ImpSD)
    welchPostCapSD <- t.test(postCap$ImpSD, cap$ImpSD)
  }
  # Output of Object
  results <- c(preCapTrainError = preCap$trainError, preCapTestError = preCap$testError, preCapTestPValue = preCap$tTestTestSet, preCapVarImp = preCap$varImp,
               capTrainError = cap$trainError, capTestError = cap$testError, capTestPValue = cap$tTestTestSet, capVarImp = cap$varImp,
               postCapTrainError = postCap$trainError, postCapTestError = postCap$testError, postCapTestPValue = postCap$tTestTestSet, postCapVarImp = postCap$varImp,
               WelchPreCapSDDirPValue = welchPreCapSDDir$p.value,
               WelchPostCapSDDirPValue = welchPostCapSDDir$p.value,
               WelchPreCapSDPValue = welchPreCapSD$p.value,
               WelchPostCapSDPValue = welchPostCapSD$p.value)  
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
    
    # Remove Chaange of Side deposits
    intervention[,"ChgSDdomBanks"]   <- NULL
    noIntervention[,"ChgSDdomBanks"] <- NULL
  }
  if (typeOfThreshold == "nominalSD")
  {
    intervention  <- subset(inputData, abs(ChgSDdomBanks*SDofDomBanks/(1+ChgSDdomBanks)) > interventionThreshold)
    noIntervention <- subset(inputData, abs(ChgSDdomBanks*SDofDomBanks/(1+ChgSDdomBanks)) <= interventionThreshold)
    
    # Remove Chaange of Side deposits
    intervention[,"ChgSDdomBanks"]   <- NULL
    noIntervention[,"ChgSDdomBanks"] <- NULL
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
  
  if (sampleSize == 0)
  {
    # Run C5.0
    int <- C5.0(intervention, interventionTarget,
                                  rules = TRUE, trials = 100)
    noInt <- C5.0(noIntervention, noInterventionTarget,
                                  rules = TRUE, trials = 100)
    print(summary(interventionResults))
    print(summary(noInterventionResults))
  }
  else
  {
    # Run C5.0 Sampling
    int   <- samplingC5(intervention,interventionTarget, sampleSize, typeOfImportance)
    noInt <- samplingC5(noIntervention, noInterventionTarget, sampleSize, typeOfImportance)
    
    welchSDDir <- t.test(int$ImpSDDir, noInt$ImpSDDir)
    welchSD    <- t.test(int$ImpSD, noInt$ImpSD)
  }
  
  # Output of Object
  results <- c(IntTrainError = int$trainError, IntTestError = int$testError, IntTestPValue = int$tTestTestSet, IntVarImp = int$varImp,
               NoIntTrainError = noInt$trainError, NoIntTestError = noInt$testError, NoIntTestPValue = noInt$tTestTestSet, NoIntVarImp = noInt$varImp,
               WelchSDDirPValue = welchSDDir$p.value,
               WelchSDPValue = welchSD$p.value) 
  attr(results, "class") <- "interventionC5"
  results
}


### Execution ---------------------------------------------------------------------------------------
sampleSize <- 0.7
sampleSize <- 0
typeOfImportance <- "usage" # splits usage


## SMI
dependentVariable <- "SMIdir"
# Periods
currentSMI   <- periodsC5(allData[currentSMIColumns], dependentVariable, sampleSize, typeOfImportance)
forecastSMI   <- periodsC5(allData[forecastSMIColumns], "SMInext", sampleSize, typeOfImportance)
currentFx   <- periodsC5(allData[currentFxSMIColumns], "CHFEURdir", sampleSize, typeOfImportance)
# Intervention
InterventionCurrentSMI_Fx1.20 <- interventionC5(allData[currentSMIColumns], dependentVariable, 0.8333, sampleSize, "FX", typeOfImportance)
InterventionCurrentSMI_Sd75 <- interventionC5(allData[currentSMIColumns], dependentVariable, 3712, sampleSize, "nominalSD", typeOfImportance)

## SPIEX
dependentVariable <- "SPIEXdir"
# Periods
currentSPIEX <- periodsC5(allData[currentSPIEXColumns], dependentVariable, sampleSize, typeOfImportance)
forecastSPIEX <- periodsC5(allData[forecastSPIEXColumns], "SPIEXnext", sampleSize, typeOfImportance)
# Intervention
InterventionCurrentSPIEX_Fx1.20 <- interventionC5(allData[currentSPIEXColumns], dependentVariable, 0.83, sampleSize, "FX", typeOfImportance)
InterventionCurrentSMI_Sd75 <- interventionC5(allData[currentSPIEXColumns], dependentVariable, 3586, sampleSize, "nominalSD", typeOfImportance)