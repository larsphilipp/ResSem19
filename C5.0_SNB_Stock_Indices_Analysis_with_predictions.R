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
dataret <- dataind08[-1,]
dataret <- cbind(dataret, RetSMI, RetSPIEX, RetSMIMid, ChgSDdomBanks) # adding percent changes to the data set

# Inclusions
SMIdata <- dataret[,-c(3,5,12,13)]  # Includes SMI as only stock indice
SPIEXdata <- dataret[,-c(4,5,11,13)] # Includes SPI Extra as only stock indice
SMIMdata <- dataret[, -c(3,4,11,12)] # Includes the SMI Mid as only stock indice


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


# Using C5.0 algorithm
# SMI Stock indice --------------------------------------
# Defining the Periods for the three stock indices


# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SMI <- SMIdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SMI <- SMIdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SMI <- SMIdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)

accuracy.SMI <- function(noSim){
  #
  # Function to calculate the accuracy of the three models over different training and test periods for a number of simulations
  #
  # Args:
  # noSIM  An integer number of simulations
  #
  # Returns:
  # The accuracy of the PreCap, Cap and PostCap model over different sample and test sets
  m.SMI <- matrix(NA,noSim,3)
  colnames(m.SMI) <- c("PreCap", "Cap", "PostCap")
  pb <- txtProgressBar(0, noSim, style = 3)
  seed <- 12
  for (i in 1:noSim){ 
    setTxtProgressBar(pb, i)
  set.seed(seed)
# Training set
smp_size <- floor(0.70*nrow(PreCapPeriod.SMI)) # Sample size
train_rows <- sample(seq_len(nrow(PreCapPeriod.SMI)), size = smp_size) # sampling rows
PreCapPeriod.SMI.training <- PreCapPeriod.SMI[train_rows,]# Training set
PreCapPeriod.SMI.test <- PreCapPeriod.SMI[-train_rows,] # Test set

smp_size <- floor(0.70*nrow(CapPeriod.SMI)) # Sample size
train_rows <- sample(seq_len(nrow(CapPeriod.SMI)), size = smp_size) # sampling rows
CapPeriod.SMI.training <- CapPeriod.SMI[train_rows,]# Training set
CapPeriod.SMI.test <- CapPeriod.SMI[-train_rows,] # Test set

smp_size <- floor(0.70*nrow(PostCapPeriod.SMI)) # Sample size
train_rows <- sample(seq_len(nrow(PostCapPeriod.SMI)), size = smp_size) # sampling rows
PostCapPeriod.SMI.training <- PostCapPeriod.SMI[train_rows,]# Training set
PostCapPeriod.SMI.test <- PostCapPeriod.SMI[-train_rows,] # Test set

# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

# PreCapPeriod Model describing SMI Forecast (column 12)
PreCapPeriod.SMI.model <- C5.0(PreCapPeriod.SMI.training[-12], PreCapPeriod.SMI.training$SMI.FC, rules = TRUE, trials = 100)
SMI.prediction.PreCap <- predict(PreCapPeriod.SMI.model, newdata = PreCapPeriod.SMI.test) # # Predicting values in the dataset for model fitting assessment
accuracy.SMI.PreCap <- sum(SMI.prediction.PreCap == PreCapPeriod.SMI.test$SMI.FC)/length(SMI.prediction.PreCap)*100 

# CapPeriod Model describing SMI Forecast (column 12)
CapPeriod.SMI.model <- C5.0(CapPeriod.SMI.training[-12], CapPeriod.SMI.training$SMI.FC, rules = TRUE, trials = 100)
SMI.prediction.Cap <- predict(CapPeriod.SMI.model, newdata = CapPeriod.SMI.test) # Predicting values in the dataset for model fitting assessment
accuracy.SMI.Cap <- sum(SMI.prediction.Cap==CapPeriod.SMI.test$SMI.FC)/length(SMI.prediction.Cap)*100


# PostCapPeriod Model describing SMI Forecast (column 12)
PostCapPeriod.SMI.model <- C5.0(PostCapPeriod.SMI.training[-12], PostCapPeriod.SMI.training$SMI.FC, rules = TRUE, trials = 100)
SMI.prediction.PostCap <- predict(PostCapPeriod.SMI.model, newdata = PostCapPeriod.SMI.test) # Predicting values in the dataset for model fitting assessment
accuracy.SMI.PostCap <- sum(SMI.prediction.PostCap == PostCapPeriod.SMI.test$SMI.FC)/length(SMI.prediction.PostCap)*100 

  m.SMI[i,1] <- accuracy.SMI.PreCap
  m.SMI[i,2] <- accuracy.SMI.Cap
  m.SMI[i,3] <- accuracy.SMI.PostCap

  seed = seed + 1 # adjusting seed
  }
  close(pb)

  return(m.SMI)
}


# Fitting model for the whole dataset ----------------------------------------------
# PreCap Period
PreCap.model.SMI <- C5.0(PreCapPeriod.SMI[-12], PreCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
summary(PreCap.model.SMI) # SDdomBanksdir 9.14%, SDdomBanks 9.14%
print(xtable(C5imp(PreCap.model.SMI, metric = "splits"),"latex"))

# Cap Period
Cap.model.SMI <- C5.0(CapPeriod.SMI[-12], CapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
summary(Cap.model.SMI) # 100% SDdomBanksdir, 100% ChgSDdomBanks, 18.08% SDofDomBanks
print(xtable(C5imp(Cap.model.SMI, metric = "splits"),"latex"))

# PostCap Period
PostCap.model.SMI <- C5.0(PostCapPeriod.SMI[-12], PostCapPeriod.SMI$SMI.FC, rules = TRUE, trials = 100)
summary(PostCap.model.SMI) # 9.91% SDdomBanksdir
print(xtable(C5imp(PostCap.model.SMI, metric = "splits"),"latex"))





# SPI Extra Stock indice --------------------------------

# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SPIEX <- SPIEXdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SPIEX <- SPIEXdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SPIEX <- SPIEXdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


accuracy.SPIEX <- function(noSim){
  #
  # Function to calculate the accuracy of the three models over different training and test periods for a number of simulations
  #
  # Args:
  # noSIM  An integer number of simulations
  #
  # Returns:
  # The accuracy of the PreCap, Cap and PostCap model over different sample and test sets
  m.SPIEX <- matrix(NA,noSim,3)
  colnames(m.SPIEX) <- c("PreCap", "Cap", "PostCap")
  pb <- txtProgressBar(0, noSim, style = 3)
  seed <- 12
  for (i in 1:noSim){ 
    set.seed(seed)
    setTxtProgressBar(pb, i)
    
    # Training set
    smp_size <- floor(0.70*nrow(PreCapPeriod.SPIEX)) # Sample size
    train_rows <- sample(seq_len(nrow(PreCapPeriod.SPIEX)), size = smp_size) # sampling rows
    PreCapPeriod.SPIEX.training <- PreCapPeriod.SPIEX[train_rows,]# Training set
    PreCapPeriod.SPIEX.test <- PreCapPeriod.SPIEX[-train_rows,] # Test set
    
    smp_size <- floor(0.70*nrow(CapPeriod.SPIEX)) # Sample size
    train_rows <- sample(seq_len(nrow(CapPeriod.SPIEX)), size = smp_size) # sampling rows
    CapPeriod.SPIEX.training <- CapPeriod.SPIEX[train_rows,]# Training set
    CapPeriod.SPIEX.test <- CapPeriod.SPIEX[-train_rows,] # Test set
    
    smp_size <- floor(0.70*nrow(PostCapPeriod.SPIEX)) # Sample size
    train_rows <- sample(seq_len(nrow(PostCapPeriod.SPIEX)), size = smp_size) # sampling rows
    PostCapPeriod.SPIEX.training <- PostCapPeriod.SPIEX[train_rows,]# Training set
    PostCapPeriod.SPIEX.test <- PostCapPeriod.SPIEX[-train_rows,] # Test set
    
    # Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
    # CapPeriod phase to see how much more the SNB variables are used to define rules
    
    # PreCapPeriod Model describing SMI Forecast (column 12)
    PreCapPeriod.SPIEX.model <- C5.0(PreCapPeriod.SPIEX.training[-12], PreCapPeriod.SPIEX.training$SPIEX.FC, rules = TRUE, trials = 100)
    SPIEX.prediction.PreCap <- predict(PreCapPeriod.SPIEX.model, newdata = PreCapPeriod.SPIEX.test) # # Predicting values in the dataset for model fitting assessment
    accuracy.SPIEX.PreCap <- sum(SPIEX.prediction.PreCap == PreCapPeriod.SPIEX.test$SPIEX.FC)/length(SPIEX.prediction.PreCap)*100 
    
    # CapPeriod Model describing SMI Forecast (column 12)
    CapPeriod.SPIEX.model <- C5.0(CapPeriod.SPIEX.training[-12], CapPeriod.SPIEX.training$SPIEX.FC, rules = TRUE, trials = 100)
    SPIEX.prediction.Cap <- predict(CapPeriod.SPIEX.model, newdata = CapPeriod.SPIEX.test) # Predicting values in the dataset for model fitting assessment
    accuracy.SPIEX.Cap <- sum(SPIEX.prediction.Cap==CapPeriod.SPIEX.test$SPIEX.FC)/length(SPIEX.prediction.Cap)*100 
    
    
    # PostCapPeriod Model describing SMI Forecast (column 12)
    PostCapPeriod.SPIEX.model <- C5.0(PostCapPeriod.SPIEX.training[-12], PostCapPeriod.SPIEX.training$SPIEX.FC, rules = TRUE, trials = 100)
    SPIEX.prediction.PostCap <- predict(PostCapPeriod.SPIEX.model, newdata = PostCapPeriod.SPIEX.test) # Predicting values in the dataset for model fitting assessment
    accuracy.SPIEX.PostCap <- sum(SPIEX.prediction.PostCap == PostCapPeriod.SPIEX.test$SPIEX.FC)/length(SPIEX.prediction.PostCap)*100 
    
    m.SPIEX[i,1] <- accuracy.SPIEX.PreCap
    m.SPIEX[i,2] <- accuracy.SPIEX.Cap
    m.SPIEX[i,3] <- accuracy.SPIEX.PostCap
    
    seed <- seed + 1
  }
  close(pb)


  return(m.SPIEX)
  
  }


# Fitting model for the whole dataset ----------------------------------------------
# PreCap Period
PreCap.model.SPIEX <- C5.0(PreCapPeriod.SPIEX[-12], PreCapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
summary(PreCap.model.SPIEX) # ChgSDdomBanks 52.15% SDdomBanksdir 17.20%, SDofDomBanks 2.69%
print(xtable(C5imp(PreCap.model.SPIEX, metric = "splits"),"latex"))
# Cap Period
Cap.model.SPIEX <- C5.0(CapPeriod.SPIEX[-12], CapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
summary(Cap.model.SPIEX) # 48.59%% SDdomBanksdir, 2.82% SDdomBanksdir
print(xtable(C5imp(Cap.model.SPIEX, metric = "splits"),"latex"))

# PostCap Period
PostCap.model.SPIEX <- C5.0(PostCapPeriod.SPIEX[-12], PostCapPeriod.SPIEX$SPIEX.FC, rules = TRUE, trials = 100)
summary(PostCap.model.SPIEX) # no usage of SNB parameters
print(xtable(C5imp(PostCap.model.SPIEX, metric = "splits"),"latex"))




# SMI Mid Stock indice --------------------------------
# Defining the Periods for the three stock indices
# Three Periods: Before, During and Post Cap (row 363 is the 16th of Jan, i.e. 1 day after the removal of the Cap, row 187 is the 2nd september, i.e. 4 days before the introduction of the cap)
PreCapPeriod.SMIM <- SMIMdata[1:186,-1] # Data set from 2008 - 02 - 08 to 2011 - 08 - 26 (PreCapPeriod phase)
CapPeriod.SMIM <- SMIMdata[187:363,-1] # Data set from 2011 - 09 - 02 to 2015 - 01 - 15 (CapPeriod phase)
PostCapPeriod.SMIM <- SMIMdata[364:575,-1] # Data set from 2015 - 01 - 22 to 2019 - 02 - 22 (PostCapPeriod CapPeriod phase)


# Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
# CapPeriod phase to see how much more the SNB variables are used to define rules

accuracy.SMIM <- function(noSim){
  #
  # Function to calculate the accuracy of the three models over different training and test periods for a number of simulations
  #
  # Args:
  # noSIM  An integer number of simulations
  #
  # Returns:
  # The accuracy of the PreCap, Cap and PostCap model over different sample and test sets
  m.SMIM <- matrix(NA,noSim,3)
  colnames(m.SMIM) <- c("PreCap", "Cap", "PostCap")
  pb <- txtProgressBar(0, noSim, style = 3)
  seed <- 12
  for (i in 1:noSim){ 
    set.seed(seed)
    setTxtProgressBar(pb, i)
    
    # Training set
    smp_size <- floor(0.70*nrow(PreCapPeriod.SMIM)) # Sample size
    train_rows <- sample(seq_len(nrow(PreCapPeriod.SMIM)), size = smp_size) # sampling rows
    PreCapPeriod.SMIM.training <- PreCapPeriod.SMIM[train_rows,]# Training set
    PreCapPeriod.SMIM.test <- PreCapPeriod.SMIM[-train_rows,] # Test set
    
    smp_size <- floor(0.70*nrow(CapPeriod.SMIM)) # Sample size
    train_rows <- sample(seq_len(nrow(CapPeriod.SMIM)), size = smp_size) # sampling rows
    CapPeriod.SMIM.training <- CapPeriod.SMIM[train_rows,]# Training set
    CapPeriod.SMIM.test <- CapPeriod.SMIM[-train_rows,] # Test set
    
    smp_size <- floor(0.70*nrow(PostCapPeriod.SMIM)) # Sample size
    train_rows <- sample(seq_len(nrow(PostCapPeriod.SMIM)), size = smp_size) # sampling rows
    PostCapPeriod.SMIM.training <- PostCapPeriod.SMIM[train_rows,]# Training set
    PostCapPeriod.SMIM.test <- PostCapPeriod.SMIM[-train_rows,] # Test set
    
    # Implement the model for the PreCap, CapPeriod phase and the PostCapPeriod 
    # CapPeriod phase to see how much more the SNB variables are used to define rules
    
    # PreCapPeriod Model describing SMI Forecast (column 12)
    PreCapPeriod.SMIM.model <- C5.0(PreCapPeriod.SMIM.training[-12], PreCapPeriod.SMIM.training$SMIM.FC, rules = TRUE, trials = 100)
    SMIM.prediction.PreCap <- predict(PreCapPeriod.SMIM.model, newdata = PreCapPeriod.SMIM.test) # # Predicting values in the dataset for model fitting assessment
    accuracy.SMIM.PreCap <- sum(SMIM.prediction.PreCap == PreCapPeriod.SMIM.test$SMIM.FC)/length(SMIM.prediction.PreCap)*100 
    
    # CapPeriod Model describing SMI Forecast (column 12)
    CapPeriod.SMIM.model <- C5.0(CapPeriod.SMIM.training[-12], CapPeriod.SMIM.training$SMIM.FC, rules = TRUE, trials = 100)
    SMIM.prediction.Cap <- predict(CapPeriod.SMIM.model, newdata = CapPeriod.SMIM.test) # Predicting values in the dataset for model fitting assessment
    accuracy.SMIM.Cap <- sum(SMIM.prediction.Cap==CapPeriod.SMIM.test$SMIM.FC)/length(SMIM.prediction.Cap)*100
    
    
    # PostCapPeriod Model describing SMI Forecast (column 12)
    PostCapPeriod.SMIM.model <- C5.0(PostCapPeriod.SMIM.training[-12], PostCapPeriod.SMIM.training$SMIM.FC, rules = TRUE, trials = 100)
    SMIM.prediction.PostCap <- predict(PostCapPeriod.SMIM.model, newdata = PostCapPeriod.SMIM.test) # Predicting values in the dataset for model fitting assessment
    accuracy.SMIM.PostCap <- sum(SMIM.prediction.PostCap == PostCapPeriod.SMIM.test$SMIM.FC)/length(SMIM.prediction.PostCap)*100 
    
    m.SMIM[i,1] <- accuracy.SMIM.PreCap
    m.SMIM[i,2] <- accuracy.SMIM.Cap
    m.SMIM[i,3] <- accuracy.SMIM.PostCap
    
    seed = seed + 1
  }
  close(pb)

  return(m.SMIM)
  }

# PreCapPeriod Model describing SMI mid Forecast (column 12)
PreCapPeriod.SMIM.model <- C5.0(PreCapPeriod.SMIM[-12], PreCapPeriod.SMIM$SMIM.FC, rules = TRUE)
summary(PreCapPeriod.SMIM.model) # 100% SDofDomBanks, 19.89% SDdomBanksdir
print(xtable(C5imp(PreCapPeriod.SMIM.model, metric = "splits"),"latex"))
# CapPeriod Model describing SMI mid Forecast (column 12)
CapPeriod.SMIM.model <- C5.0(CapPeriod.SMIM[-12], CapPeriod.SMIM$SMIM.FC, rules = TRUE, trials = 100)
summary(Period.SMIM.model) # 48.02% SDdomBanksdir, 2.82% ChgSDdomBanks
print(xtable(C5imp(PreCapPeriod.SMIM.model, metric = "splits"),"latex"))
# PostCapPeriod Model describing SMI mid Forecast (column 12)
PostCapPeriod.SMIM.model <- C5.0(PostCapPeriod.SMIM[-12], PostCapPeriod.SMIM$SMIM.FC,rules = TRUE, trials = 100)
summary(PostCapPeriod.SMIM.model) # 7.08% ChgSDdomBanks, 7.08% SDdomBanksdir
print(xtable(C5imp(PostCapPeriod.SMIM.model, metric = "splits"),"latex"))

# Summarizing the accuracy results for the three models and the three time periods ----------------------------

Accuracy.models <- function(noSim){
  temp <- matrix(NA, 3, 3)
  m.SMI <- accuracy.SMI(noSim)
  m.SPIEX <- accuracy.SPIEX(noSim)
  m.SMIM <- accuracy.SMIM(noSim)
  
  temp[1,] <- apply(m.SMI, 2, mean)
  temp[2,] <- apply(m.SPIEX, 2, mean)
  temp[3,] <- apply(m.SMIM, 2, mean)
  
  colnames(temp) <- c("PreCap", "Cap", "Postcap")
  rownames(temp) <- c("SMI", "SPIEX", "SMIM")
  names(dimnames(temp)) <- list("", "Mean predictions")
  temp
}




# Summary of the accuracy of the three models with 4 moments

# Functions for the models ----------------
# SMI
# Four moments
SMI.moments <- function(noSim){
  m.SMI <- accuracy.SMI(noSim)
  
  temp <- matrix(NA, 4, 3)
  temp[1,] <- apply(m.SMI, 2,FUN = mean)
  temp[2,] <- apply(m.SMI, 2, FUN = sd)
  temp[3,] <- apply(m.SMI, 2, FUN = skew)
  temp[4,] <- apply(m.SMI, 2, FUN = kurtosis)
  
  rownames(temp) <- c("mean", "sd", "skew", "kurt")
  colnames(temp) <- c("PreCap", "Cap", "PostCap")
  names(dimnames(temp)) <- list("", "Moments of the SMI predictions")
  temp
}



# SPIEX
# Four moments
SPIEX.moments <- function(noSim){
  m.SPIEX <- accuracy.SPIEX(noSim)
  
  temp <- matrix(NA, 4, 3)
  temp[1,] <- apply(m.SPIEX, 2,FUN = mean)
  temp[2,] <- apply(m.SPIEX, 2, FUN = sd)
  temp[3,] <- apply(m.SPIEX, 2, FUN = skew)
  temp[4,] <- apply(m.SPIEX, 2, FUN = kurtosis)
  
  rownames(temp) <- c("mean", "sd", "skew", "kurt")
  colnames(temp) <- c("PreCap", "Cap", "PostCap")
  names(dimnames(temp)) <- list("", "Moments of the SPIEX predictions")
  temp
}


# SMIM
# Four moments
SMIM.moments <- function(noSim){
  m.SMIM <- accuracy.SMIM(noSim)
  
  temp <- matrix(NA, 4, 3)
  temp[1,] <- apply(m.SMIM, 2,FUN = mean)
  temp[2,] <- apply(m.SMIM, 2, FUN = sd)
  temp[3,] <- apply(m.SMIM, 2, FUN = skew)
  temp[4,] <- apply(m.SMIM, 2, FUN = kurtosis)
  
  rownames(temp) <- c("mean", "sd", "skew", "kurt")
  colnames(temp) <- c("PreCap", "Cap", "PostCap")
  names(dimnames(temp)) <- list("", "Moments of the SMIM predictions")
  temp
}
  
# Calculation of the four moments for the three indices with mean, sd, skew and kurt and summary of mean predictions

Accuracy.models(1000) # Summary of the mean predictions of for the three models and the three time periods

# Significance testing of the difference between the mean prediction averages --------------------------------

# SMI stock ------------------------------
m.SMI <- accuracy.SMI(100)

SMI1 <- m.SMI[,1]
SMI2 <- m.SMI[,2]
SMI3 <- m.SMI[,3]
# PreCap and Cap period
t.test(SMI1,SMI2)
# Cap and PostCap period
t.test(SMI2, SMI3)

# SPIEX stock ---------------------
m.SPIEX <- accuracy.SPIEX(100)

SPIEX1 <- m.SPIEX[,1]
SPIEX2 <- m.SPIEX[,2]
SPIEX3 <- m.SPIEX[,3]
# PreCap and Cap period
t.test(SPIEX1,SPIEX2)
# Cap and PostCap period
t.test(SPIEX2, SPIEX3)


# SMI stock
m.SMIM <- accuracy.SMIM(100)

SMIM1 <- m.SMIM[,1]
SMIM2 <- m.SMIM[,2]
SMIM3 <- m.SMIM[,3]
# PreCap and Cap period
t.test(SMIM1,SMIM2)
# Cap and PostCap period
t.test(SMIM2, SMIM3)



SMI.moments(100) # Moments of SMI predictions
SPIEX.moments(100) # Moments of SPIEX predictions
SMIM.moments(100) # Moments of SMIM predictions




# Distribution of the prediction accuracies -------------------------

# SMI ------------------------------------
m.SMI <- accuracy.SMI(100) # Capturing all predictions for the SMI for the three periods

plot(density(m.SMI[,1]), main = "Density of the PreCap predictions for the SMI") # Density of PreCap predictions
plot(density(m.SMI[,2]), main = "Density of the PreCap predictions for the SMI") # Density of Cap predictions
plot(density(m.SMI[,3]), main = "Density of the PreCap predictions for the SMI") # Density of PostCap Predictions


# SPIEX ------------------------------------
m.SPIEX <- accuracy.SPIEX(100)  # Capturing all predictions for the SPIEX for the three periods

plot(density(m.SPIEX[,1]), main = "Density of the PreCap predictions for the SPIEX") # Density of PreCap predictions
plot(density(m.SPIEX[,2]), main = "Density of the PreCap predictions for the SPIEX") # Density of Cap predictions
plot(density(m.SPIEX[,3]), main = "Density of the PreCap predictions for the SPIEX") # Density of PostCap Predictions


# SMIM --------------------------------------
m.SMIM <- accuracy.SMIM(100)  # Capturing all predictions for the SMIM for the three periods

plot(density(m.SMIM[,1]), main = "Density of the PreCap predictions for the SMIM") # Density of PreCap predictions
plot(density(m.SMIM[,2]), main = "Density of the PreCap predictions for the SMIM") # Density of Cap predictions
plot(density(m.SMIM[,3]), main = "Density of the PreCap predictions for the SMIM") # Density of PostCap Predictions



