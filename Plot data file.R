# Titel:        C5.0 Decision Tree Analysis: Effect of SNB Policy on SMI Mid (2011 - 2018)
# Course:       Financial Economics Reserach Seminar
# Institute:    University of St. Gallen
# Authors:      Julian Woessner, Lars Stauffenegger
# Date:         March 2019
# Version:      1.0
# Description:  This File contains the plot of the index and the SD and the CHF/EUR for the research paper
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
#setwd("C:/Users/Lars Stauffenegger/Documents/MBF Unisg/Research Seminar/ResSem19")
setwd("/Users/julianwossner/Desktop/MBF/Vorlesungen_2._Semester/Research_Seminar_Financial_Economics/Daten/ResSem19")
#setwd("C:/Users/LST/Documents/Uni/Research Seminar/ResSem19")

dataind08 <- read.csv2("Data_for_import_Indices_from_08.csv", header = TRUE , sep = ",") #reading in data
dataind08 <- transform(dataind08, Date = as.Date(Date, format = "%d/%m/%Y") ,SMI = as.numeric(as.character(SMI)), SDofDomBanks = as.numeric(as.character(SDofDomBanks)), 
                       SPIEX = as.numeric(as.character(SPIEX)), SMIMid = as.numeric(as.character(SMIMid)), Gov10yr = as.numeric(as.character(Gov10yr)),
                       Gov3yr = as.numeric(as.character(Gov3yr)), Libor3M_CHF = as.numeric(as.character(Libor3M_CHF)), CHFUSD = as.numeric(as.character(CHFUSD)),
                       CHFEUR = as.numeric(as.character(CHFEUR))) # Converting numbers into numeric format and date column to date format


RetSMI <- diff(log(dataind08$SMI))
ChgSD <-  diff(dataind08$SDofDomBanks)/dataind08$SDofDomBanks[-length(dataind08$SDofDomBanks)] # Sight deposits of dom. Banks
CHFEUR <- 1/dataind08$CHFEUR
Date <- dataind08$Date

Data <- dataind08[,-c(2:9)][-1,]
Data$RetSMI <- RetSMI
Data$ChgSD <- ChgSD
Data <- Data[-c(1, 569:576),]

Data$RetSMI[1] <- 100
Data$ChgSD[1] <- 100

for (i in 2:nrow(Data)){
  Data$RetSMI[i] <- (1+Data$RetSMI[i])*Data$RetSMI[i-1]
  Data$ChgSD[i] <- (1+Data$ChgSD[i])*Data$ChgSD[i-1]
}


PlotIndex <- ggplot(data = Data, aes(x = Date)) + geom_line(aes(y = RetSMI, colour = "Swiss Market Index")) + geom_line(aes(y = ChgSD, colour = "Sight Deposits of Domestic Banks"))+
  #(sec.axis = sec_axis(~.), name = "Sight Deposits growth (indexed: 15.02.2008 = 100)") +
  scale_y_log10()+
  scale_colour_manual(values = c("blue", "red"))+ labs(y = "logIndex (15.02.2008 = 100)",
                                                       x = "Date",
                                                       colour = "Legend") + theme(legend.position = c(0.82,0.5))

PlotFX <- ggplot(data=Data, aes(x= Date)) + geom_line(aes(y = 1/CHFEUR, colour = "CHF/EUR Exchange rate"))+ 
  scale_colour_manual(values = c("green"))+ labs(y = "Exchange rate CHF/EUR", x = "Date", colour = "Legend")+ theme(legend.position = c(0.85,0.9))
   

                                                                                                                                                       
pdf("plot_SD_ExchangeRate.pdf", height = 10, width = 10)
PlotIndex
dev.off()



