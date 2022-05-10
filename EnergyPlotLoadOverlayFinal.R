library(readr)
library(ggplot2)
library(viridis)
library(dplyr)
library(hrbrthemes)
library(pdftools)data <- read_csv("EnergyData2.csv")
p <- data2 %>% 
  ggplot( aes(x=Hour, y=Output, fill=`Fuel Type`)) +
  geom_area( ) +
  geom_line(data = data2, aes(x=Hour,y=`Ontario Demand`),color="red",size=0.8) +
  theme(axis.title.y = element_text(angle=90, hjust = 0.5)) +
  scale_x_continuous(n.breaks = 14,labels=c("April 1", "April 2", "April 3", "April 4","April 5","April 6","April 7","April 8","April 9","April 10","April 11", "April 12","April 13","April 14"," ")
  ) +
  theme(axis.text.x=element_text(size = 5, hjust = 0.5)) +
  ggtitle("Energy Output") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="right")
p+scale_fill_manual(values=c("#FFFF00","#90EE90", "#ADD8E6","#020085", "#75003f")) 



RealTimeConstrainedTotals <- pdf_text("RealtimeConstTotals_h2.pdf")
###****Notes on Energy Output****###
###Data for Energy Output comes from the IESO's Generator Output and Capability report, which presents energy output and capability for generating facilities in the IESO-administered Energy market
###Data used is Output, or the actual energy production of the unit or facility per hour each day. To compute the hourly output, a facility's 5-minute output is averaged over the hour.
###Total output of each facility by energy type was summed together to get total output of each energy type for each hour of each day included in the graph. 
###As a result, each output value for each energy type displayed in the graph is a sum of all energy output across all plants for each specific energy type. 
###For more information, visit: https://www.ieso.ca/en/Power-Data/Data-Directory


###****Notes on Market Demand****###
###Demand for Ontario (black line) comes from the IESO's Realtime Constrained Totals Report
###The figure pictured above plots Ontario Demand over Energy output
###Ontario Demand is calculated as: Total Energy + Total Generation Without Offers - Total Exports + Total Off Market +/- Over/Undergeneration
###Total Energy: Total enregy dispatched into the IESO controlled grid, calculated as Ontario Generation plus imports 
###Total Generation Without Offers: Total energy injected into the IESO controlled grid from Generators that have not submitted offers 
###Total Exports: Total Energy dispatcehd outside Ontario from the IESO-controlled grid
###Total Off Market: Off market consists of following transactions: Segregated Mode of Operation, Emergency generation, Simultaneous Activation of Reserve and Inadvertent Interchange 
###Inadvertent Interchange: The difference between the scheduled intertie flow and actual intertie flow
###Over/Under Generation: Total energy resulting from over or under generation in the event of differences when the Dispatch Scheduling and Optimization (DSO) tool is balancing supply and demand. 
###For more information, visit: https://www.ieso.ca/en/Power-Data/Data-Directory
