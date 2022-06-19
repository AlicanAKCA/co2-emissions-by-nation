#Author: Alican Akca
#Mail  : alicanakca_eternal@hotmail.com

library("ggplot2")
library("cowplot")
library("reshape2")

prep <- function()
{
  dataset <- read.csv(file = "fossil-fuel-co2-emissions-by-nation.csv",header = TRUE,
                      sep = ",")
  dataset <- data.frame(dataset, check.rows = TRUE)
  header <-  list('Year', 'Country', 'Total', 'Solid Fuel', 'Liquid Fuel',
                  'Gas Fuel', 'Cement', 'Gas Flaring', 'Per Capita', 'Bunker Fuels')
  
  colnames(dataset) <- header
  return(dataset)
}

avgOfPerCapita <- function(lowestYear){
  avg <- c()
  dataset <- prep()
  size <- length(unique(dataset$Country))
  
  for(year in lowestYear:2014){
                               #We limited this to years for which the country had data.
  avg <- append(
      round(sum(dataset$`Per Capita`[which(dataset["Year"] == year)])/size,2),avg)}
  lowestYear
  avg <- data.frame(avgs = rev(avg),sz = rev(size))
         
  return(avg)}                 #The dataset has been given for the years averaged.

perCapita <- function(country){#Emission data per capita began to be 
  dataset <- prep()            #taken after 1949.
  data <-data.frame(
    c1=dataset$`Per Capita`[which(dataset["Country"] == country)][75:150],
    c2=dataset$Year[which(dataset["Country"] == country)][75:150],
    c3=avgOfPerCapita(1865)$avgs[75:150])
  
  xlab<- paste("Year",paste(1949, 2014,sep="-"),sep = " ")
  plot <- ggplot(data)+geom_line(aes(x=1:76,y=c1, color = "TURKEY"))+
    geom_line(aes(x=1:76,y=c3,group =2, color = "Global Average"))+
    ggtitle(paste("Emissions on Per Capita of",country,sep = " " )) +
    xlab(xlab)+ ylab("Per Capita Carbon Emissions (Mmt)")+
    theme(plot.title=element_text(
      color="black", size=9, face="bold",hjust=0.5),
      axis.title.x =element_text(color="black", size=9, face="bold"),
      axis.title.y =element_text(color="black", size=9, face="bold"))
  return(plot + theme_minimal())}