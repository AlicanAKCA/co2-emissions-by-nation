#Author: Alican Akca
#Mail  : alicanakca_eternal@hotmail.com

library("ggplot2")
library("cowplot")
library("reshape2")

prep <- function()
{
  dataset = read.csv(file = "fossil-fuel-co2-emissions-by-nation.csv",header = TRUE,
                     sep = ",")
  dataset = data.frame(dataset, check.rows = TRUE)
  header <-  list('Year', 'Country', 'Total', 'Solid Fuel', 'Liquid Fuel',
                  'Gas Fuel', 'Cement', 'Gas Flaring', 'Per Capita', 'Bunker Fuels')
  
  colnames(dataset) <- header
  return(dataset)
}

lastnyears <- function(cntry,n,year_max){
  dataset <- prep()    #This function provides to specify detail of Countries that
  COUNTRY <- cbind(    #has been choosen in last n years. N is an arbitrary number.
    dataset$`Solid Fuel`[which(dataset["Country"] == cntry)],
    dataset$`Liquid Fuel`[which(dataset["Country"] == cntry)],
    dataset$`Cement`[which(dataset["Country"] == cntry)],
    dataset$`Gas Flaring`[which(dataset["Country"] == cntry)],
    dataset$`Bunker Fuels`[which(dataset["Country"] == cntry)]) 
  
  colnames(COUNTRY) = c("Solid Fuel",
                        "Liquid Fuel",
                        "Cement",
                        "Gas Flaring",
                        "Bunker Fuels")
  
  COUNTRY <- data.frame(COUNTRY)
  COUNTRY <- tail(COUNTRY, n =n)
  #The tail() function provides thi^s.
  xlab     <- paste("Year",paste(year_max-n, year_max,sep="-"),sep = " ")
  
  plot <- ggplot(data=COUNTRY, aes(1:n))  +  
    geom_line(aes(y = Solid.Fuel, color = "Solid Fuel"))+
    geom_line(aes(y = Liquid.Fuel, color = "Liquid Fuel"))+  
    geom_line(aes(y = Cement, color = "Cement"))+
    geom_line(aes(y = Gas.Flaring, color = "Gas Flaring"))+
    geom_line(aes(y = Bunker.Fuels, color = "Bunker Fuels"))+
    ggtitle(paste("Factors Affecting Emissions of",cntry,sep = " ")) +
    xlab(xlab)+ ylab("Carbon Emissions (Mmt)")+
    theme(plot.title=element_text(color="black", size=9, face="bold",hjust=0.5),
          axis.title.x  =element_text(color="black", size=9, face="bold"),
          axis.title.y  =element_text(color="black", size=9, face="bold"))
  
  return(plot+ theme_minimal())
}

detailedUses <- function(cntry,n)
{
  #Taking Average
  avg <- c()
  dataset <- prep()
  min_year <- min(dataset$Year[which(dataset["Country"] == country)])
  size <- length(unique(dataset$Country))
  #We prepared the data to compare the total use of the country                      #to be compared with the average of all remaining countries.                       
  for(year in min_year:2014){
    #We limited this to years for which the country had data.
    avg <- append(
      round(sum(dataset$Total[which(dataset["Year"] == year)])/size,2),avg)}
  
  avg <- data.frame(avgs = rev(avg),sz = rev(size))
  
  total <- dataset$Total[which(dataset["Country"] == cntry)]
  #Two insignificant data have been found for Germany.
  if(cntry=="GERMANY"){ 
    total  <- total[total!=492]
    total  <- total[total!=353]}
  
  year_max <- max(dataset$Year[which(dataset["Country"] == cntry)])
  year_min <- min(dataset$Year[which(dataset["Country"] == cntry)])
  xlab     <- paste("Year",paste(year_min, year_max,sep="-"),sep = " ")
  #Existing years have been kept for visualization.
  plot <- ggplot(data=data.frame(total), aes(x=1:length(total),y=total))+
    ggtitle(paste("Total CO2 Emissions Each Year Graph for",cntry,sep = " ")) +
    xlab(xlab)+  
    ylab("Carbon Emissions (Mmt)")+
    theme(plot.title=element_text(color="black", size=9, face="bold",hjust=0.5),
          axis.title.x  =element_text(color="black", size=9, face="bold"),
          axis.title.y  =element_text(color="black", size=9, face="bold"))
  
  lbl4legend <- substr(cntry,1,3)
  
  plot <- plot +     #Graph one. It's a total use of country has been choosen.
    geom_line(aes(y=total,group =1,color = lbl4legend))
  plot <- plot +     #Graph two. It's an average of the rest of the countries.
    geom_line(data=avgOfTotal(cntry)[1:length(total),], 
              aes(x=1:length(total),
                  y=avgs,group =2,
                  color = "Global Average"))
  #Two graph will be combined. Look at below for second graph.
  plot <- plot_grid(plot, lastnyears(cntry,n,year_max), nrow = 2, labels = "AUTO")
  return(plot)
}