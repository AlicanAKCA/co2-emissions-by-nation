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

topCountries <- function(n)
  {
  year <- c()
  total <- c()
  country <- c()
  dataset <- prep()
  
  for (cntry in rev(unique(dataset$Country)))
    
  {country <- append(cntry,country)
  
  total <- append(
    as.numeric(sum(max(dataset$Total[which(dataset["Country"] == cntry)]),
                   dataset$`Bunker Fuels`[which(dataset["Country"]==cntry)])),
    total)            #So far, we have collected the data of the selected country.
                      #I want to note that, Bunker Fuels was not added to Total.
  
  year <- append(paste({min(dataset$Year[which(dataset["Country"] == cntry)])},
                       {max(dataset$Year[which(dataset["Country"] == cntry)])},
                       sep="-"),year)}
                      #In this section, we add in which year range the countries                         #have data.
  
  data <- data.frame(coun =country, tot = total, yr=year)
  data <- data[ with(data, order(tot,decreasing = TRUE)),]
  rownames(data) <- NULL
  data$coun <- substring(data$coun, 0, 25)
                      #We have limited country names to 25 characters
  
  data <- data[with(data, order(tot,decreasing = TRUE)),]
  data <- data[1:n,]
  
  bar <- ggplot(data, 
                aes(x = tot, y = reorder(coun, -tot),color = "black")) + 
    geom_col(stat = "identity",fill = "white")+
    ggtitle(paste("Top ", n, " Countries Have Highest Emissions for All Time"))+
    xlab("Carbon Emissions (Mmt)")+  ylab("")+
    geom_text(aes(label=format(tot, big.mark = ",", scientific = FALSE),),
              size = 3, 
              position = position_stack(vjust = 0.9),color = "black", size=12, 
              face="bold")+
    theme(plot.title=element_text(color="black", size=12, face="bold"),
          axis.title.x  =element_text(color="black", size=12, face="bold"),
          legend.position="none")
  
  bar <- bar + theme_minimal()
  
  return(bar)
  }                  #This function gave the 20 countries with the highest values 
                     #and Turkey, which is the reference country.