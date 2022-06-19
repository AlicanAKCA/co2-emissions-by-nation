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

get_lower_tri <- function(cormat)
{
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}

convert2cor <- function()
{
  dataset <- prep()
  dataset <- dataset[order(dataset$Country,decreasing = FALSE),]
  rownames(dataset) <- NULL
  cor4plot <- round(cor(cbind(dataset[1],dataset[3:10])),2)
  cor4plot <- get_lower_tri(cor4plot)
  melted_cor <- melt(cor4plot, na.rm = TRUE)
  df4cor <- data.frame(row=rownames(melted_cor)[row(melted_cor)], 
                       col=colnames(melted_cor)[col(melted_cor)], 
                       corr=c(melted_cor))
  
  ggheatmap <- ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", name ="Correlation Map")+
    theme_minimal()+ 
    theme(axis.text.x= element_text(angle= 45, vjust= 1,size= 12,hjust= 1))+
    coord_fixed()
  
  ggheatmap <- ggheatmap + 
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.major = element_blank(),panel.border = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          legend.justification = c(1, 0),legend.position = c(0.6, 0.7),
          legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", 
                                 title.hjust = 0.5))
  return(ggheatmap)
}