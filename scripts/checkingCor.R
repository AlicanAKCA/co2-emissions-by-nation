#Author: Alican Akca
#Mail  : alicanakca_eternal@hotmail.com

checkingCor <- function(dataset,var1,var2)
{
  x = dataset$var1[which(dataset["Country"] == "TURKEY")]
  y = dataset$var2[which(dataset["Country"] == "TURKEY")]
  
  data <- data.frame(var1=x,var2=y)
  plot <- ggplot(data, aes(x=var1,y=var2), col = "lightblue") + 
    geom_point() +geom_smooth(formula = y ~ x, method = "lm")+
    ggtitle(paste("Correlation Graph of Turkey's ", var1, " and ", var2, "uses"),
            subtitle = paste("Correlation:", round(cor(x, y), 2)))+
    xlab(var1)+  
    ylab(var2)+
    theme(plot.title=element_text(color="black", size=12, face="bold",hjust=0.5),
          axis.title.x  =element_text(color="black", size=12, face="bold"),
          axis.title.y  =element_text(color="black", size=12, face="bold"))
  return(plot + theme_minimal())
}