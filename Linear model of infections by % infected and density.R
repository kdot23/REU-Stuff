setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(data.table)
trees = fread("PlotRatingDBH.csv")
trees_split = split(trees,trees$Plot)
density=fread("plotDensity.csv")

year1 = c(3,5,7,11,13,15,17,21)
year2 = year1 + 2


df = data.frame()
l=0
for (k in 1:length(year1))
{
  
for (i in 1:12)
{
  l = l + 1
  temp = as.data.frame(subset(trees,trees$Plot == i))
  infected = subset(temp,temp[,year1[k]] >= 1 & temp[,year1[k]] < 5)
  healthy = subset(temp,temp[,year1[k]] == 5)
  percentInfected = nrow(healthy)/ (nrow(infected) + nrow(healthy))
  
  densityIndex = match(trees_split[[i]][1,Plot],density$plot)
  plotDensity = density[densityIndex,3] /(pi * density[densityIndex,2])
  
  healthyToInfected = subset(healthy,healthy[,year2[k]] != 5)
  perHealthyToInfected = nrow(healthyToInfected)/nrow(healthy)
  
  #For some unknown reason populating each cell of a data frame is better
  #than rbinding a vector. rbinding caused problems when creating
  #the linear model
  df[l,1] = percentInfected
  df[l,2] = plotDensity
  df[l,3] = perHealthyToInfected
}
}

colnames(df) <- c("Percent Infected","Plot Density","% of Healthy Trees 
                  Becoming Infected")
fit = lm(`% of Healthy Trees 
                  Becoming Infected`~`Percent Infected`+`Plot Density`,data=df)
#0.03910108 is the r squared of the model
