setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(data.table)
trees = fread("PlotRatingDBH.csv")
trees_split = split(trees,trees$Plot)

years = c(3,5,7,9,11,13,15,17,19,21,23)

perInf = matrix(ncol = 12, nrow = 11)
for (k in 1:12)
{
  
plot1 = as.data.frame(subset(trees,trees$Plot == k))
plot1Infected = c()
plot1Healthy = c()

for (i in 1:length(years))
{
  plot1Infected = c(plot1Infected, nrow(subset(plot1,plot1[,years[i]] >= 1 & plot1[,years[i]] < 5)))
  plot1Healthy = c(plot1Healthy,nrow(subset(plot1,plot1[,years[i]] == 5)))
}

perInf[,k] = plot1Infected/(plot1Infected + plot1Healthy)
}

theYears = c(2002,2003,2004,2005,2007,2008,2009,2010,2011,2013,2014)
matplot(theYears, perInf, type='l', lty = c(rep(1,6),rep(2,6)), 
        xlab='Years', ylab='Proportion of Infected Trees', col= c(rep(1:6,2)), 
        xlim = c(2002,2007))
legend('bottomright', legend=1:12, lty = c(rep(1,6),rep(2,6)), 
       col= c(rep(1:6,2)),y.intersp = 0.75,inset = 0.05, title = "Plot",ncol=2)
