setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(readxl)
trees <- read_excel("~/Documents/REU/Data/K_Trees_Cleaned (Autosaved).xls")
tree_df = data.frame(trees)
trees_with_lifespans = subset(tree_df, !is.na(tree_df$Lifespan.after.infection))
trees_that_lived = subset(tree_df, tree_df$Death.Year == "na")

myFrame = data.frame(cbind(trees$`Lifespan after infection`,trees$DBH))
myFrame_clean = subset(myFrame, myFrame$X1 >=0)
colnames(myFrame_clean) = c("Lifespan after infection","DBH at Infection")

for (i in 1: nrow(trees_with_lifespans))
{
  if (trees_with_lifespans$Lifespan.after.infection[i] == 0)
  {
    infectionYear = as.integer(trees_with_lifespans$Infection.Year[i]) - 1
  }
  else
  {
    infectionYear = as.integer(trees_with_lifespans$Infection.Year[i])
  }
  if (infectionYear < 2003)
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans$DBH[i])
  }
  if (infectionYear == 2003)
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans[i,9])
  }
  if (infectionYear == 2004)
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans[i,11])
  }
  if (infectionYear == 2005 || infectionYear == 2006 )
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans[i,14])
  }
  if (infectionYear == 2007)
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans[i,17])
  }
  if (infectionYear == 2008)
  {
    myFrame_clean[i,2] = as.double(trees_with_lifespans[i,20])
  }
  
}
summary(lm(myFrame_clean$`Lifespan after infection`~as.double(myFrame_clean$`DBH at Infection`)))

myFrame_clean[,2][myFrame_clean[,2] > -1 & myFrame_clean[,2] <= 1] <- 1
myFrame_clean[,2][myFrame_clean[,2] > 1 & myFrame_clean[,2] <= 10] <- 2
myFrame_clean[,2][myFrame_clean[,2] > 10 & myFrame_clean[,2] <= 20] <- 3
myFrame_clean[,2][myFrame_clean[,2] > 20] <- 4

stageFrame = split(myFrame_clean,myFrame_clean[,2])

stageDescriptions = c("<1cm","1-10cm", "10-20cm",">20cm")
boxplot(stageFrame[[1]]$`Lifespan after infection`,stageFrame[[2]]$`Lifespan after infection`,
        stageFrame[[3]]$`Lifespan after infection`,stageFrame[[4]]$`Lifespan after infection`,
        names = stageDescriptions, main = "Lifespan After Infection by DBH",
        xlab = "DBH", ylab="Years",ylim = c(0,20))
#boxplot of DBH in 2002 for trees that died in some point in the study and those
#that were still alive in 2014. 
boxplot(trees_with_lifespans$DBH,trees_that_lived$DBH, names = 
          c("Trees that died","Trees alive in 2014"),ylab = "DBH",
        ylim = c(0,70),main = "Mortality vs. DBH")


