setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(readxl)
library(data.table)
library(readr)
trees <- read_excel("~/Documents/REU/Data/K_Trees_Cleaned (Autosaved).xls")
cankerf <- read_csv("~/Documents/REU/Data/cankerdata.csv")
oldInfections = subset(trees, as.integer(trees$`Infection Year`) <= 2004)
infections05 = subset(trees,as.integer(trees$`Infection Year`)==2005)
infections06 = subset(trees,as.integer(trees$`Infection Year`)==2006)
infections07 = subset(trees,as.integer(trees$`Infection Year`)==2007)
infections08 = subset(trees,as.integer(trees$`Infection Year`)==2008)

getmode <- function(v) {
  v = v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
coolFrame = data.frame(cbind(as.integer(trees$Tree),trees$`Lifespan after infection`,trees$DBH,
                             trees$Plot, c(rep(0,nrow(trees)))))
colnames(coolFrame) = c("Tree ID","Lifespan after infection","DBH at Infection","Plot","Treatment")
for (i in 1:nrow(trees))
{
  possibleIndexes = which (cankerf$Tree == trees$Tree[i])
  possibleTreatmentCodes = c()
  if (trees$Tree[i] %in% oldInfections$Tree)
  {
    for (j in 1: length(possibleIndexes))
    {
      possibleTreatmentCodes = c(possibleTreatmentCodes,cankerf$CODE2004[possibleIndexes[j]])
    }
  }
  if (trees$Tree[i] %in% infections05$Tree)
  {
    for (j in 1: length(possibleIndexes))
    {
      possibleTreatmentCodes = c(possibleTreatmentCodes,cankerf$CODE2005[possibleIndexes[j]])
    }
  }
  if (trees$Tree[i] %in% infections06$Tree)
  {
    for (j in 1: length(possibleIndexes))
    {
      possibleTreatmentCodes = c(possibleTreatmentCodes,cankerf$CODE2006[possibleIndexes[j]])
    }
  }
  if (trees$Tree[i] %in% infections07$Tree || trees$Tree[i] %in% infections08$Tree)
  {
    for (j in 1: length(possibleIndexes))
    {
      possibleTreatmentCodes = c(possibleTreatmentCodes,cankerf$CODE2007[possibleIndexes[j]])
    }
  }
 
  if (length(possibleTreatmentCodes) != 0)
  {
    treatment = getmode(possibleTreatmentCodes)
  }
  else
  {
    treatment = NA
  }
  
  coolFrame[i,5] = treatment
}
