#use the "coolFrame from "Mortality by treatments.R" and rating vectors
#from Rating Vectors treated vs. untreated.R"

ratingVectors = plotRatings(trees,"Trees")
#recode coolFrame by stage
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > -1 & coolFrame$`DBH at Infection` <= 10] <- 1
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > 10 & coolFrame$`DBH at Infection` <= 20] <- 2
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > 20] <- 3

#recode coolFrame by treated and untreated
untreatedCodes = c(13,16,23,26,33,36)
treatedCodes = c(11,12,14,15,21,22,24,25,31,32,34,35)
for (i in 1:nrow(coolFrame))
{
  if (coolFrame$Treatment[i] %in% untreatedCodes)
  {
    coolFrame$Treatment[i] = 0
  }
  else {
    if (coolFrame$Treatment[i] %in% treatedCodes)
    {
      coolFrame$Treatment[i] = 1
    }
    else{
      coolFrame$Treatment[i] = NA
    }
  }
}
countTable = matrix(rep(0,48), ncol = 4, nrow = 12)
colnames(countTable) = c("Dead", "Virulent", "Hypovirulent", "Healthy")
rownames(countTable) = c("1/Viru/U","1/Viru/T","1/Hypo/U","1/Hypo/T",
                        "2/Viru/U","2/Viru/T","2/Hypo/U","2/Hypo/T",
                        "3/Viru/U","3/Viru/T","3/Hypo/U","3/Hypo/T")
for (i in 1: nrow(trees))
{
  if (length(ratingVectors[[i]]) >= 2)
  {
  treatment = stage = rating1 = rating2 = -99
  treatment = coolFrame$Treatment[i]
  stage = coolFrame$`DBH at Infection`[i]
  
  for (j in 1: (length(ratingVectors[[i]]) - 1))
  {
    if (!is.na(ratingVectors[[i]][j]) && !is.na(ratingVectors[[i]][j+1]))
    {
      if (ratingVectors[[i]][j] == 3 || ratingVectors[[i]][j] == 4)
      {
        #hypovirulent
        rating1 = 2
      }
      if (ratingVectors[[i]][j] == 1 || ratingVectors[[i]][j] == 2)
      {
        #virulent
        rating1 = 1
      }

      if (ratingVectors[[i]][j+1] == 3 || ratingVectors[[i]][j+1] == 4)
      {
        rating2 = 2
      }
      if (ratingVectors[[i]][j+1] == 1 || ratingVectors[[i]][j+1] == 2)
      {
        rating2 = 1
      }
      if (ratingVectors[[i]][j+1] == 0)
      {
        rating2 = 0
      }
      if (ratingVectors[[i]][j+1] == 5)
      {
        rating2 = 3
      }
    
    if (!is.na(treatment) && !is.na(stage) && !is.na(rating1) && !is.na(rating2) 
        && treatment != -99 && stage != -99 && rating1 != -99 && rating2 != -99)
    {
      theRow = 4*(stage-1) + 2*(rating1-1) + as.integer(treatment) + 1
      theCol = rating2 + 1
      countTable[theRow,theCol] = countTable[theRow,theCol] + 1
    }
    }
  }  
  }
}

getProportionTable <- function(countTable)
{
  probTable = matrix(rep(0,16), ncol = 4, nrow = 4)
  colnames(probTable) = c("Dead", "Virulent", "Hypovirulent", "Healthy")
  rownames(probTable) = c("V-untreated","V-treated","HV-untreated","HV-treated")
  
  vUf = rbind(countTable[1,],countTable[5,],countTable[9,])
  vTf = rbind(countTable[2,],countTable[6,],countTable[10,])
  hvUf = rbind(countTable[3,],countTable[7,],countTable[11,])
  hvTf = rbind(countTable[4,], countTable[8,], countTable[12,])
  
  vU = c(sum(vUf[,1]),sum(vUf[,2]),sum(vUf[,3]),sum(vUf[,4]))
  vT = c(sum(vTf[,1]),sum(vTf[,2]),sum(vTf[,3]),sum(vTf[,4]))
  hvU = c(sum(hvUf[,1]),sum(hvUf[,2]),sum(hvUf[,3]),sum(hvUf[,4]))
  hvT = c(sum(hvTf[,1]),sum(hvTf[,2]),sum(hvTf[,3]),sum(hvTf[,4]))
  
  condensedCounts = rbind(vU,vT,hvU,hvT)
  #returns the table put in proportion form
for (i in 1:nrow(condensedCounts))
{
  total = condensedCounts[i,1] + condensedCounts[i,2] + condensedCounts[i,3] + condensedCounts[i,4]
  for (j in 1:ncol(condensedCounts))
  {
    probTable[i,j] = condensedCounts[i,j]/total
  }
}
  return (probTable)
}

propTable = getProportionTable(countTable)

#a= alive (This table is all about trees that were alive)

a = condensedCounts[,2:3]
colnames(a) = c("Viru","Hypo")

for (i in 1:nrow(a))
{
  total = sum(a[i,])
  a[i,] = a[i,]/total
}

