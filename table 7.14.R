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
        rating1 = 2
      }
      if (ratingVectors[[i]][j] == 1 || ratingVectors[[i]][j] == 2)
      {
        rating1 = 1
      }
      if (ratingVectors[[i]][j] == 0)
      {
        rating1 = 0
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
      if (rating2 == 3)
      {
        print(paste("Tree",i,"Year",j,sep = " "))
      }
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
  probTable = matrix(rep(0,36), ncol = 4, nrow = 12)
  colnames(probTable) = c("Dead", "Virulent", "Hypovirulent", "Healthy")
  rownames(probTable) = c("1/Viru/U","1/Viru/T","1/Hypo/U","1/Hypo/T",
                          "2/Viru/U","2/Viru/T","2/Hypo/U","2/Hypo/T",
                          "3/Viru/U","3/Viru/T","3/Hypo/U","3/Hypo/T")

  #returns the table put in proportion form
for (i in 1:nrow(countTable))
{
  total = countTable[i,1] + countTable[i,2] + countTable[i,3] + countTable[i,4]
  for (j in 1:ncol(countTable))
  {
    probTable[i,j] = countTable[i,j]/total
  }
}
  return (probTable)
}

propTable = getProportionTable(countTable)
