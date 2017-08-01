#use the "coolFrame from "Mortality by treatments.R" and rating vectors
#from Rating Vectors treated vs. untreated.R"

ratingVectors = plotRatings(trees,"Trees")
#recode coolFrame by stage
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > -1 & coolFrame$`DBH at Infection` <= 10] <- 1
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > 10 & coolFrame$`DBH at Infection` <= 20] <- 2
coolFrame$`DBH at Infection`[coolFrame$`DBH at Infection` > 20] <- 3

newCountTable = matrix(rep(0,18), ncol = 6, nrow = 3)
rownames(newCountTable) = c("Dead", "Virulent", "Hypovirulent")
colnames(newCountTable) = c("Stage 1 or 2  & Virulent","Stage 1 or 2 & Hypovirulent",
                         "Stage 3 & Virulent","Stage 3 & Hypovirulent",
                         "Stage 4 & Virulent", "Stage 4 & Hypovirulent")
for (i in 1: nrow(trees))
{
  if (length(ratingVectors[[i]]) >= 2)
  {
    stage = rating1 = rating2 = -99
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

        
        if (!is.na(stage) && !is.na(rating1) && !is.na(rating2) 
             && stage != -99 && rating1 != -99 && rating2 != -99)
        {
          theCol = 2*(stage-1) + rating1
          theRow = rating2 + 1
          newCountTable[theRow,theCol] = newCountTable[theRow,theCol] + 1
        }
      }
    }  
  }
}

getNewProportionTable <- function(countTable)
{
  probTable = matrix(rep(0,18), ncol = 6, nrow = 3)
  colnames(probTable) = c("Stage 1 or 2  & Virulent","Stage 1 or 2 & Hypovirulent",
                          "Stage 3 & Virulent","Stage 3 & Hypovirulent",
                          "Stage 4 & Virulent", "Stage 4 & Hypovirulent")
  rownames(probTable) = c("Dead", "Virulent", "Hypovirulent")
  
  #returns the table put in proportion form
  for (j in 1:ncol(countTable))
  {
    total = countTable[1,j] + countTable[2,j] + countTable[3,j]
    for (i in 1:nrow(countTable))
    {
      probTable[i,j] = countTable[i,j]/total
    }
  }
  return (probTable)
}

newPropTable = getNewProportionTable(newCountTable)
library(xlsx)
write.xlsx(newPropTable,file = "propDeadVH.xlsx")
