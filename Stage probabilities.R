#The make table function returns a table of probabilities of individual
#trees becoming a particular rating given their current rating and stage

setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(data.table)
treesDBH = fread("DBHandRating.csv")

#column numbers of respective years
year1 = c(3,5,7,11,13,15,17,21)
year1=year1-1
year2 = c(5,7,9,13,15,17,19,23)
year2=year2-1

makeTable <- function(rating)
{
rating1 = matrix(,ncol = 3, nrow=0)
#setnames(rating1,c("Rating in year n+1","DBH in year n", "DBH in year n+1"))

for (j in 1:length(year1))
{
  for (i in 1:nrow(treesDBH))
  {
  if (!is.na(treesDBH[[year1[j]]][i]) && treesDBH[[year1[j]]][i]==rating)
  {
    temp = c(treesDBH[[year2[j]]][i],treesDBH[[year1[j]+1]][i],treesDBH[[year2[j]+1]][i])
    rating1 = rbind(rating1,temp)
  }
  }
}
#recodes data to stages based on DBH
rating1[,2][rating1[,2] > -1 & rating1[,2] <= 1] <- 1
rating1[,2][rating1[,2] > 1 & rating1[,2] <= 10] <- 2
rating1[,2][rating1[,2] > 10 & rating1[,2] <= 20] <- 3
rating1[,2][rating1[,2] > 20] <- 4

rating1[,3][rating1[,3] > -1 & rating1[,3] <= 1] <- 1
rating1[,3][rating1[,3] > 1 & rating1[,3] <= 10] <- 2
rating1[,3][rating1[,3] > 10 & rating1[,3] <= 20] <- 3
rating1[,3][rating1[,3] > 20] <- 4

rating1df = data.frame(rating1)
colnames(rating1df) <- c("Rating in year n+1","Stage in year n", 
                         "Stage in year n+1")
rating1_split = split(rating1df,rating1df[[2]])


library(gtools)
#detailedTable gives a table of probabilities given a particular rating
#for example, if you perform makeTable(2) it will return the probabilities of 
#what rating a tree will be in year n+1 given it had a rating 2 in year n.
detailedTable = data.frame(smartbind(prop.table(table(rating1df$`Rating in year n+1`)),
                           prop.table(table(rating1_split$`1`$`Rating in year n+1`)),
                          prop.table(table(rating1_split$`2`$`Rating in year n+1`)),
                          prop.table(table(rating1_split$`3`$`Rating in year n+1`)),
                          prop.table(table(rating1_split$`4`$`Rating in year n+1`)),
                          prop.table(table(rating1_split$`-1`$`Rating in year n+1`))),
                          row.names = c("Average","Stage 1","Stage 2",
                                        "Stage 3","Stage 4","Dead"))
table(rating1df$`Rating in year n+1`)
#colnames(detailedTable) = c("Rating 0","Rating 1", "Rating 2", "Rating 3",
                           # "Rating 4","Rating 5")
return(detailedTable)
}

