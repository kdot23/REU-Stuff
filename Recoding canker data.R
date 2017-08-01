setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(data.table)
cankers = fread("canker_cleaned.csv", header = TRUE)
cankerf = data.frame(cankers)

treated = c(11,12,14,15,21,22,24,25,31,32,34,35)
untreated = c(13,16,23,26,33,36)

#recode 1 = treated, 0 = not treated
#other possibilites of codes are ns = not sampled
for (i in 5:13)
{
  cankerf[,i][cankerf[,i] %in% treated] <- 1
  cankerf[,i][cankerf[,i] %in% untreated] <- 0
  cankerf[,i][cankerf[,i] == "ns" | cankerf[,i] == "n" | cankerf[,i] == "hn"
              | cankerf[,i] == "b" | cankerf[,i] == "N"] <- -99
}
cankerf[,14][cankerf[,14] == 1 | cankerf[,14] == 2] <-1


#add stuff for column 13
for (i in 1:nrow(cankerf))
{
  for (j in 14:5)
  {
    if (!(is.na(cankerf[i,j])) && cankerf[i,j] != -99)
    {
      if (cankerf[i,j] == 1 && j != 5)
      {
        cankerf[i,j-1] = 1
        j = j-1
      }
      else
      {
        j = j-1
      }
    }
    else
      if (j != 14)
      {
        cankerf[i,j] = cankerf[i,j+1]
      }
      else
      {
        cankerf[i,j] = cankerf[i,j-1]
      }
  }
}
#gets rid of 1900 dates
cankerf[,3][cankerf[,3] == 1900] <- "NA"
