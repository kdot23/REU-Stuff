#use cankerf from "Recoding canker data.R"
trees = fread("wsratingsanddbh.csv")
treef = data.frame(trees)

#year1 = 2003,2004,2007,2008,2009,2010
year1ratings = c(4,6,10,12,14,16)
year2ratings = c(6,8,12,14,16,18)

year1treatment = c(13,12,9,8,7,6) 

treatedTable <- function(rating)
{
  myTable = matrix(nrow=0,ncol=3)
for (j in 1:length(year1ratings))
{
  for (i in 1:nrow(treef))
  {
    if (!is.na(treef[i,year1ratings[j]]) && treef[i,year1ratings[j]] == rating)
    {
      if (treef[i,1] %in% cankerf$Tree)
      {
        
      #finds the index of the tree in the canker file
      thisIndex = match(treef[i,1],cankerf$Tree)
      #finds the treatment of the tree that year
      thisTreatment = as.integer(cankerf[thisIndex,year1treatment[j]])
      
      temp = c(treef[i,1],thisTreatment,treef[i,year2ratings[j]])
      myTable = rbind(myTable,temp)
      }
    }
  }
}
  myFrame = data.frame(myTable)
  sTreated = subset(myFrame, myFrame[,2]=="1")
  sNotTreated = subset(myFrame, myFrame[,2]=="0")
  props = smartbind(prop.table(table(sTreated[,3])),prop.table(table(sNotTreated[,3])))
  return(props)
}
treatedTable(4)
library(gtools)
lastTable = smartbind(treatedTable(1),treatedTable(2),treatedTable(3),treatedTable(4),treatedTable(5))
rownames(lastTable) = c("Rated 1: Treated", "Rated 1: Untreated","Rated 2: Treated"
                            ,"Rated 2: Untreated","Rated 3: Treated", "Rated 3: Untreated",
                            "Rated 4: Treated", "Rated 4: Untreated", "Rated 5: Treated",
                            "Rated 5: Untreated")
View(lastTable)
library(xlsx)
write.xlsx(lastTable,file="treatmentProbs.xlsx")

barplot(as.matrix(treatedTable(2)),beside = T,
        col=c("lightgreen","darkblue"),xlab="Tree Rating in Year n+1",ylab="Proportion",
        ylim=c(0,0.8),main = "Stage Distribution of Trees Rated 2")
legend("topright",c("Treated","Untreated"),fill=c("lightgreen","darkblue"))

