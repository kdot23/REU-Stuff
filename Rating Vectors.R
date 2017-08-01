untreatedTrees = subset(coolFrame,coolFrame$Treatment == 0)
untreatedSubset = subset(trees, trees$Tree %in% untreatedTrees$`Tree ID`)
treatedSubset = subset(trees, !(trees$Tree %in% untreatedTrees$`Tree ID`))

treesThatLived = subset(trees, trees$`Death Year` == "na")
treesThatDied = subset(trees, trees$`Death Year` != "na")

library(readxl)
trees <- read_excel("~/Documents/REU/Data/K_Trees_Cleaned (Autosaved).xls")

plotRatings <- function(someTrees,title)
{
ratingFrame = data.frame(cbind(as.integer(someTrees$Tree),someTrees$`Lifespan after infection`,someTrees$DBH,
                               someTrees$Plot, someTrees$`2002 RATING`, someTrees$`2003 RATING`, someTrees$`2004 Rating`,
                               someTrees$`2005 Rating`,someTrees$`2007 Rating`, someTrees$`2008 Rating`,
                               someTrees$`2009 Rating`, someTrees$`2010 Rating`, someTrees$`2011 Rating`,
                               someTrees$`2013 Rating`, someTrees$`2014 Rating`, c(rep(0,nrow(someTrees)))))
colnames(ratingFrame) = c("Tree ID", "Lifespan after infection", "DBH", "Plot", "2002Rating",
                          "2003Rating","2004Rating","2005Rating","2007Rating", "2008Rating",
                          "2009Rating", "2010Rating", "2011Rating", "2013Rating", "2014Rating", "Treatment")
ratingFrame[,"2008Rating"][ratingFrame$`2008Rating`=="Dead"] <- 0
for (i in 1: nrow(someTrees))
{
  if (someTrees$`Death Year`[i] != "na")
  {
    k = paste(someTrees$`Death Year`[i],"Rating",sep="")
    ratingFrame[i,k] = 0
  }
}

ratingVectors = list()
for (i in 1: nrow(someTrees))
{
  ratingVector = c()
  if (is.na(as.integer(someTrees$`Infection Year`[i])))
  {
    ratingVector = c()
  }
  else {
    
  if (as.integer(someTrees$`Infection Year`[i] <= 2002))
      {
        year = 2002
      }
  else
  {
    year = as.integer(someTrees$`Infection Year`[i])
  }
  if (is.na(as.integer(someTrees$`Death Year`[i])))
    {
      maxYear = 2014
    }
  else
    {
      maxYear = as.integer(someTrees$`Death Year`[i])
    }
    t = 1
  while (year <= maxYear)
    {
      if (year == 2006 || year == 2012)
      {
        year = year + 1
      }
      else
      {
        k = paste(year,"Rating", sep="")
        newRating = ratingFrame[,k][i]
        ratingVector[t] <- as.numeric(as.character(newRating))
        year = year + 1
        t = t + 1
        
      }
    }
  }
  ratingVectors[[i]] = ratingVector
}

plot(unlist(ratingVectors),type="n",xlim=c(1,max(sapply(ratingVectors,length))),
     xlab = "Years since 2002 (or infection)", ylab = "Rating", main = title)
mapply(lines,ratingVectors,lty=1)

return (ratingVectors)
}
