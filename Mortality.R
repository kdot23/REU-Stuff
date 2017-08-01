#uses cankerf from "Recoding canker data.R"
trees = fread("wsratingsanddbh.csv")
treef = data.frame(trees)
#columns corresponding to useable years in treef data frame that have tree ratings
years = c(2,4,6,8,10,12,14,16,18,20,22)
#empty vector for lifespans of trees that are recorded dying
lifespans = c()
dbhs = c()

for (i in 1:nrow(treef))
{
  #finds the index with the matching tree ID
  possibleIndexes = which (cankerf$Tree == treef[i,1])
  #if there are no matches, runs through the treef data file to find if
  #the tree got infected since 2002 and then if/when it died
  if (length(possibleIndexes) == 0 && !is.na(treef$X2002Rating[i]) && treef$X2002Rating[i]==5 || 
      length(possibleIndexes) == 0 && is.na(treef$X2002Rating[i]))
  {
    j = 1
    infectionYear = -1
    dYear = -2
    while (j < length(years) && infectionYear == -1)
    {
      if (!is.na(treef[[years[j]]][i])  && treef[[years[j]]][i] != 5 )
      {
        infectionYear <- switch(j, 2002,2003,2004,2005,2007,2008,2009,2010,2011,2013,2014)
        dbhAtInfection = treef[[years[j]+1]][i]
      }
      else
      {
        j = j + 1
      }
    }
    while (j < length(years) && dYear == -2)
    {
      if (!is.na(treef[[years[j]]][i])  && treef[[years[j]]][i] == 0 )
      {
        dYear <- switch(j, 2002,2003,2004,2005,2007,2008,2009,2010,2011,2013,2014)
      }
      else
      {
        j = j + 1
      }
    }
    if (dYear != -2)
    {
      lifespan = dYear - infectionYear 
      lifespans = c(lifespans,lifespan)
      dbhs = c(dbhs, dbhAtInfection)
    }
  }
  else
  {
  #if there are multiple recorded cankers, it chooses the earliest infection year
  if (length(possibleIndexes) > 1)
  {
    possibleInfectionDates = c()
    for (k in 1: length(possibleIndexes))
    {
      possibleInfectionDates = c(possibleInfectionDates,cankerf[possibleIndexes[k],3])
    }
    infectionYear = min(as.integer(possibleInfectionDates))
  }
    #if there is only one match, it just uses that infection year
  else
  {
    thisIndex = match(treef[i,1],cankerf$Tree)
    infectionYear = cankerf[thisIndex,3]
  }
  #finds the death year for trees with at least one possible infection date  
  j = 1
  dYear = -1
  while (j < length(years) && dYear == -1)
  {
    if (!(is.na(treef[[years[j]]][i])) && treef[[years[j]]][i] == 0)
    {
      dYear = j
    }
    else
    {
      if (!is.na(treef[[years[j]]][i]) && treef[[years[j]]][i] == 1 
          && !is.na(treef[[years[j]+1]][i]) && treef[[years[j]+1]][i] < 2)
      {
          dYear = j
      }
      else
      {
        j = j+ 1
      }
    }
  }
  if (dYear != -1)
  {
    deathYear <- switch(dYear, 2002,2003,2004,2005,2007,2008,2009,2010,2011,2013,2014)
    infectionYear = as.integer(infectionYear)
    lifespan = deathYear - infectionYear
    print(treef$Tree[i])
    print(infectionYear)
    print(lifespan)
    lifespans = c(lifespans,lifespan)
    if (is.na(infectionYear) || infectionYear < 2002)
    {
      dbhAtInfection = NA
    }
    else
    {
      
    if (infectionYear >= 2002 && infectionYear <= 2005)
    {
      dbhAtInfection = treef[[infectionYear-1999]][i]
    }
    else
    {
      if (infectionYear >= 2007 && infectionYear <=2011)
      {
        dbhAtInfection = treef[[infectionYear-1996]][i]
      }
      else 
      {
        if (infectionYear == 2013)
        {
          dbhAtInfection = treef[[19]][i]
        }
        else
        {
          
        if (infectionYear == 2006)
        {
          dbhAtInfection = treef[[11]][i]
        }
        }
      }
    }
    }
    dbhs = c(dbhs,dbhAtInfection)
  }
  }
  }  
