"This calculates the mean number of trees that a source infects. Note:
hypovirulent trees can spread both virulent and hypovirulent fungi)"

DiseaseSpread <- read_excel("~/Documents/REU/Data/DiseaseSpread.xls", 
                            sheet = "All for manipulation")

uniqueSources = unique(DiseaseSpread$`Source No.`)

f = data.frame()

virus = subset(DiseaseSpread,DiseaseSpread$Virus=="Yes")
noVirus = subset(DiseaseSpread,DiseaseSpread$Virus=="No")

for (i in 1: length(uniqueSources))
{
  f[i,1] = uniqueSources[i]
  f[i,2] = sum(virus$`Source No.` == uniqueSources[i])
  f[i,3] = sum(noVirus$`Source No.` == uniqueSources[i])
}

colnames(f) = c("Source No", "# Infections w/ Virus", "# Infections w/out Virus")

#trees that only infected without virus (Note: it is possible that
#these are hypovirulent but only spread the virulent pathogen)
noVirusOnly = subset(f, f$`# Infections w/ Virus` == 0)

#trees that infected at least one other tree with a hypovirulent pathogen
someVirus = subset(f, f$`# Infections w/ Virus` != 0)

mean(noVirusOnly$`# Infections w/out Virus`)
mean(someVirus$`# Infections w/out Virus`)
mean(someVirus$`# Infections w/ Virus`)

