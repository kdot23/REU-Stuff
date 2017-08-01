#plot DBH by plot
plot(as.factor(coolFrame$Plot),coolFrame$`DBH at Infection`,xlab = "Plot",
     ylab = "Centimeters", main = "DBH")

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
plot(as.factor(coolFrame$Treatment),coolFrame$`Lifespan after infection`,
     ylab = "Years", main = "Lifespan
     After Infection", names = c("Untreated","Treated"))

#recode coolFrame by treatment type
untreatedCodes = c(13,16,23,26,33,36)
scratchTreatedCodes = c(11,12,21,22,31,32)
punchTreatedCodes = c(14,15,24,25,34,35)
for (i in 1:nrow(coolFrame))
{
  if (coolFrame$Treatment[i] %in% untreatedCodes)
  {
    coolFrame$Treatment[i] = 0
  }
  if (coolFrame$Treatment[i] %in% scratchTreatedCodes)
  {
    coolFrame$Treatment[i] = 1
  }
  if (coolFrame$Treatment[i] %in% punchTreatedCodes)
  {
    coolFrame$Treatment[i] = 2
  }
}
plot(as.factor(coolFrame$Treatment),coolFrame$`Lifespan after infection`,
     ylab = "Years", main = "Lifespan After Infection", names = c("Untreated","Scratch Treated", "Punch Treated"))

#recode by plot area
center = c(2,3,4,5)
front=c(1,6,7,8)
beyondFront = c(9,10,11,12)
for (i in 1:nrow(coolFrame))
{
  if (coolFrame$Plot[i] %in% center)
  {
    coolFrame$Plot[i] = 0
  }
  if (coolFrame$Plot[i] %in% front)
  {
    coolFrame$Plot[i] = 1
  }
  if (coolFrame$Plot[i] %in% beyondFront)
  {
    coolFrame$Plot[i] = 2
  }
}

plot(as.factor(coolFrame$Plot),coolFrame$`Lifespan after infection`,xlab = "Plot",
     ylab = "Years", main = "Lifespan After Infection", names = c("Disease Center",
                                                                  "Front", "Beyond Front"))
plot(as.factor(coolFrame$Plot),coolFrame$`Lifespan after infection`,xlab = "Plot",
     ylab = "Years", main = "Lifespan After Infection")

