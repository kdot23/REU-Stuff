"This creates exponetial graphs for the distances virulent and hypovirulent
fungi travel to infect trees. Note: this is more accurate using a logarthmic 
trendline on the cdf curve (I did this in excel)"

spread<- read_excel("~/Documents/REU/Data/spread distance data.xlsx", 
                                   sheet = "Sheet3")
spread = spread[1:23,1:3]
spread$Dist = as.numeric(spread$`Distance class`)
offset = 0.1
spread$Vly = log(spread$Virulent + offset)
m.1 = lm(spread$Vly ~ spread$Dist)
summary(m.1)

plot(spread$Dist,spread$Virulent)
lines(spread$Dist, exp(2.39 -0.206*spread$Dist))

plot(spread$Dist,spread$Vly,xlab="Distance Class",ylab="log(Frequency+offset)")
abline(a=2.3987,b=-0.2061,col="red")

#Equation is e^(a + bx) --> e^(2.39 - .206x)

spread$Hly = log(spread$Hypoirulent + 1)
m.2 = lm(spread$Hly ~ spread$Dist)
summary(m.2)
plot(spread$Dist,spread$Hypoirulent)
lines(spread$Dist, exp(0.7203 - 0.1333*spread$Dist))
plot(spread$Dist,spread$Hly,xlab="Distance Class",ylab="log(Frequency+offset)")
abline(a=0.7203,b=-0.1333,col="red")
