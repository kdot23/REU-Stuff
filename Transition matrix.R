setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(data.table)
trees = fread("WestSalemRatings.csv")

#Subsets for trees with rating 1 in years with relevant data.
#r1y03 means rating 1 in year 2002. Note data is missing for 2006 and 2012
r1y02 = subset(trees, trees$`2002Rating`==1)
r1y03 = subset(trees, trees$`2003Rating`==1)
r1y04 = subset(trees, trees$`2004Rating`==1)
r1y07 = subset(trees, trees$`2007Rating`==1)
r1y08 = subset(trees, trees$`2008Rating`==1)
r1y09 = subset(trees, trees$`2009Rating`==1)
r1y10 = subset(trees, trees$`2010Rating`==1)
r1y13 = subset(trees, trees$`2013Rating`==1)

#A vector with all the ratings in the year after the trees were rated 1
trans1 = c(r1y02$`2003Rating`,r1y03$`2004Rating`,r1y04$`2005Rating`,
           r1y07$`2008Rating`,r1y08$`2009Rating`,r1y09$`2010Rating`,r1y10$`2011Rating`,r1y13$`2014Rating`)

r2y02 = subset(trees, trees$`2002Rating`==2)
r2y03 = subset(trees, trees$`2003Rating`==2)
r2y04 = subset(trees, trees$`2004Rating`==2)
r2y07 = subset(trees, trees$`2007Rating`==2)
r2y08 = subset(trees, trees$`2008Rating`==2)
r2y09 = subset(trees, trees$`2009Rating`==2)
r2y10 = subset(trees, trees$`2010Rating`==2)
r2y13 = subset(trees, trees$`2013Rating`==2)
trans2 = c(r2y02$`2003Rating`,r2y03$`2004Rating`,r2y04$`2005Rating`,
           r2y07$`2008Rating`,r2y08$`2009Rating`,r2y09$`2010Rating`,r2y10$`2011Rating`,r2y13$`2014Rating`)

r3y02 = subset(trees, trees$`2002Rating`==3)
r3y03 = subset(trees, trees$`2003Rating`==3)
r3y04 = subset(trees, trees$`2004Rating`==3)
r3y07 = subset(trees, trees$`2007Rating`==3)
r3y08 = subset(trees, trees$`2008Rating`==3)
r3y09 = subset(trees, trees$`2009Rating`==3)
r3y10 = subset(trees, trees$`2010Rating`==3)
r3y13 = subset(trees, trees$`2013Rating`==3)
trans3 = c(r3y02$`2003Rating`,r3y03$`2004Rating`,r3y04$`2005Rating`,
           r3y07$`2008Rating`,r3y08$`2009Rating`,r3y09$`2010Rating`, 
           r3y10$`2011Rating`,r3y13$`2014Rating`)

r4y02 = subset(trees, trees$`2002Rating`==4)
r4y03 = subset(trees, trees$`2003Rating`==4)
r4y04 = subset(trees, trees$`2004Rating`==4)
r4y07 = subset(trees, trees$`2007Rating`==4)
r4y08 = subset(trees, trees$`2008Rating`==4)
r4y09 = subset(trees, trees$`2009Rating`==4)
r4y10 = subset(trees, trees$`2010Rating`==4)
r4y13 = subset(trees, trees$`2013Rating`==4)
trans4 = c(r4y02$`2003Rating`,r4y03$`2004Rating`,r4y04$`2005Rating`,
           r4y07$`2008Rating`,r4y08$`2009Rating`,r4y09$`2010Rating`,
           r4y10$`2011Rating`,r4y13$`2014Rating`)

r5y02 = subset(trees, trees$`2002Rating`==5)
r5y03 = subset(trees, trees$`2003Rating`==5)
r5y04 = subset(trees, trees$`2004Rating`==5)
r5y07 = subset(trees, trees$`2007Rating`==5)
r5y08 = subset(trees, trees$`2008Rating`==5)
r5y09 = subset(trees, trees$`2009Rating`==5)
r5y10 = subset(trees, trees$`2010Rating`==5)
r5y13 = subset(trees, trees$`2013Rating`==5)
trans5 = c(r5y02$`2003Rating`,r5y03$`2004Rating`,r5y04$`2005Rating`,
           r5y07$`2008Rating`,r5y08$`2009Rating`,r5y09$`2010Rating`,
           r5y10$`2011Rating`,r5y13$`2014Rating`)

#gtoolscontains smartbind
library(gtools)

#table() gives frequencies of each rating in the vector
#prop.table() turns the frequencies into proportions
#smartbind() binds by column names
mytable = smartbind(prop.table(table(trans1)),prop.table(table(trans2)),
                    prop.table(table(trans3)),prop.table(table(trans4)),
                    prop.table(table(trans5)))
#changes column order
mytable[,c(1,2,3,4,6,5)]


