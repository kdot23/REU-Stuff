library(data.table)
cankerdata = fread("wsREUdata.csv")
View(cankerdata)

#combine rows one and two to make the column names
canker_columnnames =
  paste0(as.character(as.vector(cankerdata[1])),as.character(as.vector(cankerdata[2])))

#set the new names
setnames(cankerdata,names(cankerdata),canker_columnnames)

#remove the first two rows
cankerdata = cankerdata[-c(1,2)]

#remove useless rows at the bottom of the excel sheet
cankerdata = cankerdata[-c((nrow(cankerdata)-29):nrow(cankerdata))]

#get rid of blanks (replace with 0s)
cankerdata['2012Code' != 1,`2012Code`:=0]
#:= means you are preforming a replacement


#names and unique names in cankerdata
length(names(cankerdata))
length(unique(names(cankerdata)))

