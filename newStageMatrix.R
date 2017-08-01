setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
library(readxl)
disease_free <- read_excel("~/Documents/REU/Data/OldStageMatrices.xlsx", 
                           sheet = "Disease-free")
recovering <- read_excel("~/Documents/REU/Data/OldStageMatrices.xlsx", 
                         sheet = "Recovering")
epidemic <- read_excel("~/Documents/REU/Data/OldStageMatrices.xlsx", 
                       sheet = "Epidemic")

newStageMatrix <- function(oldStageMatrix)
{
oldStageMatrix[is.na(oldStageMatrix)] <- 0
A = as.matrix(oldStageMatrix[1:8,2:9])
V = eigen(A)
W1 = abs(V$vectors[,1])
W1 = W1/sum(W1)

#The probability that a tree in old states 1-5 will survive is:
a = (sum(A[,1])* W1[1] + sum(A[,2])*W1[2] + sum(A[,3])* W1[3] + sum(A[,4]) * W1[4] + 
       sum(A[,5]) * W1[5]) / sum(W1[1:5])
#The probability that a tree in old states 1-5 will grow to old state 6 is:
b = a*A[6,5]*W1[5]

#The probability that a tree in old states 1-5 will survive and remain in 
#old states 1-5 is:
c= a*(1-A[6,5]*W1[5])

A_0 = matrix(0,6,4)
  A_0[1,1] = c
  A_0 [1,2] = sum(A[2:5,6])
  A_0[1,3] = 0
  A_0[1,4] = 0
  
  A_0[2,1] = b
  A_0[2,2] = A[6,6]
  A_0[2,3] = A[6,7]
  A_0[2,4] = A[6,8]
  
  A_0[3,1] = A[7,5]
  A_0[3,2] = A[7,6]
  A_0[3,3] = A[7,7]
  A_0[3,4] = A[7,8]
  
  A_0[4,1] = A[8,5]
  A_0[4,2] = A[8,6]
  A_0[4,3] = A[8,7]
  A_0[4,4] = A[8,8]
  
  A_0[5,1] = 1 - sum(A_0[,1])
  A_0[5,2] = 1 - sum(A_0[,2])
  A_0[5,3] = 1 - sum(A_0[,3])
  A_0[5,4] = 1 - sum(A_0[,4])
  
  A_0[6,1] = 0
  A_0[6,2] = A[1,6]
  A_0[6,3] = A[1,7]
  A_0[6,4] = A[1,8]
  
  
  rownames(A_0) = c("1","2","3","4","dead","reprod")
  "V2 = eigen(A_0)
  W2 = abs(V2$vectors[,1])
  W2 = W2/sum(W2)"
  A_0 = round(A_0,digits=3)
  return (A_0)
} 

A = t(newStageMatrix(disease_free))
B = t(newStageMatrix(recovering))
C = t(newStageMatrix(epidemic))
rownames(A) = c("Healthy-1","Healthy-2","Healthy-3","Healthy-4")
rownames(B) = c("Recovering-1","Recovering-2","Recovering-3","Recovering-4")
rownames(C) = c("Epidemic-1","Epidemic-2","Epidemic-3","Epidemic-4")

D = rbind(A,B,C)

makeCDF <- function(stageMatrix)
{
  cdf = matrix(0,nrow(stageMatrix),ncol(stageMatrix))
  for (i in 1:nrow(stageMatrix))
  {
    #subtracted 1 because the last column is reproduction
    for (j in 1:ncol(stageMatrix) - 1)
    {
      cdf[i,j] = sum(stageMatrix[i,1:j])
    }
    if (cdf[i,ncol(stageMatrix) - 1] != 1.0)
    {
      print(paste("error in row", i))
      cdf[i,ncol(stageMatrix) - 1] = 1.0
    }
  }
  cdf[,ncol(stageMatrix)] = stageMatrix[,ncol(stageMatrix)]
  colnames(cdf) = c("dead","1","2","3","4","reproduction")
  rownames(cdf) = c("Healthy-1","Healthy-2","Healthy-3","Healthy-4",
                    "Recovering-1","Recovering-2","Recovering-3","Recovering-4",
                    "Epidemic-1","Epidemic-2","Epidemic-3","Epidemic-4")
  return(cdf)
}
cdf = makeCDF(D)
D2 = cbind(D[,5], D[,1:4],D[,6])
cdf2 = makeCDF(D2)
write.xlsx(cdf2, file = "Stage CDF2.xlsx")
write.xlsx(cdf, file = "StageMatrixCDF.xlsx")
write.xlsx(D,file= "NewStageMatrixWMortality.xlsx")

