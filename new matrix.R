library(readxl)
disease_free <- read_excel("~/Documents/REU/Data/Stage matrix for disease-free.xlsx")
A = as.matrix(disease_free[1:8,2:9])
V = eigen(A)
W1 = abs(V$vectors[,1])
W1 = W1/sum(W1)

#The probability of survival (1-mortality) * the percentage of trees in that
#stage in a stable state distribution for old stages 1-4
a = sum(A[,1])* W1[1] + sum(A[,2])*W1[2] + sum(A[,3])* W1[3] + sum(A[,4]) * W1[4] + sum(A[,5]) * W1[5]
a = a/sum(W1[1:5])
b= A[6,5] * W1[5]
a = a*(1-b)

#The probability that a tree in old states 1-5 will survive is:
a = (sum(A[,1])* W1[1] + sum(A[,2])*W1[2] + sum(A[,3])* W1[3] + sum(A[,4]) * W1[4] + 
       sum(A[,5]) * W1[5]) / sum(W1[1:5])
#The probability that a tree in old states 1-5 will grow to old state 6 is:
b = a*A[6,5]*W1[5]

#The probability that a tree in old states 1-5 will survive and remain in 
#old states 1-5 is:
c= a*(1-b)

A_0 = matrix(0,4,4)
  A_0[1,1] = a
  A_0 [1,2] = sum(A[1:5,6])
  A_0[1,3] = A[1,7]
  A_0[1,4] = A[1,8]
  
  A_0[2,1] = 
  A_0[2,2] = A[6,6]
  A_0[2,3] = A[6,7]
  A_0[2,4] = A[6,8]
  
  A_0[3,2] = A[7,6]
  A_0[3,3] = A[7,7]
  
  A_0[4,3] = A[8,7]
  A_0[4,4] = A[8,8]
  