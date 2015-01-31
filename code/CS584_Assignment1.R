#Loading data
setwd("/Users/ayacha/Desktop/")

data<-read.table("svar-set1.dat.txt")

#Plotting data
plot(data)

#Linear model
test_idx <- seq(1, 20)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]

Adata = c(nrow(trainingSet), colSums (trainingSet)[1], 
          colSums (trainingSet)[1], colSums(trainingSet^2)[1] )

A = matrix(Adata, 2)

cc = trainingSet[1]*trainingSet[2]
bbb =  colSums (cc)

bData = c(colSums (trainingSet)[2], bbb)
b = matrix(bData, 2)

theta = solve(A, b)

#Fitting model
testYpredictions = theta[1] + testingSet[1] * theta[2]

#Training error
trainYpredictions = theta[1] + trainingSet[1] * theta[2]

#RSE = ∑(yest – yactual)2 / ∑(ymean – yactual)2
mu <- colMeans(trainingSet[2], na.rm =  TRUE)
rse <- mean((trainYpredictions - trainingSet[2])^2) / mean((mu - trainingSet[2])^2) 

#Testing error
mu <- colMeans(testingSet[2], na.rm =  TRUE)
rse <- mean((testYpredictions - testingSet[2])^2) / mean((mu - testingSet[2])^2) 

#Plotting the regression model you obtain on top of the test data.
plot(testingSet)

x=seq(0,30,by=1)
y=theta[1] + theta[2] * x
lines(x,y, type='l')

#Polynomial model
m = dim(trainingSet)[1]
Z = matrix(1,m)

            #n
for (i in 1:3) {
  Z <- cbind(Z, trainingSet[1]^i)  
}

theta = solve(t(Z) %*% Z) %*% t(Z) %*% as.matrix(trainingSet[2])


#Fitting model
#testYpredictions = t(Z %*% theta - as.matrix(testingSet[2])) %*% 
 #                      (Z %*% theta - as.matrix(testingSet[2]))

testYpredictions = theta[1] + theta[2] * testingSet[2] + theta[3] * testingSet[2]^2 + theta[4] * testingSet[2]^3


#Training error
trainYpredictions = theta[1] + theta[2] * trainingSet[2] + theta[3] * trainingSet[2]^2 + theta[4] * trainingSet[2]^3


#RSE = ∑(yest – yactual)2 / ∑(ymean – yactual)2
mu <- colMeans(trainingSet[2], na.rm =  TRUE)
rse <- mean((trainYpredictions - trainingSet[2])^2) / mean((mu - trainingSet[2])^2) 

#Testing error
mu <- colMeans(testingSet[2], na.rm =  TRUE)
rse <- mean((testYpredictions - testingSet[2])^2) / mean((mu - testingSet[2])^2) 


#Plotting the regression model obtained on top of the test data.
plot(testingSet)

x=seq(0,30,by=1)
y=theta[1] + theta[2] * x + theta[3] * x^2 + theta[4] * x^3

lines(x,y, type='l')

###########
#Reducing Data
###########

foo = seq(10, 50, by=10)
for (i in foo) {
  test_idx <- seq(1, 200*i/100)
  testingSet <- data[test_idx,]
  trainingSet <- data[-test_idx,]
  
  Adata = c(nrow(trainingSet), colSums (trainingSet)[1], 
            colSums (trainingSet)[1], colSums(trainingSet^2)[1] )
  
  A = matrix(Adata, 2)
  
  cc = trainingSet[1]*trainingSet[2]
  bbb =  colSums (cc)
  
  bData = c(colSums (trainingSet)[2], bbb)
  b = matrix(bData, 2)
  
  theta = solve(A, b)
  
  testYpredictions = theta[1] + testingSet[1] * theta[2]
  trainYpredictions = theta[1] + trainingSet[1] * theta[2]
  
  mu <- colMeans(trainingSet[2], na.rm =  TRUE)
  rse <- mean((trainYpredictions - trainingSet[2])^2) / mean((mu - trainingSet[2])^2) 
  
  print(rse)
  
  mu <- colMeans(testingSet[2], na.rm =  TRUE)
  rse <- mean((testYpredictions - testingSet[2])^2) / mean((mu - testingSet[2])^2) 
}




#Multivariate regression
#Loading data and mapping

Mdata<-read.table("mvar-set1.dat.txt")

#Plotting data
plot(Mdata)
install.packages("plot3D")
library(plot3D)

scatter3D(Mdata[1], Mdata[2], Mdata[3])



Mtest_idx <- seq(1, 250)
MtestingSet <- Mdata[Mtest_idx,]
MtrainingSet <- Mdata[-Mtest_idx,]



#Polynomial model
Mm = dim(MtrainingSet)[1]
MZ = matrix(1,Mm)

MZ <- cbind(MZ, MtrainingSet[1])  
MZ <- cbind(MZ, MtrainingSet[2])  
MZ <- cbind(MZ, MtrainingSet[1]*MtrainingSet[2])  
MZ <- cbind(MZ, MtrainingSet[1]^2)
MZ <- cbind(MZ, MtrainingSet[2]^2)

Mtheta = solve(t(MZ) %*% as.matrix(MZ)) %*% t(MZ) %*% as.matrix(MtrainingSet[2])


#Fitting model
#testYpredictions = t(Z %*% theta - as.matrix(testingSet[2])) %*% 
#                      (Z %*% theta - as.matrix(testingSet[2]))

MtestYpredictions = Mtheta[1] + Mtheta[2] * MtestingSet[2] + Mtheta[3] * MtestingSet[2]^2 + Mtheta[4] * MtestingSet[2]^3


#Training error
MtrainYpredictions = t(Mtheta) %*% t(as.matrix(MZ))

#RSE = ∑(yest – yactual)2 / ∑(ymean – yactual)2
Mmu <- colMeans(MtrainingSet[2], na.rm =  TRUE)
Mrse <- mean((MtrainYpredictions - MtrainingSet[2])^2) / mean((Mmu - MtrainingSet[2])^2) 

#Testing error
Mmu <- colMeans(MtestingSet[2], na.rm =  TRUE)
Mrse <- mean((MtestYpredictions - MtestingSet[2])^2) / mean((Mmu - MtestingSet[2])^2) 




#Explicit solution
model = lm(V3 ~ V1 + V2, data = MtestingSet )
#plot(model)

e1 = predict(model, MtrainingSet)
e2 = predict(model, MtestingSet)





#Iterative 
X =MtrainingSet[,1:2]

thetaI = matrix(rexp(1, rate=.1), ncol=2)
fill.matrix = function(expr, nrow=1, ncol=1) {
  matrix(eval(expr, envir=list(x=nrow*ncol)), nrow=nrow, ncol=ncol)
}

thetaI = fill.matrix(rexp(1, rate=.1), nrow=1, ncol=2)
thetaI[1]= 1.3




norm_vec <- function(x) sqrt(sum(x^2))





X =MtrainingSet[,1:2]
thetaI = matrix(rexp(1, rate=.1), ncol=2)
thetaI[1]= 1.3
i=3
while(i < norm_vec(thetaI)) {
  J = 1/2 *norm_vec(thetaI %*% t(X) - t(MtrainingSet[3]))^2  
  thetaI = thetaI - .0001 * (thetaI %*% t(X) - t(MtrainingSet[3])) %*% as.matrix(X)
}



#dim((t(alphaI) %*% t(X) - t(MtrainingSet[3])))
#dim(t(X))
#dim((t(alphaI) %*% t(X) - t(MtrainingSet[3])) %*% as.matrix(X))

uu= MtrainingSet[3]
####ETA: If you want to do it without repeating the 200, you can define a function to do so:  
#  fill.matrix = function(expr, nrow=1, ncol=1) {
#    matrix(eval(expr, envir=list(x=nrow*ncol)), nrow=nrow, ncol=ncol)
#  }
#fill.matrix(rexp(x, rate=.1), nrow=10, ncol=20)


#Kernel Method
#fun <- function (x1, x2) {
#  return ( exp(-1/2 * length((x1 - x2))^2) )
#} 

#sssd <- laply(MtrainingSet, function(x) {
#  
#})

install.packages("KRLS") 
library("KRLS")

X = as.matrix(MtrainingSet[,1:2])

G = gausskernel(X=MtrainingSet[,1:2],sigma=.01)
dim(G)
alphaK = solve (G, as.matrix(MtrainingSet[3]))
dim(MtrainingSet[3])

KtrainYpredictions = t(alphaK) %*% X %*% t(X)
#yyy = t(alphaK) %*% MtrainingSet %*% MtrainingSet










foo = seq(1, 181, by=20)
for (i in foo) {
  train_idx <- seq(i, i+19)
  test <- data[train_idx,]
  train <- data[-train_idx,]
}



