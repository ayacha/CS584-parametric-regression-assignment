test_idx <- seq(62, 81)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]



m = dim(trainingSet)[1]
Z = matrix(1,m)

#n
for (i in 1:2) {
  Z <- cbind(Z, trainingSet[1]^i)  
}

theta = solve(t(Z) %*% Z) %*% t(Z) %*% as.matrix(trainingSet[2])


#Fitting model

testYpredictions = theta[1] + theta[2] * testingSet[2] + theta[3] * testingSet[2]^2


#Training error
trainYpredictions = theta[1] + theta[2] * trainingSet[2] + theta[3] * trainingSet[2]^2


#RSE = ∑(yest – yactual)2 / ∑(ymean – yactual)2
mu <- colMeans(trainingSet[2], na.rm =  TRUE)
rse <- mean((trainYpredictions - trainingSet[2])^2) / mean((mu - trainingSet[2])^2) 
print(rse)
#Testing error
mu <- colMeans(testingSet[2], na.rm =  TRUE)
rse <- mean((testYpredictions - testingSet[2])^2) / mean((mu - testingSet[2])^2) 
print(rse)



#Plotting the regression model obtained on top of the test data.
plot(testingSet)

x=seq(0,30,by=1)
y=theta[1] + theta[2] * x + theta[3] * x^2

lines(x,y, type='l')