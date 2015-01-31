foo = seq(1, 181, by=20)
for (i in foo) {
  test_idx <- seq(i, i+19)
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
  
  print(rse)
}