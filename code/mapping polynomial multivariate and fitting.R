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
print(Mrse)
#Testing error
Mmu <- colMeans(MtestingSet[2], na.rm =  TRUE)
Mrse <- mean((MtestYpredictions - MtestingSet[2])^2) / mean((Mmu - MtestingSet[2])^2) 
print(Mrse)


