install.packages("KRLS") 
library("KRLS")

Mtest_idx <- seq(1, 250)
MtestingSet <- Mdata[Mtest_idx,]
MtrainingSet <- Mdata[-Mtest_idx,]


X = as.matrix(MtrainingSet[,1:2])

G = gausskernel(X=MtrainingSet[,1:2],sigma=.01)
dim(G)
alphaK = solve (G, as.matrix(MtrainingSet[3]))
dim(MtrainingSet[3])

KtrainYpredictions = t(alphaK) %*% X %*% t(X)

Mmu <- colMeans(MtestingSet[2], na.rm =  TRUE)
Mrse <- mean((KtrainYpredictions - MtestingSet[2])^2) / mean((Mmu - MtestingSet[2])^2) 
print(Mrse)
