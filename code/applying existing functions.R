Mtest_idx <- seq(1, 250)
MtestingSet <- Mdata[Mtest_idx,]
MtrainingSet <- Mdata[-Mtest_idx,]



model = lm(V3 ~ V1 + V2, data = MtestingSet )
#plot(model)

e1 = predict(model, MtrainingSet)
e2 = predict(model, MtestingSet)

Mmu <- colMeans(MtestingSet[2], na.rm =  TRUE)
Mrse <- mean((e2 - MtestingSet[2])^2) / mean((Mmu - MtestingSet[2])^2) 


