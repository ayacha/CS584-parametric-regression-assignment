all <- read.table("mvar-set1.dat.txt",skip=5)
x1 <- all$V1
x2 <- all$V2
y <- all$V3
gradient <- function(t1_current, t2_current, t3_current, t4_current, t5_current, t6_current, x1, x2, y, learningRate){
  t1_gradient = 0
  t2_gradient = 0
  t3_gradient = 0
  t4_gradient = 0
  t5_gradient = 0
  t6_gradient = 0
  N = length(x1)
  for (i in 1:N){
    t1_gradient = t1_gradient + 0
    t2_gradient = t2_gradient - (2/N)*(y[i]-((t1_current+t2_current*x1[i]+t3_current*x2[i]+t4_current*x1[i]*x2[i]+t5_current*x1[i]^2+t6_current*x2[i]^2)))*x1[i]
    t3_gradient = t3_gradient -(2/N)*(y[i]-((t1_current+t2_current*x1[i]+t3_current*x2[i]+t4_current*x1[i]*x2[i]+t5_current*x1[i]^2+t6_current*x2[i]^2)))*x2[i]
    t4_gradient = t4_gradient -(2/N)*(y[i]-((t1_current+t2_current*x1[i]+t3_current*x2[i]+t4_current*x1[i]*x2[i]+t5_current*x1[i]^2+t6_current*x2[i]^2)))*x1[i]*x2[i]
    t5_gradient = t5_gradient -(2/N)*(y[i]-((t1_current+t2_current*x1[i]+t3_current*x2[i]+t4_current*x1[i]*x2[i]+t5_current*x1[i]^2+t6_current*x2[i]^2)))*x1[i]^2
    t6_gradient = t6_gradient -(2/N)*(y[i]-((t1_current+t2_current*x1[i]+t3_current*x2[i]+t4_current*x1[i]*x2[i]+t5_current*x1[i]^2+t6_current*x2[i]^2)))*x2[i]^2
  }
  new_t1 = t1_current - (learningRate * t1_gradient)
  new_t2 = t2_current - (learningRate * t2_gradient)
  new_t3 = t3_current - (learningRate * t3_gradient)
  new_t4 = t4_current - (learningRate * t4_gradient)
  new_t5 = t5_current - (learningRate * t5_gradient)
  new_t6 = t6_current - (learningRate * t6_gradient)
  ans = c(new_t1,new_t2,new_t3,new_t4,new_t5,new_t6) 
  return (ans)
}
t1_current = -0.39171671
t2_current = 0.04
t3_current = -0.2
t4_current = -0.05
t5_current = 0.18
t6_current = 0.02
learningRate = 0.001
for (j in 1:1000){
  ans = gradient(t1_current, t2_current, t3_current, t4_current, t5_current, t6_current, x1, x2, y, learningRate)
  t1_current = ans[1]
  t2_current = ans[2]
  t3_current = ans[3]
  t4_current = ans[4]
  t5_current = ans[5]
  t6_current = ans[6]
}

Mmu <- colMeans(MtestingSet[2], na.rm =  TRUE)
Mrse <- mean((ans[1]+ ans[2]*x1 +ans[3]*x2+ans[4]*x1*x2+ans[5]*x1^2+ans[6]*x2^2- y)^2) / mean((Mmu - MtestingSet[2])^2) 


