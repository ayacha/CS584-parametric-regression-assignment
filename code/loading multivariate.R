Mdata<-read.table("mvar-set1.dat.txt")

#Plotting data
install.packages("plot3D")
library(plot3D)

scatter3D(Mdata[1], Mdata[2], Mdata[3])

