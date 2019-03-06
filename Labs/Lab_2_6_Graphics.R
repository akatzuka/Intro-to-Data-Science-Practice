plot(mtcars[,3],mtcars[,1],pch=16,col = "blue", xlab="Displacement",ylab="Mileage",main="Mileage vs Engine Displacement")
grid(col="grey60")
y = mean(mtcars[,1])
abline(h=y,col="red",lty=2)
datb = mtcars[mtcars$am== 1,]
points(datb[,3],datb[,1],pch=16,col = "green")
