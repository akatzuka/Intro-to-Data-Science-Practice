source("lin-regr-util.R")
dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/heart.csv")

# turn the categorical variables into factors
dat$output = factor(dat$output, labels=c("ok", "disease"))
dat$sugar  = factor(dat$sugar, labels=c("low", "high"))
dat$chestpain = factor(dat$chestpain, labels=c("typ ang", "atyp ang", "non-ang", "asympt"))
dat$sex = factor(dat$sex, labels=c("female", "male"))
dat$fluor = factor(dat$fluor, labels=c("0", "1", "2", "3"))

library(e1071)

set.seed(123)
split = split_data(dat)
tr_dat = split[[1]]
te_dat = split[[2]]

fit = naiveBayes(output ~ age + maxhr, data = tr_dat)

predicted = predict(fit, te_dat)
actual = te_dat$output
con = table(actual, predicted)
con
mean(actual == predicted)

plot(density(tr_dat[tr_dat$output == "disease",]$age), col="#FF88FF", main = "Double Density Plot of the Output", lwd = 2, xlab = "Naive Bayes Output")
lines(density(tr_dat[tr_dat$output == "ok",]$age), col="#88FF88", lwd = 2)
abline(v = 1, untf = FALSE, col="888888", lty = 2)
abline(v = 2, untf = FALSE, col="888888", lty = 2)
legend("topright", legend = c("Heart Disease", "No Heart Disease"), col = c("#FF88FF", "#88FF88"), pch = 19)

fit$table$age

x = 10:90
y1 = dnorm(x, mean = fit$table$age[1,1], sd = fit$table$age[1,2])
y2 = dnorm(x, mean = fit$table$age[2,1], sd = fit$table$age[2,2])
plot(x,y2, col = "#FF88FF", main = "Double Density Plot of the Normal Distribution", type = "l", lwd = 2, xlab = "Age")
lines(x,y1, col="#88FF88", lwd = 2)


# plot(density(predicted[actual == "disease"]), col="#FF88FF", main = "Double Density Plot of the Output", ylim = c(0,2), lwd = 2, xlab = "Age")
# lines(density(predicted[actual == "ok"]), col="#88FF88", lwd = 2)
# abline(v = fit$tables$age[1,1], untf = FALSE, col="888888", lty = 2)
# legend("topleft", legend = c("Heart Disease", "No Heart Disease"), col = c("#FF88FF", "#88FF88"), pch = 19)

plot(density(tr_dat[tr_dat$output == "ok",]$age), col="#FF88FF", main = "Density Plot of Age Based on Training Data", lwd = 2, xlab = "Age")
abline(v=50, untf = FALSE, col="888888", lty = 2)
dens_age_ok = density(tr_dat[tr_dat$output == "ok",]$age)
str(dens_age_ok)
approxfun(dens_age_ok$x,dens_age_ok$y)(50)
View(approxfun(dens_age_ok$x,dens_age_ok$y))

dens_age_disease = density(tr_dat[tr_dat$output == "disease",]$age)
str(dens_age_disease)
approxfun(dens_age_disease$x,dens_age_disease$y)(50)

dens_maxhr_ok = density(tr_dat[tr_dat$output == "ok",]$maxhr)
approxfun(dens_maxhr_ok$x,dens_maxhr_ok$y)(140)

dens_maxhr_disease = density(tr_dat[tr_dat$output == "disease",]$maxhr)
approxfun(dens_maxhr_disease$x,dens_maxhr_disease$y)(140)

##Cluster Analysis

clus_dat <- read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/cars-1978.csv")
country = clus_dat$Country
clus_dat$Country = NULL
row.names(clus_dat) = clus_dat$Car
clus_dat$Car = NULL

clus_dat = scale(clus_dat)

set.seed(1)
num_clusters = 2
clust = kmeans(clus_dat, num_clusters)

clust$size
str(clust)
clust_1 = clust$cluster == 1
clust_2 = clust$cluster == 2