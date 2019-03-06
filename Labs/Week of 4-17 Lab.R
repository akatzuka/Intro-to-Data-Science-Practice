source("lin-regr-util.R")

dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/College.csv")
#replace values of feature "Private" with feature "PrivPub"
dat$PubPriv = factor(ifelse(dat$Private == "Yes", "private", "public"))
dat$Private = NULL

str(dat)
summary(dat)

dsets = split_data(dat, c(3,1))
tr_dat = dsets[[1]]
te_dat = dsets[[2]]

library(rpart)
fit = rpart(PubPriv ~ Outstate + F.Undergrad + Expend,data=tr_dat, method="class")

library(rpart.plot)
# see rpart.plot documentation
prp(fit, extra=106, varlen=-10,
    main="classification tree for public/private univ.",
    box.col=c("palegreen", "pink")[fit$frame$yval])

predicted = predict(fit, te_dat, type="class")
actual = te_dat$PubPriv
table(actual, predicted)
mean(actual == predicted)

# 4/19

library(rpart)
library(rpart.plot)
library(maptree)

# read the data

dat = read.table("https://raw.githubusercontent.com/grbruns/cst383/master/heart.dat", quote = "/")
names(dat) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")
names(dat) = tolower(names(dat))
dat$output = factor(dat$output)

# compute Gini index for a node (factor of 2 not used)

gini_index = function(v) 
{
  tbl = table(v)
  probs = tbl/sum(tbl)
  terms = sapply(probs, function(p) p * (1 - p))
  return(sum(terms))
}

node = dat[,c("age","restbp","maxhr","output")]
gini_index(node$output)

# compute the Gini improvement associated with a split

# x is a vector of numeric predictor values

# k is a value to "split" x on

# y is a vector of categorical target values

split_score = function(x, k, y) 
{
  y1 = y[x < k]
  y2 = y[x >= k]
  g = gini_index(y)
  g1 = gini_index(y1)
  g2 = gini_index(y2)
  weight1 = length(y1) / length(y)
  weight2 = length(y2) / length(y)
  score = g - (weight1 * g1 + weight2 * g2)
  return(score)
}

split_score(dat$age, 50, dat$output)
split_score(dat$age, 55, dat$output)
split_score(dat$age, 60, dat$output)

scores = c()
for (i in 40:60)
{
  scores[i - 39] = split_score(dat$age, i, dat$output)
}
plot(scores, xlab)
plot(sapply(dat,FUN = split_score(dat$age, c(40:60), dat$output)))

max(scores)

#Regression Trees

library(rpart)
library(rpart.plot)
library(maptree)
source("https://raw.githubusercontent.com/grbruns/cst383/master/lin-regr-util.R")

dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/machine.csv")
dat = dat[,3:9]

set.seed(123)
split = split_data(dat)
tr_dat = split[[1]]
te_dat = split[[2]]

fit = rpart(prp ~ cach + myct + mmin, data = tr_dat)
summary(fit)
prp(fit)

# get predictions on test data
predicted = predict(fit, te_dat)
errors = te_dat$prp - predicted
rmse = sqrt(mean(errors^2))
rmse

hist(predicted)

plot_predict_actual(predicted, te_dat$prp, 10, title = "Predicted vs. Actual")


fit2 = rpart(prp ~ ., data = tr_dat)
prp(fit2)

predicted2 = predict(fit2, te_dat)
errors2 = te_dat$prp - predicted2
rmse2 = sqrt(mean(errors2^2))
rmse2

hist(predicted2)
plot_predict_actual(predicted2, te_dat$prp, 10, title = "Predicted vs. Actual")
