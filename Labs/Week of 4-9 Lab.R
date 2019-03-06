heart = read.table("https://raw.githubusercontent.com/grbruns/cst383/master/heart.dat", quote = "/")

names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL","SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR","THAL", "OUTPUT")
names(heart) = tolower(names(heart))
heart$output = heart$output - 1    # convert to 0-1 range

heart$sex = factor(heart$sex)
heart$sugar = factor(heart$sugar)
heart$angina = factor(heart$angina)
heart$chestpain = factor(heart$chestpain)
heart$ecg = factor(heart$ecg)
heart$thal = factor(heart$thal)
heart$exercise = factor(heart$exercise)

tr_rows = sample(nrow(heart), 0.8 * nrow(heart))
tr_dat = heart[tr_rows,]
te_dat = heart[-tr_rows,]

fit = glm(output~ sex + chestpain + dep + fluor + thal, data = tr_dat, family = binomial)
summary(fit)

y = predict(fit, newdata=te_dat, type="response")
predicts = as.numeric(y > 0.2)
actuals = te_dat$output
conf_mtx = table(predicts, actuals)

mean(predicts == actuals)
mean(predicts == 1 & actuals == 0)
mean(predicts == 0 & actuals == 1)

 fit2 = glm(output~ ., data = tr_dat, family = binomial)
 summary(fit2)

tr_rows100 = sample(nrow(heart), 0.8 * nrow(heart))
tr_dat100 = heart[tr_rows,]
te_dat100 = heart[-tr_rows,]

fit100 = apply