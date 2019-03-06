x = abs(2.5 * -23.2)
dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/german-credit.csv")

hist(dat$amount, col="firebrick", xlab="Loan amount (DM)",
     main="Histogram of German loan amounts")

v1 = c("foo", "bar", "baz")
v_2 = v1[2]
v2 = c(1:3)
paste0(v1, v2)
x = sample(1:500, 10)
mean(x)
sum(x)/length(x)