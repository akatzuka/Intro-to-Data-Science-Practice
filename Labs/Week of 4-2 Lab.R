machine = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/machine.csv")

machine = machine[,3:9]

tr_row = sample(nrow(machine),0.7*nrow(machine))
tr_dat = machine[tr_row,]
te_dat = machine[-tr_row,]

labels = machine[,"prp"]
tr_labels = labels[(1:n)]
te_labels = labels[-(1:n)]

model = lm(prp ~ myct + cach + mmin, dat = tr_dat)
actual = tr_labels
predicted = predict(model, te_dat)
rmse = sqrt(mean(actual - predicted) ^2)
