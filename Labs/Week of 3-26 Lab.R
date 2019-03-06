# read data

user="brun1992"

dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/College.csv")    


# make college names into the row

rownames(dat) = dat[,1]

dat = dat[,-1]


# scale all features

dat[,2:18] = scale(dat[,2:18])


# feature data

fdat = dat[,c("Outstate", "F.Undergrad")]


# labels for the feature data

labels =  dat[,"Private"]


# example to classify

x1 = data.frame(Outstate=0, F.Undergrad=0.5)


# kNN parameter k

k = 5

edist = function(x1,x2) sqrt(sum((x1-x2)^2))

dists = apply(fdat, 1, function(x1) edist(fdat,x1))

close_rows <- dists[order(dists, decreasing = TRUE)][1:k]
close_labels <-fdat[]

x = runif(20)
y=(order(abs(x-.5)))
z=y[c(1:3)]
y1 = x[z]

# slope and intercept of a line

m = 1.2

b = -1.4

# randomly generate n inputs from 0 to 10

n = 30

x = sort(runif(n, min=0, max=10))

# calculate the corresponding outputs, with added noise

y = m*x + b + rnorm(n, sd=0.5)

# plot them

plot(x, y, pch=20, col="grey40")

k = 3
x1 = runif(1,0,10)
z=(order(abs(x-x1)))[1:k]
y = y[z]
y1 = mean(y)

par(new = FALSE)
points(x1,y1, pch=17, col="firebrick")
par(new = TRUE)

############
data = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/machine.csv")
plot(data)
plot()