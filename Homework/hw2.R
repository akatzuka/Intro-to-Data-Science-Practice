#@ 1 
# assign the value of variable z to variable x

x = z

#@ 2 
# assign "hello" to variable msg
msg = "hello"
#@ 3 
# check whether variable msg is numeric
typeof(msg)
#@ 4 check whether variable b is logical
typeof(b)
#@ 5 assign the result of squaring x to variable x2
x2 = x*x
#@ 6 
# compute the length of the string obtained by appending strings 
# s1 and s2, with a space between them
nchar(paste(s1, s2))
#@ 7 compute the substring consisting of the 2nd-4th characters of string s
substr(s, 2, 4)
#@ 8 assign the result of testing whether x is greater than 5 to variable xt
xt = (x > 5)
#@ 9 check if variable x is NA
is.na(x)
#@ 10 compute the length of vector x
length(x)
#@ 11 create a vector foo from strings s1, s2, and s3
foo = c(s1, s2, s3)
#@ 12 get the first element of vector x
x[1]
#@ 13 create a vector x consisting of the integers from 50 to 100, in that order
x = c(50:100)
#@ 14 compute the mean value of numeric vector x
mean(x)
#@ 15 create a vector x consisting of values 10, 20, 30, ..., 400
x = seq(10,400,by=10)
#@ 16 
# compute a vector consisting of the 3rd and 4th elements of a vector x
c(x[3],x[4])
#@ 17 
# compute a vector consisting of the elements of a vector x that are negative
x[c(x<0)]
#@ 18 
# given two numeric vectors x and y of the same length, get a vector 
# by adding the corresponding elements of x and y
c(x+y)
#@ 19 
# given a vector x of strings, create a new vector like x but 
# with "baz" added to the end
c(x, "baz")
#@ 20 
# given a numeric vector x, compute a vector like x but sorted 
# and with no duplicates (hint: R has a 'sort' function)
c(sort(x))
#@ 21 
# compute a data by getting rows 3, 5, and 7 from the 'faithful' data
# frame that is built into R
data.frame(faithful[c(3,5,7),])
#@ 22
# compute a data frame by taking rows 1-10 of the mtcars data frame,
# and using only columns "mpg", "disp", and "qsec"
data.frame(mtcars[c(1:10),c(1,3,7)])
#@ 23
# compute the average horsepower across all cars in the mtcars data
# (column "hp" gives the horsepower of a car)
mean(mtcars[,4])
#@ 23
# compute a vector that is like the "hp" column of mt cars, except
# that it gives the relative horsepower, by dividng the "hp" value
# by the average horsepower across all cars
c(mtcars[,4]/mean(mtcars[,4]))
#@ 24
# In one of the first lectures of class, we looked at creating a
# system to identify the part # of a transmission part, based on
# width and length measurements ("features") of the part.  Create
# data frame named tr.dat containing the following training examples
# for the system.  
# part1: 3.1, 5.2
# part1: 3.0, 5.3
# part1: 3.0, 5.2
# part2: 3.1, 4.8
# part2: 3.2, 4.9
# part2: 3.2, 4.8
# part3: 3.4, 5.6
# part3: 3.4, 5.5
# part3: 3.3, 5.4
# Each row above is a training example. In the data frame tr.dat
# that you create, the column names should be "width", "len",
# and "label".  The rows of the data frame should be in the order
# shown above.  For the label values, use numbers 1, 2, or 3.
width = c(3.1, 3.0, 3.0, 3.1, 3.2, 3.2, 3.4, 3.4, 3.3)
len = c(5.2, 5.3, 5.2, 4.8, 4.9, 4.8, 5.6, 5.5, 5.4)
label =c(1, 1, 1, 2, 2, 2, 3, 3, 3)

tr.dat = data.frame(label, width, len)
#@ 25
# x and y and numeric vectors of length 3.  Compute a vector d
# such that d[1] is the (Euclidean) distance from x[1],y[1] to x[1],y[1],
# d[2] is the distance from x[1],y[1] to x[2],y[2], and d[3] is the
# distance from x[1],y[1] to x[3],y[3].
d_1 = sqrt('^'((x[1]-x[1]),2)+'^'((y[1]-y[1]),2))
d_2 = sqrt('^'((x[2]-x[1]),2)+'^'((y[2]-y[1]),2))
d_3 = sqrt('^'((x[3]-x[1]),2)+'^'((y[3]-y[1]),2))
d = c(d_1,d_2,d_3)

