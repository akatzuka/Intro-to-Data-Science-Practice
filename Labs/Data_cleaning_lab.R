dat = airquality
head(dat)
str(dat)
sum(complete.cases(dat) == FALSE)

nrow(is.na(dat) == TRUE)
z = apply(dat,2,function(x) mean(is.na(x)))
apply(dat,1,function(x) mean(is.na(x)))
barplot(z, length(z), names.arg = (dat[,1:length(z)]

                                   