v1 = c("foo", "bar", "baz", "whatevs")
v2 = c(1:4)
dat= data.frame(jargon=c(v1),points=c(v2), stringsAsFactors=FALSE )
row_num = nrow(dat)
dat_names = names(dat)
row_2 = dat[2,]
col_2 = dat$jargon
jar_r1_r2 = col_2[1:2]
q_8_2 = dat$points>2
q_9_2 = nchar(dat$jargon) > 3
q_10_2 = 
names(dat) = c("slang", "points")
