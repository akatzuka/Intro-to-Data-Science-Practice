#
# CST 383: Data cleaning with R
#
# Dr. Bruns

###########################################################################
#
# Write code after each problem number, following the instructions
# in comments.
#
# Important: when writing the code for a problem, do not use functions
# or variables from previous problems, except as directed.
#
###########################################################################

# read the campaign contribution data

dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/campaign-ca-2016-sample.csv")

# get a summary of the data

summary(dat)

#@ 1
# How many NAs in the data?
# (compute a number)
sum(is.na(dat))

#@ 2
# Use apply to compute a vector containing the number of
# NA values in each column of dat 
# (compute a vector)
apply(dat,2,function(x){sum(is.na(x))})

#@ 3
# Get the rows of dat with more than 1 NA (it's possible there are none)
# (compute a data frame)
dat[!complete.cases(dat),]

#@ 4
# What fraction of the values of the contb_occupation column are ""?
# Ignore any NA values that might be present.
# (compute a value between 0 and 1)
sum(dat$contbr_occupation == "", na.rm=TRUE)/length(dat$contbr_occupation)

#@ 5
# Create a new data frame dat1 that is like dat except all rows
# containing NA values are removed.  
# (assign to dat1)
dat1 = dat[complete.cases(dat),]

#@ 6
# Create a new data frame dat2 that is like dat (NOT dat1) by
# replacing all NA values by ""
dat2 = dat 
dat2[is.na(dat2)] <- ""

#@ 7
# How many contributor zip values have length that is not 5 or 9?
# (note: the zip values are stored by R as factor values, so your will
# probably want to convert to character values first, but don't modify 
# data frame dat)
# (compute a number)
sum(nrow(dat$contbr_zip) - nchar(as.character(dat$contbr_zip)) != 5 | nchar(as.character(dat$contbr_zip)) != 9)

#@ 8
# What fraction of contb_receipt_amt values are less than 0?
# (compute a number between 0 and 1)
sum(dat$contb_receipt_amt < 0, na.rm=TRUE)/length(dat$contb_receipt_amt)

#@ 9
# Modify dat to add a new 'scaled_receipt_amt' column.  
# This column will contain values of 'contb_receipt_amt' that have
# been scaled as follows:
#   - 0 will remain 0
#   - the largest contb_receipt_amt value will become 1
# All other values will be scaled linearly in a way that is
# consistent with the 0 and 1 values.  Note that some values
# will become negative.
# (add a column to dat)
scaled = dat$contb_receipt_amt / max(dat$contb_receipt_amt)
dat$scaled_receipt_amt = scaled

#@ 10
# modify dat to add another new column, named 'zscaled_receipt_amt'.
# This column contain the 'contb_receipt_amt' values that have
# been scaled using z-score normalization.
# (add a column to dat)
zscaled = scale(dat$contb_receipt_amt)
dat$zscaled_receipt_amt = zscaled












