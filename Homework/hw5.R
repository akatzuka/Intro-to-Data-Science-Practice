
#
# Intro to Data Science - Homework 5
#

# In this homework we will write objects that model probability mass
# functions and discrete random variables.

# The functions that capture probability mass functions come first,
# and have names that start with "pmf_".

# The functions that capture random variables come next, and have
# names that start with "rv_".

# Please supply code wherever you see "YOUR CODE HERE"

# Do not change anything else in this file!

###################################################################
#
# PMF object
#
###################################################################

# A PMF object models a probability mass function with finite support.
# Such an object has two components: "support", which is a numeric
# vector, and "probs", another numeric vector of the same length as
# "support".  The idea is that the probability of value support[1]
# is probs[1], the probability of support[2] is probs[2], etc.
# 
# So if we have a probability mass function with support 1:3, we
# might have probs c(0.5, 0.1, 0.4), meaning that the probability
# of 1 is 0.5, the probability of 2 is 0.1, and the probability of
# 3 is 0.4.
#
# In the comments below, I will use object 'pmf' for this example.

# return a new PMF
# support - a non-empty numeric vector
# probs   - a numeric vector of the same length as support
# example: pmf = pmf_create(1:3, c(0.5, 0.1, 0.4))
pmf_create = function(support, probs) {
  stopifnot(length(support) > 0 && length(support) == length(probs))
  
  # normalize probs so that they sum to 1
  probs = probs/sum(probs)
  
  # put support and probs in order of increasing values of support
  idx = order(support)
  pmf = list(support=support[idx], probs=probs[idx])
  
  return(pmf)
}

# return n samples from the PMF (with replacement)
# example: pmf_sample(pmf, 4) might give 1, 2, 1, 3
pmf_sample = function(pmf, n=1) {
  
  return(sample(1:length(pmf$support),n, replace = TRUE))
}

# return the expectation of the PMF
# example: pmf_expect(pmf) gives 1.9
pmf_expect = function(pmf) {
  return(sum(pmf$support * pmf$probs))
}

# return the variance of the PMF
# example: pmf_variance(pmf) gives 0.89
pmf_variance = function(pmf) {
  
  return((sum(pmf$support^2 * pmf$probs)) - (pmf_expect(pmf)^2))
  
}

# return the sum of the probabilities associated with
# the subset a of the support of this PMF
# example: pmf_prob(pmf, 1) gives 0.5
# example: pmf_prob(pmf, c(1,3)) gives 0.9
pmf_prob = function(pmf, a) {
  
  return(sum(pmf$probs[is.element(pmf$support, unique(a))]))

}

# plot the pmf
  # example: pmf_plot(pmf) gives three bars, with x values
# 1, 2, and 3, and y values 0.5, 0.1, and 0.4
pmf_plot = function(pmf) {
  
  barplot(pmf$probs, width = length(pmf$probs), names.arg = (c(pmf$support)))
  # hint: I recommend using 'barplot'
  # hint: do you know about parameter 'names.arg' of barplot?

}

###################################################################
#
# RV object
#
###################################################################

# An RV object models a discrete random variable with finite support.
# This means there are only finitely-many values the random variable
# can take.  In an RV object we capture a sample space, the values
# that the random variable gives for each outcome in the sample
# space, and the probability of each outcome.
#
# In our R code, an RV object has three components: "outcomes", which 
# is a data frame with as many rows as there are outcomes in the sample
# space, "probs", a numeric vector that gives a probability for each row
# of the outcomes data frame, and "vals", which is a numeric vector which
# gives the number for each outcome.  Remember that a random variable is
# defined as something that labels each outcome of a sample space with a
# number.  That is what "vals" does.
# 
# So if we have a random variable X that gives the number of heads in
# two coin flips, we would have outcomes is data.frame(x1=c(0,0,1,1), x2=c(0,1,0,1)),
# probs = c(0.25, 0.25, 0.25, 0.25), and vals = c(0,1,1,2).
#
# In the comments below, I use 'x' for this example.  I use 'y' for the
# random variable that is 1 when the exactly one coin is head, and 0 otherwise.


# return a discrete random variable with finite support
# outcomes - a data frame of n > 0 rows; each row represents one outcome
# probs    - probability of each outcome; a numeric vector of length n, all values >= 0
# vals     - numeric value of each outcome; a numeric vector of length n
# example: x = rv_create(data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1)), probs=rep(0.25,4), vals=c(0,1,1,2))
# example: y = rv_create(data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1)), probs=rep(0.25,4), vals=c(0,1,1,0))
rv_create = function(outcomes, probs, vals) {
  n = nrow(outcomes)
  stopifnot(n > 0 && length(probs) == n && length(probs) == n && all(probs >= 0))
  
  # make probs such that sum(probs) is 1
  probs = probs/(sum(probs))
  
  # compute the PMF
  support = unique(vals)
  val_probs = sapply(support, function(x) sum(probs[vals == x]))
  pmf = pmf_create(support, val_probs)
  x = list(outcomes=outcomes, probs=probs, vals=vals, pmf=pmf)
  return(x)
}

# return n samples from random variable x, with replacement
# example: rv_sample(x, 5) might give 1 2 1 0 1
rv_sample = function(x, n=1) {
  
  return(sample(1:length(x$vals),n, replace = TRUE, prob = x$probs))

}

# get expectation of random variable x
# example: rv_expect(x) is 1
rv_expect = function(x) {

    return(sum(x$vals * x$probs))

}

# return the variance of random variable x
# example: rv_variance(x) is 0.5
rv_variance = function(x) {
  
  return((sum(x$vals^2 * x$probs)) - (rv_expect(x)^2))
  
}

# plot the PMF of random variable s
# example: rv_plot(x)
rv_plot = function(x) {

  barplot(x$probs, width = length(x$probs), names.arg = (c(x$vals)))
  
}

# return the probability that the value of random variable x
# is in the given subset a of the support of x
# example: rv_prob(x, 2) is 0.25
# example: rv_prob(x, 1:2) is 0.75
rv_prob = function(x, a) {
  
  return(pmf_prob(x$pmf,a))
  # hint: remember that RV objects have a PMF component

}

# return a boolean vector indicating the outcomes associated with
# the subset a of the support of the random variable
# example: rv_event(x, 1) is FALSE, TRUE, TRUE FALSE
# (because in x, the second and third row of the outcomes data frame get value 1)
# example: rv_event(x, 2) is FALSE, FALSE, FALSE, TRUE
rv_event = function(x, a) {
  
  return(is.element(x$vals, unique(a)))

}

# return the conditional probability that the value of random variable x 
# belongs to a set of values, given some condition.  The set of values is 
# given as a vector of values in the support of x.  The condition is given 
# as a boolean vector indicating the elements of the sample space that define 
# the condition.
# example: rv_cond_prob(x, 1, c(TRUE, TRUE, TRUE, FALSE)) is 0.6667
# example: rv_cond_prob(x, 2, c(TRUE, TRUE, TRUE, FALSE)) is 0
# example: rv_cond_prob(x, 1, rv_event(x, 1:2)) is 0.6667
rv_cond_prob = function(x, a, event) {
  
  #brun's code calls rv_create
  return(rv_prob(rv_create(x$outcomes, (x$probs*event), (x$vals*event)), a))
}

# return a new random variable by taking the sum of two random variables
# example: z = rv_add(x, y)
# example: rv_variance(z) is 0.75
rv_add = function(x, y) {
  # probability spaces of x and y must be the same
  stopifnot(all(x$outcomes == y$outcomes))
  stopifnot(all(x$probs == y$probs))
  
  return(rv_create(x$outcomes, y$probs, (x$vals + y$vals)))

}

# return a new random variable by taking a function f of random variable x
# example: x2 = rv_apply_fun(x, function(x) x^2)
# example: rv_variance(x2) is 2.25
rv_apply_fun = function(x, f) {
  
  # YOUR CODE HERE (14)
  # hint: my code contained a call to rv_create
  return(rv_create(x$outcomes, x$probs, f(x$vals)))

}

