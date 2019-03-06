
#
# prob. 4 lab
#

# 1
# You apply to 10 companies for a job, and each company offers
# jobs to 20% of (qualified) applicants.  Compute a sample of
# replies, using 0 for no offer, and 1 for offer.
q1 = sample(c(0,1),10,prob = c(80,20), replace = TRUE)

# 2
# repeat the experiment 10,000 times, and record in a vector
# the number of offers for each experiment
q2 = replicate(10000,(sum(sample(c(0,1),10,prob = c(80,20), replace = TRUE))))
# 3
# plot your vector from 2 as a bar plot, showing for each possible
# value 0-10, the fraction of experiments that resulted in that value
barplot(table(q2)/length(q2))
# 4
# using your data from problem 3, create another graph, but this time,
# for value i (from 0-10) show the fraction of experiments in which
# the number of offers was less than or equal to i. The value for 10 should
# be 1.0.  (Hint: consider using function 'cumsum')
q4 = pbinom(0:10,10,)
barplot(table(q2)/length(q2),)
# 5
# create another plot as in part 3, but this time get your vector 
# of values from # a call to function 'rbinom'.  (Use ?rbinom for details)

# 6
# create one more plot as in part 3, but this time get the values from calls to
# dbinom

# 7
# create another plot as in part 4, but this time get the values from calls to
# pbinom

# 8
# If you have time, work out the probability of getting 2 job offers 
# after 8 applications, given the the probability of an offer in one
# application is 0.1.  











