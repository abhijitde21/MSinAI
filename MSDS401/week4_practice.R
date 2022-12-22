getwd()
setwd("C:/Users/JM933JS/Downloads/MSDS/Stat/r_code/MSDS401")

#question 1 lession 6 
# Assume the purchases of shoppers in a store have been studied for a period of time and it is determined
# the daily purchases by individual shoppers are normally distributed with a mean of $81.14 and a
# standard deviation of $20.71. Find the following probabilities using R.

# a) What is the probability that a randomly chosen shopper spends less than $75.00
sprintf("%.4f",pnorm(75,mean = 81.14,sd = 20.71,lower.tail = TRUE))

# b) What proportion of shoppers spends more than $100.00?
sprintf("%.4f",pnorm(100,mean = 81.14,sd = 20.71,lower.tail = FALSE))

# c) What proportion of shoppers spends between $50.00 and $100.00?
prob_greater_than_50 <- pnorm(50,mean = 81.14,sd = 20.71,lower.tail = FALSE)
prob_greater_than_50
prob_greater_than_100 <- pnorm(100,mean = 81.14,sd = 20.71,lower.tail = FALSE)
prob_greater_than_100
sprintf("%.4f",prob_greater_than_50 - prob_greater_than_100)

#question 2
# Assume that the shopper’s purchases are normally distributed with a mean of $97.11 and a standard
# deviation of $39.46. Find the following scores using R.

# a) What weight is the 90th Percentile of the shoppers’ purchases? That is, find the score P90
# that separates the bottom 90% of shoppers’ purchases from the top 10%.
sprintf("%.4f",qnorm(0.90,mean = 97.11,sd = 39.46,lower.tail = TRUE))

# b) What is the median shoppers’ purchase? (Find the score P50 that separates the bottom 50%
# of shoppers’ purchases from the top 50%.) What is important about this number?
sprintf("%.4f",qnorm(0.50,mean = 97.11,sd = 39.46,lower.tail = TRUE))
# What is important about this number?
# The normal distribution is symmetric, so its mean and median are identical.

#question 3
# Generate a sample of size 50 from a normal distribution with a mean of 100 and a standard deviation of
# 4. What is the sample mean and sample standard deviation? Calculate the standard error of the mean
# for this sample. Generate a second sample of size 50 from the same normal population. What is the
# sample mean and sample standard deviation? Calculate the standard error of the mean for this sample.
# 
# Compare your results. Are the sample means and sample standard deviations of random samples of the
# same size taken from the same population identical? Why or why not?
# 
# Now, repeat this process generating a sample of size 5000. Calculate the sample mean, sample
# standard deviatioon and standard error of the mean for this third sample and compare to the
# previous samples.

set.seed(123)

my_first_sample <- rnorm(50,mean = 100,sd = 4)
my_first_sample
std_error1 <- 4/sqrt(50)
std_error1
cat("\nmy_first_sample mean: ",mean(my_first_sample),
    " sample_sd: ",sd(my_first_sample)," sample_std_error: ",std_error1,"\n")

my_second_sample <- rnorm(50,mean = 100,sd = 4)
my_second_sample
std_error2 <- 4/sqrt(50)
cat("\nmy_second_sample mean: ",mean(my_second_sample),
    " sample_sd: ",sd(my_second_sample)," sample_std_error: ",std_error2,"\n")

my_third_sample <- rnorm(5000,mean = 100,sd = 4)
my_third_sample
std_error3 <- 4/sqrt(5000)
cat("\nmy_second_sample mean: ",mean(my_third_sample),
    " sample_sd: ",sd(my_third_sample)," sample_std_error: ",std_error3,"\n")

#question 4
# Assume a biased coin when flipped will generate heads one third of the time. Estimate the probability
# of getting at least 250 heads out of 600 flips using the normal distribution approximation. Compare to
# the exact probability using the binomial distribution.

n <- 600
p <- 1/3
x <- 250

# R provides binomial probabilties directly
sprintf("%.6f",pbinom(q = x,size = n,prob = p,lower.tail = FALSE))

# The normal approximation to the binomial is very close to the binomial.
xCorrect <- 250 - 0.5
z <- (xCorrect - n*p)/sqrt(n*p*(1-p))
sprintf("%.6f",pnorm(z,mean = 0,sd = 1,lower.tail = FALSE))

#question 5
# Use the uniform distribution over 0 to 1. Generate three separate simple random samples of size n =
# 25, n = 100, n = 400. Plot histograms for each and comment on what you observe.

par(mfrow = c(1,3))
hist(runif(25,min = 0,max = 1),main = "")
hist(runif(100,min = 0,max = 1),main = "")
hist(runif(400,min = 0,max = 1),main = "")
mtext("Histograms of uniform distribution (n = 25, 100 and 400)",side = 3,outer = T,line = -1)
par(mfrow = c(1,1))

#question 6
# salaries.csv gives the CEO age and salary for 60 small business firms. Construct QQ plots and histograms.
# Is the distribution of ages a normal distribution? Explain your answer.

salaries <- read.csv("salaries.csv")
str(salaries)
summary(salaries)
with(salaries,hist(AGE))
with(salaries,plot(density(AGE)))
qqnorm(salaries$AGE,main = "AGE QQ",xlab = "Normal Quantiles",ylab = "Age Quantile")
qqline(salaries$AGE,distribution = qnorm,probs = c(0.25,0.75),qtype = 7)

#question 1 lession 7
# Use the uniform distribution over the interval 0 to 1. Draw 100 random samples of size 10. Calculate
# the means for each sample. Using the 100 mean values plot a histogram. Repeat with 100 random
# samples of size 50. Repeat with 100 samples of size 500. Present the three histograms using par().
# Calculate the variance of each histogram and compare to the original uniform distribution. What do
# you conclude?

set.seed(1234)

# 10 samples of size 50
x <- c()  
y <- c()

for (i in 1:100) {
  z <- runif(10)
  x <- append(x,mean(z))
  y <- append(y,var(z))
}

x
y
hist(x)

# repeat with 100 samples of size 50
a <- c()
b <- c()

for (i in 1:100) {
  c <- runif(50)
  a <- append(a,mean(c))
  b <- append(b,var(c))
}

hist(a)

# repeat with 100 samples of size 500
d <- c()
e <- c()

for (i in 1:100) {
  f <- runif(500)
  d <- append(d,mean(f))
  e <- append(e,var(f))
}

hist(d)

# present the three histograms using par() and mfrow()
par(mfrow = c(1,3),oma = c(0,0,2,0))
hist(x,main = "",xlim = c(0.2,0.8))
abline(v = 0.5,col = "darkred",lwd = 2)
hist(a,main = "",xlim = c(0.2,0.8))
abline(v = 0.5,col = "darkred",lwd = 2)
hist(d,main = "",xlim = c(0.2,0.8))
abline(v = 0.5,col = "darkred",lwd = 2)
mtext("Histogram of random uniform sample means 
      (n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

# create and present histograms of the sample variances
par(mfrow = c(1,3), oma = c(0,0,2,0))
hist(y, main = "", xlim = c(0.02,0.16))
abline(v = 0.0833333, col = "darkred", lwd = 2)
hist(b, main = "", xlim = c(0.02,0.16))
abline(v = 0.0833333, col = "darkred", lwd = 2)
hist(e, main = "", xlim = c(0.02,0.16))
abline(v = 0.0833333, col = "darkred", lwd = 2)
mtext("Histogram of random uniform sample variances
(n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

# abline() for mean and variance histograms equal to "true" values
# for a uniform distribution (0,1).
# mean = a + b / 2 = 0.5
# variance = (b - a)^2 / 12 = 0.08333333

# question 2

# Using the histogram determined above for samples of size 50, find the quartiles. Using the normal
# distribution with the true mean and variance for a uniform distribution over the interval 0 to 1,
# determine the theoretical quartiles for a sample mean from 50 observations. Compare the two sets of
# quartiles. What do you conclude?

j <- c()

for (i in 1:100) {
  k <- rnorm(50,0.5,sqrt(0.08333333))
  j <- append(j,mean(k))
}

quantile(a)
quantile(j)

# The two sets of quartiles are very similar, likely to converge as sample sizes
# are increased.

# question 3

# Use the binomial distribution with p = 0.5. Draw 100 random samples of size 10. Calculate the means
# for each sample. Using the 100 mean values plot a histogram. Repeat with 100 random samples of
# size 50. Repeat with 100 samples of size 500. Present the three histograms using par(). Calculate the
# variance of each histogram and compare to the original mean and variance for the binomial. What do
# you conclude?

m <- c()
n <- c()

for (i in 1:100) {
  o <- rbinom(10,1,p = 0.5)
  m <- append(m,mean(o))
  n <- append(n,var(o))
}

hist(m)

# repeat with 100 samples of size 50
p <- c()
q <- c()

for (i in 1:100) {
  o <- rbinom(50,1,p = 0.5)
  p <- append(m,mean(o))
  q <- append(n,var(o))
}

hist(p)

# repeat with 100 samples of size 500
s <- c()
t <- c()

for (i in 1:100) {
  u <- rbinom(500,1,p = 0.5)
  s <- append(s,mean(u))
  t <- append(t,var(u))
}

hist(s)

# present the three histograms using par() and mfrow()
par(mfrow = c(1,3), oma = c(0,0,2,0))
hist(m, main = "", xlim = c(0.2,0.8))
abline(v = 0.5, col = "darkred", lwd = 2)
hist(p, main = "", xlim = c(0.2,0.8))
abline(v = 0.5, col = "darkred", lwd = 2)
hist(s, main = "", xlim = c(0.2,0.8))
abline(v = 0.5, col = "darkred", lwd = 2)
mtext("Histogram of random binomial (p = 0.5) sample means
(n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

# create and present histograms of the sample variances
par(mfrow = c(1,3), oma = c(0,0,2,0))
hist(n, main = "", xlim = c(0.10,0.25))
abline(v = 0.25, col = "darkred", lwd = 2)
hist(q, main = "", xlim = c(0.10,0.25))
abline(v = 0.25, col = "darkred", lwd = 2)
hist(t, main = "", xlim = c(0.10,0.25))
abline(v = 0.25, col = "darkred", lwd = 2)
mtext("Histogram of random binomial (p = 0.5) sample variances
(n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

#question 4

# Using the histogram determined above for samples of size 50, find the quartiles. Using the normal
# distribution with the true mean and variance for a binomial distribution with p = 0.5, determine the
# theoretical quartiles for a sample mean from 50 observations. Compare the two sets of quartiles. What
# do you conclude?

v <- c()

for (i in 1:100) {
  w <- rnorm(50,0.5,sqrt(0.25))
  v <- append(v,mean(w))
}

quantile(p)
quantile(w)

#question 5

# Use the binomial distribution with p = 0.1. Draw 100 random samples of size 10. Calculate the means
# for each sample. Using the 100 mean values plot a histogram. Repeat with 100 random samples of
# size 50. Repeat with 100 samples of size 500. Present the three histograms using par(). Calculate the
# variance of each histogram and compare to the original mean and variance for the binomial. What do
# you conclude?

mm <- c()
nn <- c()

for (i in 1:100) {
  oo <- rbinom(10, 1, p = 0.1)
  mm <- append(mm, mean(oo))
  nn <- append(nn, var(oo))
}

hist(mm)

# repeat with 100 samples of size 50
pp <- c()
qq <- c()

for (i in 1:100) {
  rr <- rbinom(50, 1, p = 0.1)
  pp <- append(pp, mean(rr))
  qq <- append(qq, var(rr))
}

hist(pp)

# repeat with 100 samples of size 500
ss <- c()
tt <- c()

for (i in 1:100) {
  uu <- rbinom(500, 1, p = 0.1)
  ss <- append(ss, mean(uu))
  tt <- append(tt, var(uu))
}

hist(ss)

# present the three histograms using par() and mfrow()
par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(mm, main = "", xlim = c(0,0.5))
abline(v = 0.1, col = "darkred", lwd = 2)
hist(pp, main = "", xlim = c(0,0.5))
abline(v = 0.1, col = "darkred", lwd = 2)
hist(ss, main = "", xlim = c(0,0.5))
abline(v = 0.1, col = "darkred", lwd = 2)
mtext("Histogram of random binomial (p = 0.1) sample means
      (n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

# create and present histograms of the sample variances
par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(nn, main = "", xlim = c(0,0.30))
abline(v = 0.09, col = "darkred", lwd = 2)
hist(qq, main = "", xlim = c(0,0.30))
abline(v = 0.09, col = "darkred", lwd = 2)
hist(tt, main = "", xlim = c(0,0.30))
abline(v = 0.09, col = "darkred", lwd = 2)
mtext("Histogram of random binomial (p = 0.1) sample variances
(n = 10, n = 50 and n = 500)",side = 3, outer = T, line = -1)
par(mfrow = c(1,1))

#question 6

# Using the histogram determined above for samples of size 50, find the quartiles. Using the normal
# distribution with the true mean and variance for a binomial distribution with p = 0.1, determine the
# theoretical quartiles for a sample mean from 50 observations. Compare the two sets of quartiles. What
# do you conclude?

vv <- c()

for (i in 1:100) {
  ww <- rnorm(50, 0.1, sqrt(0.09))
  vv <- append(vv, mean(ww))
}

quantile(pp)
quantile(vv)  


# test 2

#question 1
0.0125/0.125

#question 2
x <- c(0,1,2,3)
px <- c(0.749,0.225,0.024,0.002)
expected_value <- sum(x*px)
expected_value
variance1 <- sum(((x-expected_value)^2)*px)
variance1

#question 3
q1 <- c(1.3,2.2,2.7,3.1,3.3,3.7)
q2 <- quantile(q1,probs = c(0.33),type = 7)
q2

#question 4

(pnorm(50.5,mean = 0.7*76,sd = sqrt(76*0.7*0.3),lower.tail = TRUE) 
  - pnorm(49.5,mean = 0.7*76,sd = sqrt(76*0.7*0.3),lower.tail = TRUE))

#question 5
print("The area to the left of 47.5")

#question 6
pbinom(1,size = 10,prob = 1/9,lower.tail = FALSE)

#question 7
z1 <- (8.7-8.4)/(1.8/sqrt(36))
z1
pnorm(1,0,1,lower.tail = FALSE)

#question 8
pbinom(0,size = 5,prob = 1/9,lower.tail = FALSE)

#question 9
3/3^5

#question 10
(40/50)^3
