getwd()
setwd("C:/Users/JM933JS/Downloads/MSDS/Stat/r_code/MSDS401")

#question 1
# Use hot_dogs.csv data and hypothesis tests to determine which type of hot dog has average calories
# less than 140 with 95% confidence. Present boxplots of calories by type of hot dog.
hotdogs <- read.csv("hot_dogs.csv")
str(hotdogs)
summary(hotdogs)

beef <- subset(hotdogs,subset = (Type == "Beef"))
meat <- subset(hotdogs,subset = (Type == "Meat"))
poultry <- subset(hotdogs,subset = (Type == "Poultry"))

# First, we'll identify the 95% confidence intervals, per Type
with(beef,t.test(Calories,mu = 140,alternative = "less")$conf.int)
with(meat,t.test(Calories,mu = 140,alternative = "less")$conf.int)
with(poultry,t.test(Calories,mu = 140,alternative = "less")$conf.int)

# Second, we'll extract just the upper bound of the 95% CI
with(beef,t.test(Calories,mu = 140,alternative = "less")$conf.int[2])
with(meat,t.test(Calories,mu = 140,alternative = "less")$conf.int[2])
with(poultry,t.test(Calories,mu = 140,alternative = "less")$conf.int[2])

# Third, we'll add a logical comparison to our statement (" < 140")
with(beef,t.test(Calories,mu = 140,alternative = "less")$conf.int[2]<140)
with(meat,t.test(Calories,mu = 140,alternative = "less")$conf.int[2]<140)
with(poultry,t.test(Calories,mu = 140,alternative = "less")$conf.int[2]<140)

#boxplots
with(hotdogs,boxplot(Calories ~ Type,main = "Calories, by hotdog type",
                     ylab = "Calories"))
abline(h = 140,lty = 2,lwd = 2,col = "red")

#question 2
# Using hot_dogs.csv data and hypothesis tests at the 95% confidence level, determine which type of hot
# dog has an average Sodium level different from 425 milligrams
with(beef,t.test(Sodium,mu = 425,alternative = "two.sided"))
with(meat,t.test(Sodium,mu = 425,alternative = "two.sided"))
with(poultry,t.test(Sodium,mu = 425,alternative = "two.sided"))

#None of the three p-values is less than 0.05, so we do not reject the null hypothesis.

#question 3
# Using hot_dogs.csv data and hypothesis tests, determine if the variance in Sodium values for beef hot
# dogs is different from 6000 with 95% confidence.
var.conf.int <- function(x,conf.level = 0.95){
  df <- length(x) - 1
  chilower <- qchisq((1 - conf.level)/2,df,lower.tail = TRUE)
  chiupper <- qchisq((1 - conf.level)/2,df,lower.tail = FALSE)
  v <- var(x)
  return(c(df*v/chiupper,df*v/chilower))
}

with(beef,var.conf.int(Sodium))

# If this logical is TRUE, then we reject the null hypothesis that mu = 6000:
with(beef, (6000 < var.conf.int(Sodium)[1]) || (6000 >var.conf.int(Sodium)[2]))

#question 4
# Assume a random sample of size 100 is drawn from a normal distribution for which the mean and
# variance are unknown. Assume the sample mean is 50 and the standard deviation of the sample is 2.
# Test the hypothesis that the true mean is 56, and also test the hypothesis the true mean is 40. Report
# p-values and comment on the results.
samp.mean <- 50 # sample mean
test.mean1 <- 56 # mean value to test
test.mean2 <- 40 # alternate mean value to test
samp.sd <- 2 # sample standard deviation
n <- 100 # sample size

# for test.mean1
t1 <- (samp.mean - test.mean1)/(samp.sd/sqrt(n))
t1
p.value1 <- 2*pt(t1,df = n-1,lower.tail = TRUE)
p.value1
?pt()

# for test.mean2
t2 <- (samp.mean - test.mean2)/(samp.sd/sqrt(n))
t2
p.value2 <- 2*pt(t2,df = n-1,lower.tail = FALSE)
p.value2

#question 5
# A coin is flipped 100 times. If it is unbiased the probability of a heads should equal the probability of a
# tails. At the 95% confidence level, test the null hypothesis the coin is unbiased versus the alternative
# that it is biased if 43 heads are obtained. Test the same hypothesis if 63 heads are obtained. Use
# one-sided hypothesis tests.
prop.test(x = 43,n = 100,alternative = "less")

# see p-value 0.0968 > 0.05 (do not reject null hypothesis)

prop.test(x = 63,n = 100,alternative = "greater")

# see p-value 0.00621 < 0.05 (reject null hypothesis)

#question 6
# salaries.csv contains data derived from a November 8, 1993 article in Forbes titled “America’s Best
# Small Companies”. The file gives the CEO age and salary for 60 small business firms. Use these data to
# test the hypothesis at 95% confidence that at least 50% of the CEOs are 45 years old or older. Also
# test the hypothesis at 95% confidence that at least 50% of the CEOs earn less than $500,000 per year.
# Use one-sided hypothesis tests.

salaries <- read.csv("salaries.csv")
str(salaries)
summary(salaries)

age <- salaries$AGE >= 45
age
count <- sum(age)
count
total <- length(age)
total

# To test the hypothesis about age, we must count the number >= 45 years old.
# Then prop.test will be used.
prop.test(x = count,n = total,alternative = "greater")

# The p-value is less than 0.05 so reject the null hypothesis.

# Now we must count the number earning less than $500,000 per year.
# Then prop.test will be used.
salary <- salaries$SAL < 500
count <- sum(salary)
total <- length(salary)
prop.test(x= count, n = total, alternative = "greater")

# The null hypothesis must be rejected based on the small p-value.

