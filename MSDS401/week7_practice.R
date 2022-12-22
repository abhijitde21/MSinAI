getwd()
setwd("C:/Users/JM933JS/Downloads/MSDS/Stat/r_code/MSDS401")

#question 1
# A double-blind clinical trial of a new drug for back pain was designed using control and treatment
# groups. Volunteers were fully informed and assigned at random to each group. Neither the volunteers
# nor the doctor knew when the new drug or a placebo was being administered. When 100 volunteers
# in each group had been treated and evaluated, the results revealed an 85% success rate for the new
# drug and a 65% success rate for the control group. At the 95% confidence level, is there a statistically
# significant difference between the two reported rates? Use a one-sided test. Also, report a confidence
# interval for the difference.

x <- matrix(c(85,65,15,35),nrow = 2,ncol = 2,byrow = FALSE,
            dimnames = list(c("new_drug","control"),c("success","fail")))
print(x)

prop.test(x,alternative = "greater",conf.level = 0.95,correct = FALSE)

# p-value = 0.0009589 < 0.05 (reject null hypothesis)

#question 2
# Two baseball players had their career records compared. In 267 times at bat, one player hit 85 home
# runs. In 248 times at bat, the other player hit 89 home runs. Assume the number of home runs follows
# a binomial distribution, is there a statistically significant difference with 95% confidence between the
# home run averages for these two baseball players?
  
x <- matrix(c(85,89,(267-85),(248-89)),nrow = 2,ncol = 2,byrow = FALSE,
            dimnames = list(c("Player A", "Player B"),c("HR", "Other")))
print(x)

prop.test(x,alternative = "two.sided",conf.level = 0.95,correct = FALSE)

# p-value = 0.3799 > 0.05 (do not reject null hypothesis, the difference
# between the home run rates of these players is nonsignificant.)

#question 3
# Using the home_prices.csv data (described in Lesson 1), compare mean selling prices between homes
# located in the northeast sector of the city versus the remaining homes. Also, compare the mean selling
# prices between homes with a corner lot and those located elsewhere. Use two-sample t-tests for the
# hypothesis tests at the 95% confidence level. Report confidence intervals for each.

houses <- read.csv("home_prices.csv")
str(houses)
summary(houses)

# price stats across sectors
with(houses,by(PRICE,NBR,summary))

# price stats across corner or not
with(houses,by(PRICE,CORNER,summary))

# Now, we are ready to do the hypothesis tests.
NE_PRICE <- subset(houses,subset = (NBR == "YES"))$PRICE
NE_PRICE
OTHER_PRICE <- subset(houses,subset = (NBR == "NO"))$PRICE
OTHER_PRICE
t.test(NE_PRICE,OTHER_PRICE,alternative = "two.sided",conf.level = 0.95)

# p-value = 0.1134 > 0.05 (do not reject null hypothesis; prices of homes
# in the NE are not statistically different from prices of other homes).

CORNER_PRICE <- subset(houses,subset = (CORNER == "YES"))$PRICE
CORNER_PRICE
NON_CORNER_PRICE <- subset(houses,subset = (CORNER == "NO"))$PRICE
NON_CORNER_PRICE
t.test(CORNER_PRICE,NON_CORNER_PRICE,alternative = "two.sided",conf.level = 0.95)

# p-value = 0.6685 > 0.05 (do not reject null hypothesis; prices of homes
# on corners are not statistically different from non-corner homes).

#question 4
# The nsalary.csv data are derived from data collected by the Department of Social Services of the State
# of New Mexico. The data have been adapted for this problem. Using these data compare mean salary
# levels between RURAL and non-RURAL locations. Use a two-sample t-test at the 95% confidence level.
# Report your results.

nsalary <- read.csv("nsalary.csv")
str(nsalary)
summary(nsalary)

# price stats across sectors
with(nsalary,by(NSAL,RURAL,summary))

# Create comparative boxplot
with(nsalary,boxplot(NSAL ~ RURAL,main = "Salary, RURAL",ylab = "Salary"))

# salaries obviously different for rural vs. non-rural
RURAL_SALARY <- subset(nsalary,subset = (RURAL == "YES"))$NSAL
RURAL_SALARY
NON_RURAL_SALARY <- subset(nsalary,subset = (RURAL == "NO"))$NSAL
NON_RURAL_SALARY
t.test(RURAL_SALARY,NON_RURAL_SALARY,alternative = "two.sided",conf.level = 0.95)

# p-value = 8.504e-06 < 0.05 (reject null hypothesis, there are
# statistically significant differences between rural and non-rural salaries.)

#question 5
# tires.csv contains data published by R.D. Stichler, G.G. Richey, and J. Mandel, “Measurement of
# Treadware of Commercial Tires, Rubber Age, 73:2 (May 1953). Treadwear measures of tires each tire
# was subject to measurement by two methods, the first based on weight loss and the second based on
# groove wear. Use a paired t-test at the 95% confidence level to test for a difference between the two
# methods. Report your results using a confidence interval.

tires <- read.csv("tires.csv")
str(tires)
summary(tires)

# Let's see how the measures compare on a scatter plot, using
# the same scale for both axes and a diagonal line of equality.

with(tires,plot(WGT,GRO,las = 1,
                xlim = c(min(WGT,GRO),max(WGT,GRO)),
                ylim = c(min(WGT,GRO),max(WGT,GRO))))
segments(10,10,45,45,col = "red")
title("Comparing Measures of Tire Wear")

# Note that all but one of the WGT measures is larger than
# the corresponding GRO measure.

# Now for the paired t-test
with(tires,t.test(WGT,GRO,alternative = "two.sided",
                  paired = TRUE,conf.level = 0.95))

#p-value = 4.614e-05 < 0.05 (reject the null hypothesis that the means
# of the two measures are identical. There are statistically significant
# differences between these two measures of tire wear.)