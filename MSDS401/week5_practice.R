getwd()
setwd("C:/Users/JM933JS/Downloads/MSDS/Stat/r_code/MSDS401")

#question 1
# Assume a random sample of size 100 is drawn from a normal distribution with variance 1. The average
# value of the sample is 50. Find a 95% confidence interval for the mean.

n <- 100
mean <- 50
sd <- 1

margin.of.error <- qnorm(1-(0.05/2),mean = 0, sd = 1)*sd/sqrt(n)

conf.int <- c(mean - margin.of.error, mean + margin.of.error)
conf.int

#question 2
# Assume the standard deviation for a normal distribution is equal to 100 units. Also assume we want
# to estimate the unknown mean with a 95% confidence interval of total width 8 units. Calculate the
# sample size required.

z_score <- qnorm(1 - 0.025,mean = 0, sd = 1,lower.tail = TRUE)
z_score

sample_size <- (z_score*100/4)**2
round(sample_size)

#question 3
# A random sample of 1600 registered voters are contacted and asked a variety of questions. For one
# question, 60% of the voters expressed approval and 40% disapproval. Calculate a 95% confidence
# interval for the proportion expressing approval.

?prop.test()
prop_test_object <- prop.test(x = 1600*0.6,n = 1600,
                              alternative = "two.sided",conf.level = 0.95)
print(str(prop_test_object))
as.numeric(prop_test_object$conf.int)

#question 4
# A random sample of consumers are presented with two beverages in random order and asked which
# they prefer most. All the consumers expressed a preference. One beverage was preferred 85% of the
# time. Use this number to determine how large a sample of consumers would be needed to generate a
# 95% confidence interval with an overall width just less than 2% (i.e. from 84% to 86%)?

p <- 0.85  
z_score <- qnorm(0.025,mean = 0,sd = 1,lower.tail = FALSE)
sample_size <- (z_score**2)*p*(1-p)/(0.02/2)**2
round(sample_size)

#question 5
# Create boxplots and find 95% confidence intervals for the mean amount of calories in each Type of hot
# dog: beef, meat and poultry. Construct 99% one-sided lower confidence intervals for the mean amount
# of calories in each Type of hot dog: beef, meat and poultry.

hotdogs <- read.csv("hot_dogs.csv")
str(hotdogs)
summary(hotdogs)
boxplot(hotdogs$Calories ~ hotdogs$Type,main = "Calories, by hotdog type",
        ylab = "Calories")

beef <- subset(hotdogs,subset = (Type == "Beef"))
meat <- subset(hotdogs,subset = (Type == "Meat"))
poultry <- subset(hotdogs,subset = (Type == "Poultry"))

?t.test()
t.test(beef$Calories)$conf.int
with(meat, t.test(Calories)$conf.int)
t.test(poultry$Calories,conf.level = 0.95)

t.test(beef$Calories,alternative = "less",conf.level = 0.99)
t.test(meat$Calories,alternative = "less",conf.level = 0.99)
t.test(poultry$Calories,alternative = "less",conf.level = 0.99)

#question 6
# Find a 95% confidence interval for the variance in the amount of calories found for each type of hotdog:
# beef, meat and poultry.

var.conf.int <- function(x,conf.level = 0.95) {
  df <- length(x) - 1
  chilower <- qchisq((1-conf.level)/2,df,lower.tail = TRUE)
  chiupper <- qchisq((1-conf.level)/2,df,lower.tail = FALSE)
  v <- var(x)
  return(c(df*v/chiupper,df*v/chilower))
}

var.conf.int(beef$Calories)
var.conf.int(meat$Calories)
var.conf.int(poultry$Calories)
