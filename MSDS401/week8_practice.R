getwd()
setwd("C:/Users/JM933JS/Downloads/MSDS/Stat/r_code/MSDS401")

#question 1
# Use the tableware.csv data to test the hypothesis that the mean RATE for the five levels of TYPE are
# equal. Test the hypothesis using a 0.05 significance level. Plot means and confidence intervals of RATE
# for each level of TYPE (Use the example given in Davies Chapter 9.3 One-way ANOVA (pp. 218-223)).
tableware <- read.csv("tableware.csv")
str(tableware)
summary(tableware)

?aov()
rate_anova <- aov(RATE ~ TYPE,data = tableware)
summary(rate_anova)

rate_intervals <- TukeyHSD(rate_anova,conf.level = 0.95)
rate_intervals

par(mfrow = c(1,1),las = 2)
plot(rate_intervals)
?plot()

library(plyr)
RATEbyType <- ddply(tableware, "TYPE", summarize,
                    RATE.mean=mean(RATE), RATE.sd=sd(RATE),
                    Length=NROW(RATE),
                    tfrac=qt(p=.975, df=Length-1),
                    Lower=RATE.mean - tfrac*RATE.sd/sqrt(Length),
                    Upper=RATE.mean + tfrac*RATE.sd/sqrt(Length)
)

RATEbyType

?lm()
RATE_lm <- lm(RATE ~ TYPE, data = tableware)
RATE_lm
summary(RATE_lm)

#question 2
# Use the tableware.csv data to test the hypothesis that the mean PRICE for the five levels of TYPE are
# equal. Test the hypothesis using a 0.05 significance level. Plot means and confidence intervals of PRICE
# for each level of TYPE (Use the example given in Davies Chapter 9.3 One-way ANOVA (pp. 218-223)).

price_anova <- aov(PRICE ~ TYPE,data = tableware)
price_anova
summary(price_anova)

aggregate(PRICE ~ TYPE ,data = tableware,var)
aggregate(PRICE ~ TYPE ,data = tableware,mean)

price_intervales <- TukeyHSD(price_anova,conf.level = 0.95)
price_intervales

par(mfrow = c(1,1),las = 2)
plot(price_intervales)

boxplot(PRICE ~ TYPE,data = tableware)

#question 3
# Use the hot_dogs.csv data. Perform a one-way AOV by Type on Calories and also Sodium (Use the
# example given in Davies Chapter 9.3 One-way ANOVA (pp. 218-223)). Use Tukey’s Honest Significant
# Difference Test if the F-test is significant. Generate boxplots.

hotdogs <- read.csv("hot_dogs.csv")
str(hotdogs)
summary(hotdogs)

with(hotdogs,boxplot(Calories ~ Type,main = "Calories"))
with(hotdogs,boxplot(Sodium ~ Type,main = "Sodium"))

calories_anova <- aov(Calories ~ Type,data = hotdogs)
summary(calories_anova)

sodium_anova <- aov(Sodium ~ Type,data = hotdogs)
summary(sodium_anova)

calories_interval <- TukeyHSD(calories_anova,conf.level = 0.95)
calories_interval
plot(calories_interval)

sodium_interval <- TukeyHSD(sodium_anova,conf.level = 0.95)
sodium_interval
plot(sodium_interval)


x <- c(2,5,8,10,12)
y <- c(7,11,13,20,24)

line_points <- data.frame(cbind(x,y))
line_points

linear_line <- lm(y ~ x,data = line_points)
summary(linear_line)

pf(5.17,3,16,lower.tail = FALSE)


n1 <- 35
xbar1 <- 19.4
s1 <- 1.4
n2 <- 40
xbar2 <- 15.1
s2 <- 1.3

sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
sp

margin <- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
margin

lowerinterval <- (xbar1-xbar2) - margin
lowerinterval

upperinterval <- (xbar1-xbar2) + margin
upperinterval


p1 <- 78/400
q1 <- 1-p1
n1 <- 400

p2 <- 92/500
q2 <- 1-p2
n2 <- 500

z <- qnorm(0.95,lower.tail = TRUE)
z
margin_p <- p1 - p2
margin_p
error <- z*sqrt(p1*q1/n1 + p2*q2/n2)
error
lowerinterval <- margin_p - error
lowerinterval

upperinterval <- margin_p + error
upperinterval

prop.test(x = c(78,92),n = c(400,500),conf.level = 0.90)
