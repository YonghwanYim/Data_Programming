# Subject : Data Programming HW 6
# Author : Yim Yonghwan
# Final update : 2018.12.01

#######################################################################################
# Ex 1.
age <- c(2:16); age
maxfreq <- c(5.33, 5.75, 5.8, 5.6, 6, 5.78, 5.9, 6.23,
             7.28, 7.06, 7.6, 7.45, 8.23, 8.5, 9.38); maxfreq

cor.test(age, maxfreq)


#######################################################################################
# Ex 2-1.
total <- c(6.4, 6.8, 6.4, 5.9, 5.7, 5.9, 6.9, 7.4, 6.5, 5.7, 6.1, 5.6,
          4.9, 4.2, 4.7, 5.1, 5.1, 4.3, 3.9, 3.5, 4.7, 4.4, 5.6, 5.9)
industrial_product <- c(4.3, 1.8, 4.9, 6.4, 5.7, 5.9, 6.4, 7.5, 7.1, 6.1, 6.1, 6.2,
                        5.2, 4.3, 3.9, 3.8, 4.9, 4.6, 4.2, 4.2, 5.8, 6.2, 6.1, 6.9)

plot(total, industrial_product)

# Ex 2-2.
cor.test(total, industrial_product)


#######################################################################################
# Ex 3-1.
GPA <- c(3.73, 3.92, 3.45, 3.32, 3.54, 3.3, 3.2,
         3.3, 3.01, 3.77, 3.55, 3.5, 3.3, 3.5, 3.44); GPA
salary <- c(2400, 2550, 3200, 3000, 2460, 1780, 2420,
            2200, 3600, 4400, 2500, 2800, 2000, 3000, 2500); salary

plot(GPA, salary)

# Ex 3-2.
fit <- lm(salary ~ GPA)
summary(fit)

# Ex 3-3.
# 절편과 기울기의 추정치가 유의수준 0.05에서 모두 유의하지 않음.
# 또한 Adjusted R-squared : -0.05로 설명력이 현저하게 떨어짐.
# 즉, 성적 평점과 연봉간에 선형 상관관계가 있다고 할 수 없음.


#######################################################################################
# Ex 4-1.
library(MASS)

TV <- c(2.07, 3.11, 4.00, 4.98, 6.02, 7.04, 7.96, 
        8.93, 10.01, 11.08, 12.07, 13.00, 13.98, 15.01, 16.00); TV
radio <- c(13.08, 14.10, 15.04, 16.07, 17.05, 18.03, 18.97,
           19.94, 21.05, 22.01, 22.94, 23.90, 24.92, 26.03, 27.01); radio
internet <- c(5.22, 6.21, 7.03, 8.05, 9.08, 10.04, 10.90, 11.75,
              12.90, 14.04, 14.80, 16.00, 16.83, 17.92, 18.99); internet
sales <- c(16.98, 18.37, 18.86, 12.66, 25.70, 28.69, 31.97, 36.56,
           34.41, 35.29, 39.35, 38.36, 43.22, 42.33, 47.83); sales

sample4 <- data.frame(TV, radio, internet, sales)
plot(sample4)
cor(sample4)

# Stepwise selection
bothStep <- stepAIC(lm(sales ~ TV + radio + internet), 
                    direction = 'both')

# forward selection
forwardStep <- stepAIC(lm(sales ~ 1), 
                       direction = 'forward', 
                       scope = ~TV + radio + internet)

# Ex 4-2.
fit <- lm(sales ~ TV + internet)
summary(fit)

fit2 <- lm(sales ~ TV)
summary(fit2)


#######################################################################################
# Ex 5-1.
entrance <- c(170, 165, 128, 154, 167, 159, 156, 120, 185, 176,
              198, 180, 128, 152, 105, 126, 146, 172, 187, 134); entrance
statistics <- c(700, 654, 568, 512, 745, 634, 557, 486, 760, 700,
                782, 800, 470, 542, 448, 628, 490, 618, 755, 540); statistics
plot(entrance, statistics)

# Ex 5-2.
cor.test(entrance, statistics)

# Ex 5-3 ~ 5-4.
fit <- lm(statistics ~ entrance)
summary(fit)


#######################################################################################
# Ex 6-1.
advert_cost <- c(2.00, 2.68, 4.01, 4.78, 5.99, 6.99, 8.00, 9.01, 10.01, 11.31,
                 12.20, 13.50, 14.00, 14.80, 16.75, 17.10, 17.80, 19.50, 19.89) 
sales <- c(12.44, 12.76, 12.97, 13.18, 13.41, 13.73, 13.82, 14.03, 14.15, 14.28,
           14.53, 14.56, 14.81, 14.76, 14.98, 15.15, 15.27, 15.33, 15.42)
plot(advert_cost, sales)

# Ex 6-2 ~ 6-3.
root_x <- sqrt(advert_cost)
plot(root_x, sales)
fit <- lm(sales ~ root_x)
summary(fit)


#######################################################################################
# Ex 7-1 ~ 7-4.
head(cars)
plot(cars$speed, cars$dist)
fit <- lm(cars$dist ~ cars$speed)
summary(fit)


#######################################################################################
# Ex 8-1. Shapiro test
reg <- residuals(fit)
shapiro.test(reg)

# Ex 8-2. Durbin-Watson test
install.packages("lmtest")
library(lmtest)
dwtest(fit)

# Ex 8-3.
plot(fit)


#######################################################################################
# Ex 9.
Year <- c(1984:1997); Year
T <- c(1:14); T
Yt <- c(27, 47, 71, 103, 204, 397, 800, 1662, 2719, 4718, 9600, 16410, 28900, 45700); Yt

# "Yt = a * e^(bT)" -> ln(Yt) = b*T + ln(a)
lnYt <- log(Yt); lnYt
plot(T, lnYt)
fit <- lm(lnYt ~ T)
summary(fit)

# ln(a) = 2.52371, b = 0.59388
a <- exp(2.52371); a


#######################################################################################
# Ex 10-1.
head(swiss)
class(swiss)
fit1 <- lm(swiss$Fertility ~ swiss$Agriculture + swiss$Examination +
            swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
summary(fit1)

# Ex 10-2.
fit2 <- lm(swiss$Fertility ~ swiss$Agriculture + swiss$Education +
            swiss$Catholic + swiss$Infant.Mortality)
summary(fit2)

# Ex 10-3.
# Stepwise selection
bothStep <- stepAIC(fit1, direction = 'both')

# forward selection
forwardStep <- stepAIC(lm(swiss$Fertility ~ 1), 
                       direction = 'forward', 
                       scope = ~swiss$Agriculture +
                         swiss$Examination +
                         swiss$Education + 
                         swiss$Catholic + 
                         swiss$Infant.Mortality)
