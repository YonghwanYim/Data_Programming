# Subject : Data Programming HW 5
# Author : Yim Yonghwan
# Final update : 2018.11.26

#######################################################################################
# Ex 1.
data1 <- c(5.6, 7.9, 4.5, 8.9, 5.4, 8.7, 4.6, 6.5, 6.3, 7.7,
           10.5, 11.1, 3.9, 6.3, 8.5, 5.9, 5.5, 6.8, 5.2, 7.1); data1
t.test(data1, conf.level = 0.95)


#######################################################################################
# Ex 2.
data2 <- c(68.9, 75.8, 61.6, 58.8, 72.6, 78.9, 55.5, 71.7, 62.6, 63.8,
           62.6, 72.9, 66.8, 62.4, 53.1, 72.0, 69.1, 57.9, 66.1, 64.7,
           72.6, 79.5, 65.1, 57.6, 62.7, 61.3, 58.3, 51.7, 54.8, 59.2); data2
t.test(data2, conf.level = 0.90)


#######################################################################################
# Ex 3.
data3 <- c(23, 27, 27, 29, 32, 35, 35, 39, 41, 42,
           47, 47, 50, 51, 53, 53, 57, 60, 62, 65); data3
dx3 <- data3 - 40; dx3
t.test(dx3, alternative = 'greater')


#######################################################################################
# Ex 4.
data4_A <- c(12.9, 8.9, 11.1, 13.8, 10.9, 7.9, 9.7, 8.7, 8.8, 13.5); data4_A
data4_B <- c(13.5, 9.5, 12.0, 13.3, 11.6, 7.2, 10.3, 9.9, 9.8, 13.8); data4_B
var.test(data4_A, data4_B)
t.test(data4_A, data4_B, paired = F, var.equal = T)


#######################################################################################
# Ex 5. Equal variance.
n_A <- 15; xbar_A <- 83; s_A <- 5
n_B <- 12; xbar_B <- 87; s_B <- 4
DOF <- n_A + n_B - 2; DOF  # Degree of Freedom
t <- qt(0.975, DOF); t

# 95% Confidence Interval
sp <- sqrt(((n_A - 1) * (s_A)^2 + (n_B - 1) * (s_B)^2) / DOF); sp
upper <- (xbar_B - xbar_A) + t * sp * sqrt((1 / n_A) + (1 / n_B)); upper
lower <- (xbar_B - xbar_A) - t * sp * sqrt((1 / n_A) + (1 / n_B)); lower


#######################################################################################
# Ex 6. Different variance.
n_A <- 25; xbar_A <- 10; s_A <- 1.3
n_B <- 25; xbar_B <- 11; s_B <- 1.2

DOF <- (s_A^2 / n_A + s_B^2 / n_B)^2 / 
  ((s_A^2 / n_A)^2 / (n_A - 1) + (s_B^2 / n_B)^2 / (n_B - 1)); DOF # Degree of Freedom

t <- qt(0.975, DOF); t

# 95% Confidence Interval
upper <- (xbar_B - xbar_A) + t * sqrt((s_A^2 / n_A) + (s_B^2 / n_B)); upper
lower <- (xbar_B - xbar_A) - t * sqrt((s_A^2 / n_A) + (s_B^2 / n_B)); lower


#######################################################################################
# Ex 7.
data7_A <- c(102, 86, 98, 109, 92); data7_A
data7_B <- c(81, 165, 97, 134, 92, 87, 114); data7_B
var.test(data7_A, data7_B)


#######################################################################################
# Ex 8. Contingency Table
install.packages("gmodels")
library(gmodels)

preference <- c(75, 100, 75, 80, 56, 114); preference
subject <- rep(c('1.Kor', '2.Math', '3.Eng'), 2); subject
sex <- rep(c('1.Male', '2.Female'), each = 3); sex

table8 <- xtabs(preference ~ sex + subject); table8
CrossTable(table8, expected = T)  # Test of independence


#######################################################################################
# Ex 9.
n <- 500; x <- 86; p_hat <- x / n;
z <- qnorm(0.95); z

upper <- p_hat + z * sqrt((p_hat * (1 - p_hat)) / n); upper
lower <- p_hat - z * sqrt((p_hat * (1 - p_hat)) / n); lower


#######################################################################################
# Ex 10.
n_Y <- 60; xbar_Y <- 85.3; ss_Y <- 6.2
n_N <- 60; xbar_N <- 89.9; ss_N <- 7.1

cv <- qf(0.95, n_N - 1, n_Y - 1); cv  # 'No' variance > 'Yes' variance
f <- ss_N / ss_Y; f
p <- pf(f, n_N - 1, n_Y - 1); p
p_value <- 1 - p; p_value

result <- c(cv, f, p_value)
names(result) <- c('cv', 'F', 'P-value')
result


#######################################################################################
# Ex 11-1.
data11 <- c(28.9, 32.4, 29.8, 30.6, 27.8, 29.4, 31.3); data11
sd <- 1.5; sd
n <- length(data11); n
xbar <- mean(data11); xbar
z <- qnorm(0.95); z

# 90% Confidence Interval
upper <- xbar + z * sd / sqrt(n); upper
lower <- xbar - z * sd / sqrt(n); lower

# Ex 11-2.
s <- sd(data11); s
t <- qt(0.95, n - 1); t

# 90% Confidence Interval
upper <- xbar + t * s / sqrt(n); upper
lower <- xbar - t * s / sqrt(n); lower


#######################################################################################
# Ex 12.
n <- 100; x <- 3; p_hat <- x / n;
z <- qnorm(0.975); z

# 95% Confidence Interval
upper <- p_hat + z * sqrt((p_hat * (1 - p_hat)) / n); upper
upper <- p_hat - z * sqrt((p_hat * (1 - p_hat)) / n); upper


#######################################################################################
# Ex 13-1.
n_fiv <- 30; xbar_fiv <- 107; ss_fiv <- 2.5
n_six <- 35; xbar_six <- 112; ss_six <- 3.2

# F-test
f <- ss_fiv / ss_six; f
p <- pf(f, n_fiv - 1, n_six - 1); p
p_value <- 1 - p; p_value

# 95% Confidence Interval - Equal Variance (by Result of F-test)
DOF <- n_fiv + n_six - 2; DOF  # Degree of Freedom
t <- qt(0.975, DOF); t
sp <- sqrt(((n_fiv - 1) * (ss_fiv) + (n_six - 1) * (ss_six)) / DOF); sp
upper <- (xbar_six - xbar_fiv) + t * sp * sqrt((1 / n_fiv) + (1 / n_six)); upper
lower <- (xbar_six - xbar_fiv) - t * sp * sqrt((1 / n_fiv) + (1 / n_six)); lower

# Ex 13-2.
t_test_statistic <- (xbar_six - xbar_fiv) / (sp * sqrt((1 / n_fiv) + (1 / n_six))) 
cv <- qt(0.95, DOF)
result <- c(cv, t_test_statistic)
names(result) <- c('cv', 't')
result


#######################################################################################
# Ex 14-1. Equal Variance - 90% Confidence Interval
n_A <- 20; xbar_A <- 27.8; s_A <- 1.5 
n_B <- 20; xbar_B <- 25.4; s_B <- 2.1
DOF <- n_A + n_B - 2; DOF
t <- qt(0.95, DOF); t
sp <- sqrt(((n_A - 1) * (s_A)^2 + (n_B - 1) * (s_B)^2) / DOF); sp

upper <- (xbar_A - xbar_B) + t * sp * sqrt((1 / n_A) + (1 / n_B)); upper
lower <- (xbar_A - xbar_B) - t * sp * sqrt((1 / n_A) + (1 / n_B)); lower

# Ex 14-2. Different Variance - 90% Confidence Interval
DOF <- (s_A^2 / n_A + s_B^2 / n_B)^2 / 
  ((s_A^2 / n_A)^2 / (n_A - 1) + (s_B^2 / n_B)^2 / (n_B - 1)); DOF # Degree of Freedom
t <- qt(0.95, DOF); t

upper <- (xbar_A - xbar_B) + t * sqrt((s_A^2 / n_A) + (s_B^2 / n_B)); upper
lower <- (xbar_A - xbar_B) - t * sqrt((s_A^2 / n_A) + (s_B^2 / n_B)); lower


#######################################################################################
# Ex 15-1. Equal Variance
only_child <- c(105, 110, 120, 90, 100, 136, 125, 188, 105, 118, 
                97, 109, 103, 110, 115, 99); only_child
brother <- c(95, 120, 110, 115, 116, 108, 125, 98, 85); brother

t.test(only_child, brother, var.equal = T)

# Ex 15-2. Different Variance
t.test(only_child, brother, var.equal = F)

