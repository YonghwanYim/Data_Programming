# Subject : Data Programming HW 3
# Author : Yim Yonghwan
# Final update : 2018.11.05

########################################################################################
# Ex 1-1.
data1 <- c(24, 28, 29, 33, 35, 36, 37, 39, 40, 41, 44, 45, 46, 48, 49,
           50, 51, 53, 55, 56, 57, 57, 59, 60, 61, 62, 64, 64, 64, 66,
           67, 67, 68, 68, 69, 70, 72, 72, 73, 74, 75, 75, 76, 77, 78,
           79, 79, 80, 83, 83, 85, 85, 86, 88, 88, 90, 92, 94, 95, 99); data1
mean(data1)
median(data1)
quantile(data1)

# Ex 1-2.
var(data1)
sqrt(var(data1))

########################################################################################
# Ex 2-1.
data2 <- c(10.4, 9.8, 12.9, 7.6, 9.8, 11.0, 8.5, 9.6, 17.9, 10.1, 13.2, 9.4,
           5.5, 14.2, 9.1, 9.7, 9.3, 6.9, 6.1, 6.3, 7.4, 9.9, 17.8, 11.6); data2
summary(data2)
var(data2)
sqrt(var(data2))

# Ex 2-2.
install.packages("moments")
library(moments)
skewness(data2)
kurtosis(data2)

########################################################################################
# Ex 3-1.
Year <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
          1, 1, 2, 1, 2, 1, 2, 2, 2, 1); Year
Sex <- c('M', 'F', 'F', 'M', 'M', 'F', 'F', 'F', 'M', 'M',
         'F', 'F', 'M', 'M', 'M', 'F', 'M', 'F', 'F', 'M'); Sex
Height <- c(169.2, 159.8, 162.5, 178.8, 182.8, 155.9, 152.9, 155.9, 169.2, 170.5,
            166.9, 155.9, 161.3, 175.8, 162.5, 159.2, 178.3, 160.6, 155.6, 178.8); Height 
Weight <- c(65.6, 44.5, 49.8, 73.5, 77.7, 50.8, 43.8, 54.8, 59.2, 62.1,
            51.8, 49.1, 57.1, 66.3, 59.9, 40.9, 72.5, 53.5, 42.8, 78.1); Weight
sample03 <- data.frame(cbind(Year, Sex, Height, Weight), stringsAsFactors = F); sample03
sample03$Height <- as.numeric(sample03$Height)
sample03$Weight <- as.numeric(sample03$Weight)

sort_sample03 <- sample03[order(sample03$Year, sample03$Sex,
                                -sample03$Height, -sample03$Weight), ]; sort_sample03

# Ex 3-2.
table(sample03$Year, sample03$Sex)

########################################################################################
# Ex 4-1.
Year <- c(1, 1, 3, 2, 2, 3, 1, 2, 3, 3,
          3, 1, 1, 2, 2, 3, 2, 2, 3, 3,
          3, 1, 1, 1, 2, 1, 1, 2, 3, 3); Year
College <- c('Eng', 'Lib', 'Bus', 'Law', 'Eng', 'Bus', 'Bus', 'Eng', 'Law', 'Law',
             'Lib', 'Eng', 'Bus', 'Law', 'Eng', 'Eng', 'Lib', 'Eng', 'Eng', 'Bus',
             'Bus', 'Law', 'Eng', 'Eng', 'Lib', 'Law', 'Eng', 'Bus', 'Lib', 'Law'); College
Sex <- c('M', 'F', 'F', 'M', 'F', 'M', 'M', 'F', 'F', 'F',
         'M', 'M', 'F', 'M', 'M', 'M', 'M', 'F', 'F', 'M',
         'M', 'F', 'F', 'F', 'F', 'M', 'M', 'F', 'F', 'F'); Sex
Average <- c(2.85, 3.25, 3.82, 3.12, 3.58, 3.01, 2.62, 1.95, 2.02, 3.52,
             3.92, 3.12, 2.93, 1.51, 4.02, 3.11, 3.78, 4.25, 2.34, 1.75,
             3.44, 2.56, 1.90, 3.34, 4.06, 2.55, 4.14, 3.94, 1.85, 3.45); Average
Age <- c(20, 19, 25, 21, 23, 19, 25, 24, 22, 21,
         20, 21, 22, 23, 24, 22, 23, 24, 25, 22,
         21, 23, 24, 20, 19, 24, 21, 25, 21, 22); Age
sample04 <- data.frame(cbind(Year, College, Sex, Average, Age), stringsAsFactors = F); sample04
sample04$Average <- as.numeric(sample04$Average)
sample04$Age <- as.numeric(sample04$Age)

sample04_1 <- sample04[sample04$Year == 1, ]; sample04_1
sample04_2 <- sample04[sample04$Year == 2, ]; sample04_2
sample04_3 <- sample04[sample04$Year == 3, ]; sample04_3

boxplot(sample04$Average ~ sample04$Year,
        main = "Box Plot",
        xlab = "Year",
        ylab = "Average Grade")

# Ex 4-2.
sample04_4 <- sample04[sample04$Sex == 'F', ]; sample04_4
sample04_5 <- sample04[sample04$Sex == 'M', ]; sample04_5

hist(sample04_4$Age,
     main = "Histogram - Female",
     breaks = seq(15, 30, 1),
     freq = FALSE,
     xlab = "Age")
hist(sample04_5$Age,
     main = "Histogram - Male",
     breaks = seq(15, 30, 1),
     freq = FALSE,
     xlab = "Age")

# Ex 4-3.
hist(sample04_4$Age,
     main = "Histogram - Female",
     breaks = seq(15, 30, 1),
     freq = FALSE,
     xlab = "Age")
lines(density(sample04_4$Age))

hist(sample04_5$Age,
     main = "Histogram - Male",
     breaks = seq(15, 30, 1),
     freq = FALSE,
     xlab = "Age")
lines(density(sample04_5$Age))

########################################################################################
# Ex 5-1.
Index <- c(1:20); Index
Sex <- c('M', 'M', 'F', 'F', 'M', 'M', 'F', 'M', 'M', 'M',
         'F', 'F', 'M', 'F', 'M', 'M', 'F', 'F', 'M', 'F'); Sex
TV <- c(50, 45, 35, 40, 55, 100, 95, 25, 50, 70,
        50, 45, 50, 600, 30, 120, 40, 55, 60, 45); TV
Newspaper <- c(25, 30, 35, 30, 15, 60, 45, 40, 35, 10,
               40, 50, 20, 75, 45, 30, 20, 15, 30, 50); Newspaper
sample05 <- data.frame(cbind(Index, Sex, TV, Newspaper), stringsAsFactors = F); sample05

plot(sample05$Index, sample05$TV,
     main = "Index - TV Plot",
     xlab = "Index",
     ylab = "TV")

# Ex 5-2.
sample05$Newspaper <- as.numeric(sample05$Newspaper) 
stem(sample05$Newspaper)

########################################################################################
# Ex 6-1.
sample06 <- matrix(c(7, 11, 5, 4, 14,
                     13, 13, 11, 8, 12,
                     9, 12, 8, 11, 13,
                     20, 8, 8, 6, 7),
                   nrow = 4,
                   ncol = 5,
                   byrow = TRUE); sample06
rownames(sample06) <- c(1:4); sample06
colnames(sample06) <- c('A', 'B', 'C', 'D', 'E'); sample06

# Ex 6-2.
x <- apply(sample06, 1, sum); x

# Ex 6-3.
y <- apply(sample06, 2, sum); y

# Ex 6-4.
par(mfrow = c(2, 2))
pie(x, main = "Pie Chart of Low")
pie(y, main = "Pie Chart of Col")
plot(x, type = 'l', main = 'Graph x')
plot(y, type = 'l', main = 'Graph y')
par(mfrow = c(1, 1))

# Ex 6-5.
barplot(sample06)

# Ex 6-6.
barplot(sample06, beside = TRUE)

########################################################################################
# Ex 7.
sample07 <- sample(1:100, size = 50, replace = TRUE); sample07
stem(sample07, scale = 0.5)
stem(sample07, scale = 1.0)

########################################################################################
# Ex 8.
summary(iris)
setosa <- iris[iris$Species == 'setosa', ]; setosa
versicolor <- iris[iris$Species == 'versicolor', ]; versicolor
virginica <- iris[iris$Species == 'virginica', ]; virginica

cor(setosa[,1:4])

########################################################################################
# Ex 9-1.
sample09 <- read.csv("C:/Users/uko99/Desktop/hw3_9.csv", header = T); sample09
tapply(sample09$Age, sample09$War, mean)
tapply(sample09$Age, sample09$War, sum)
tapply(sample09$Age, sample09$War, var)
tapply(sample09$Age, sample09$War, sd)

# Ex 9-2.
boxplot(sample09$Age ~ sample09$War,
        main = "Life comparison before and after war",
        xlab = 'Before and after the war',
        ylab = 'Age')

# Ex 9-3.
table(sample09$War)
table(sample09$AgeGroup)

# Ex 9-4.
hist(sample09$Age,
     main = "Histogram - Age",
     breaks = seq(10, 90, 5),
     freq = FALSE,
     xlab = 'Age')
lines(density(sample09$Age))
