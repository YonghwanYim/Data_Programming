# Subject : Data Programming HW 1 
# Author : Yim Yonghwan
# Final update : 2018.10.14

########################################################################################
# Ex 1-1.
ID <- c('001', '002', '003', '004', '005'); ID
dept <- c('stat', 'law', 'econ', 'math', 'engl'); dept
age <- c(22, 21, 23, 27, 21); age
ex1 <- c(9, 10, 10, 16, 11); ex1
ex2 <- c(12, 15, 17, 17, 13); ex2
gender <- c('m', 'f', 'f', 'm', 'f'); gender

# Ex 1-2.
matrix1 <- cbind(age, ex1, ex2); matrix1

# Ex 1-3.
data1 <- data.frame(ID, dept, matrix1, gender, stringsAsFactors = F); data1
data2 <- data1[data1$gender == 'f',]; data2 # version 1
for(i in 1:length(ID)) # version 2
  if(data1[i,6] == 'f')
    print(data1[i,])

# Ex 1-4.
sample1 <- data1; sample1

# Ex 1-5.
new.row <- data.frame(ID = c('006', '007'), dept = c('history', 'chem'),
                      age = c(28, 22), ex1 = c(17, 18), ex2 = c(19, 20),
                      gender = c('m', 'f'))
sample1 <- rbind(sample1, new.row); sample1

# Ex 1-6.
print(sample1[,c(1:2,6)])

########################################################################################
# Ex 2-1.
NO <- c('11', '21', '31', '41', '51'); NO
NAME <- c('Kim', 'Hong', 'Jung', 'Nam', 'Lee'); NAME
SEX <- c('M', 'F', 'M', 'F', 'F'); SEX
AGE <- c(28, 42, 34, 21, 33); AGE
INCOME <- c(250, 280, 310, 180, 210); INCOME
sample02 <- data.frame(cbind(NO, NAME, SEX, AGE, INCOME), stringsAsFactors = F); sample02
sample02[,5] <- as.numeric(sample02[,5])  # character type -> numeric type

# Ex 2-2.
sumOfResult <- sum(sample02[,5]); sumOfResult

########################################################################################
# Ex 3-1.
NAME <- c('Park', 'Lee', 'Kim', 'Baek', 'Seo'); NAME
SEX <- c('m', 'f', 'm', 'm', 'f'); SEX
WEIGHT <- c(62, 43, 77, 68, 54); WEIGHT # unit : kg
HEIGHT <- c(158, 155, 175, 166, 158); HEIGHT # unit : com
sample03 <- data.frame(cbind(NAME, SEX, WEIGHT, HEIGHT), stringsAsFactors = F); sample03
sample03[,3] <- as.numeric(sample03[,3])
sample03[,4] <- as.numeric(sample03[,4])

# Ex 3-2.
BMI <- {sample03$WEIGHT / (sample03$HEIGHT / 100)^2}; BMI
sample03 <- cbind(sample03, BMI); sample03

########################################################################################
# Ex 4-1.
score_matrix <- matrix(c(9, 10, 8, 8, 8, 9, 8, 6,
                         7, 10, 10, 7, 6, 9, 9, 4,
                         9, 10, 10, 8), 5, 4, byrow = T)
colnames(score_matrix) <- c('r1', 'r2', 'r3', 'r4')
rownames(score_matrix) <- c('Park', 'Lee', 'Kim', 'Baek', 'Seo')
score_matrix

for(i in 1:length(score_matrix[1,]))
  print(mean(score_matrix[,i]))

# Ex 4-2.
for(i in 1:length(score_matrix[,1]))
  print(mean(score_matrix[i,]))

########################################################################################
# Ex 5-1.
f1 <- function(x) {1 / (x^2 + 3)}
integrate(f1, 0, Inf)

# Ex 5-2.
f2 <- function(x) {exp(-abs(x^2 + 5))}
integrate(f2, -Inf, 10)

########################################################################################
# Ex 6-1.
ID <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110); ID
AgeGroup <- c(1, 2, 4, 4, 4, 5, 3, 5, 5, 2); AgeGroup
Gender <- c('M', 'M', 'F', 'M', 'M', NA, 'M', 'F', 'M', 'F'); Gender
Item1 <- c(1, 3, 3, 3, 1, 2, 1, 1, 1, 2); Item1
Item2 <- c(1, 3, 3, 3, 1, NA, 1, 1, 1, 3); Item2
Item3 <- c(1, 3, 1, 2, 1, NA, 1, 1, 3, 2); Item3
Company <- data.frame(cbind(ID, AgeGroup, Gender, Item1, Item2, Item3), stringsAsFactors = F); Company

# Ex 6-2.
CompanyA1 <- Company[Company$AgeGroup == 4, ]; CompanyA1

# Ex 6-3.
CompanyA2 <- Company[(Company$AgeGroup == 4) |
                       (Company$AgeGroup == 5), ]; CompanyA2

# Ex 6-4.
CompanyA3 <- Company[Company$Gender != 'M', ]; CompanyA3

# Ex 6-5.
Company$AgeGroup <- as.integer(Company$AgeGroup)
CompanyA4 <- Company[(Company$AgeGroup >= 3) &
                       (Company$Gender == 'F'), ]; CompanyA4

# Ex 6-6.
class(Company$AgeGroup)

# Ex 6-7.
Company$AgeGroup <- as.factor(Company$AgeGroup)
class(Company$AgeGroup)
Company$AgeGroup

# Ex 6-8.
class(Company$Item1); class(Company$Item2); class(Company$Item3)
Company$Item1 <- as.numeric(Company$Item1)
Company$Item2 <- as.numeric(Company$Item2)
Company$Item3 <- as.numeric(Company$Item3)
item1_mean <- mean(Company$Item1, na.rm = TRUE); item1_mean
item2_mean <- mean(Company$Item2, na.rm = TRUE); item2_mean
item3_mean <- mean(Company$Item3, na.rm = TRUE); item3_mean

########################################################################################
# Ex 7-1.
ID <- c(1:20); ID
Age <- c(10, 10, 20, 20, 10, 20, 20, 30, 30, 10,
         10, 20, 10, 20, 30, 30, 20, 20, 30, 10); Age
Drink <- c('A', 'D', 'D', 'C', 'D', 'B', 'B', 'B', 'C', 'A',
           'D', 'D', 'C', 'D', 'B', 'A', 'C', 'C', 'A', 'B'); Drink
drink_data <- data.frame(cbind(ID, Age, Drink)); drink_data
cross_table1 <- table(drink_data$Age); cross_table1

# Ex 7-2.
cross_table2 <- table(drink_data$Age, drink_data$Drink); cross_table2

# Ex 7-3.
cross_table1 <- cross_table1 /length(drink_data$ID); cross_table1

# Ex 7-4.
cross_table2 <- cross_table2 / length(drink_data$ID); cross_table2

########################################################################################
# Ex 8-1.
ID <- c(1:20); ID
Class <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3,
           3, 3, 4, 4, 4, 4, 5, 5, 5, 5); Class
Math <- c(50, 60, 45, 30, 25, 50, 80, 90, 20, 50,
          65, 45, 46, 48, 75, 58, 65, 80, 89, 78); Math
English <- c(98, 97, 86, 98, 80, 89, 90, 78, 98, 98,
             65, 85, 98, 87, 56, 98, 68, 78, 68, 83); English
Science <- c(50, 60, 78, 58, 65, 98, 45, 25, 15, 45,
             65, 32, 65, 12, 78, 65, 98, 90, 87, 58); Science
score_data <- data.frame(cbind(ID, Class, Math, English, Science)); score_data
score_data$Class <- as.factor(score_data$Class)
score_data$Math <- as.numeric(score_data$Math)
score_data$English <- as.numeric(score_data$English)
score_data$Science <- as.numeric(score_data$Science)

class1_math_mean <- mean(score_data[score_data$Class == 1, "Math"]); class1_math_mean
class2_math_mean <- mean(score_data[score_data$Class == 2, "Math"]); class2_math_mean
class3_math_mean <- mean(score_data[score_data$Class == 3, "Math"]); class3_math_mean
class4_math_mean <- mean(score_data[score_data$Class == 4, "Math"]); class4_math_mean
class5_math_mean <- mean(score_data[score_data$Class == 5, "Math"]); class5_math_mean
# Same function (aggregate)
class_math_mean <- aggregate(Math ~ Class, score_data, mean); class_math_mean

# Ex 8-2.
mean(score_data[score_data$English >= 85, "Math"])

# Ex 8-3.
score_data[(score_data$Class == 3) & (score_data$Science >= 50), ]

########################################################################################
# Ex 9-1.
my_mtcar <- mtcars; my_mtcar
my_mtcar$wt_group [my_mtcar$wt < 3] <- 'Light'
my_mtcar$wt_group [(my_mtcar$wt >= 3) & 
                     (my_mtcar$wt <= 5)] <- 'Medium'
my_mtcar$wt_group [my_mtcar$wt > 5] <- 'Heavy'
my_mtcar

# Ex 9-2.
my_mtcar_order <- my_mtcar[order(-my_mtcar$mpg, my_mtcar$wt), ]; my_mtcar_order

########################################################################################
# Ex 10.
file_data <- read.csv("c:/Users/yh/Desktop/sample.csv", header = TRUE); file_data



