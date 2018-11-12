# Subject : Data Programming HW 2
# Author : Yim Yonghwan
# Final update : 2018.10.28

########################################################################################
# Ex 1-1.
ID <- c(001, 002, 003, 004, 005); ID
NAME <- c('Hong', 'Kim', 'Lee', 'Park', 'Jung'); NAME
HEIGHT <- c(160, 175, 165, 180, 170); HEIGHT  # Unit : cm
sample01 <- data.frame(cbind(ID, NAME, HEIGHT), stringsAsFactors = T); sample01

ID <- c(001, 002, 003, 006, 007); ID
NAME <- c('Hong', 'Kim', 'Lee', 'Oh', "Lee"); NAME
WEIGHT <- c(55, 75, 60, 75, 60); WEIGHT  # Unit : kg
sample02 <- data.frame(cbind(ID, NAME, WEIGHT), stringsAsFactors = T); sample02

# Ex 1-2.
merge(sample01, sample02, by = "ID")

# Ex 1-3.
merge(sample01, sample02, by = "ID", all = TRUE)

# Ex 1-4.
merge(sample01, sample02, by = "ID", all.x = TRUE)

########################################################################################
# Ex 2-1.
ID <- c(1001:1010); ID
NAME <- c('Wang', 'Lee', 'Kim', 'Song', 'Gong', 'Park', 'Kim', 'Jo', 'Lee', 'Gi'); NAME
SEX <- c('F', 'M', 'F', 'M', 'F', 'M', 'F', 'M', 'F', 'M'); SEX
ENG <- c(95, 80, 90, 70, 65, 80, 95, 45, 30, 70); ENG
MATH <- c(80, 90, 90, 95, 75, 45, 100, 95, 70, 75); MATH
sample03 <- data.frame(cbind(ID, NAME, SEX, ENG, MATH), stringsAsFactors = F); sample03
class(sample03$ENG)
sample03$ENG <- as.numeric(sample03$ENG);
sample03$MATH <- as.numeric(sample03$MATH); 
sample03$Average <- (sample03$ENG + sample03$MATH) / 2; sample03

# Ex 2-2.
sample03[sample03$Average >= 80, ]

# Ex 2-3.
tapply(sample03$MATH, sample03$SEX, mean)

########################################################################################
# Ex 3-1.
INDEX <- c(1:5); INDEX
SEX <- c('M', 'F', 'M', 'F', 'F'); SEX
AGE <- c("20~", "41~60", "21~40", "60~", "60~"); AGE
SATISFACTION <- c("strong negative", "positive", "positive",
                  "strong positive", "neutral"); SATISFACTION
sample04 <- data.frame(cbind(INDEX, SEX, AGE, SATISFACTION), stringsAsFactors = F); sample04
sample04$SCORE [SATISFACTION == "strong positive"] <- 5
sample04$SCORE [SATISFACTION == "positive"] <- 4
sample04$SCORE [SATISFACTION == "neutral"] <- 3  
sample04$SCORE [SATISFACTION == "negative"] <- 2  
sample04$SCORE [SATISFACTION == "strong negative"] <- 1; sample04

# Ex 3-2.
aggregate(SCORE ~ SEX, sample04, mean)

########################################################################################
# Ex 4-1.
sample05 <- matrix(c(9,10,8,8,8,9,8,6,
                     7,10,10,7,6,9,9,4,
                     9,10,10,8),
                   5, 4, byrow = T); sample05
colnames(sample05) <- c('r1', 'r2', 'r3', 'r4'); sample05
apply(sample05, 2, mean)

# Ex 4-2.
rownames(sample05) <- c('stu1', 'stu2', 'stu3', 'stu4', 'stu5')
apply(sample05, 1, mean)

########################################################################################
# Ex 5.
x <- c(rep(1, 300), rep(2, 200), rep(3, 100), rep(4, 300), rep(5, 100))
for(i in 1:5) {
  y <- sample(x, size = 10)
  print(y)
  print(mean(y))
  }

########################################################################################
# Ex 6.
fresh <- c(155, 162, 169, 171, 159, 168, 160, 168, 159, 175,
           154, 173, 165, 158, 160, 161, 165, 172, 174, 167); fresh
quantile(fresh, c(0.4, 0.5, 0.8, 0.95))

########################################################################################
# Ex 7.
matrix1 <- data.frame(rbind(c(95, 75, 65), c(85, 65, 55), c(84, 65, 55)),
                      stringsAsFactors = F); matrix1  # use data frame
solve(matrix1)

########################################################################################
# Ex 8.
sample06 <- c(10, 20, 40, 50, 70, 80, 90, 100, 110,
              140, 170, 190, 250, 400, 410, 470); sample06 
quantile(sample06, c(0, 0.25, 0.5, 0.75, 1.0))

########################################################################################
# Ex 9-1.
library(MASS)
sample07 <- split(iris, iris$Species); sample07

# Ex 9-2.
list1 <- list(setosa = sample07[[1]]$Sepal.Width,
              versicolor = sample07[[2]]$Sepal.Width,
              virginica = sample07[[3]]$Sepal.Width); list1
lapply(list1, mean) 

# Ex 9-3.
tapply(iris$Sepal.Length, iris$Species, mean)

# Ex 9-4.
sample08 <- iris[, c(-2, -4, -5)]; sample08

########################################################################################
# Ex 10.
sample09 <- data.frame(num = c(1:100)); sample09
sample09$class [(sample09$num %% 2) == 1] <- 'odd'   # odd number
sample09$class [(sample09$num %% 2) == 0] <- 'even'  # even number
tapply(sample09$num, sample09$class, sum)

########################################################################################
# Ex 11-1.
mtcars
aggregate(mpg ~ am + cyl, mtcars, mean)

# Ex 11-2.
sample10 <- data.frame(aggregate(mpg ~ am + cyl, mtcars, mean))
sample10$wt <- aggregate(wt ~ am + cyl, mtcars, mean)$wt
sample10

# Ex 11-3.
table(mtcars$cyl)

# Ex 11-4.
summary(mtcars$mpg)

########################################################################################
# Ex 12-1.
array1 <- array(c(80, 76, 98, 87, 76, 54, 87, 90, 64, 79),
                dim = c(5,2), 
                dimnames = list(c('stu1', 'stu2', 'stu3', 'stu4', 'stu5'),
                                c("EngScore", "MathScore"))); array1

# Ex 12-2.
margin.table(array1, 1)

# Ex 12-3.
margin.table(array1, 2)

# Ex 12-4.
margin.table(array1)

########################################################################################
# Ex 13-1.
current_sum <- 0
i <- 1
while((current_sum + i) < 1000)
{
  current_sum <- current_sum + i
  i <- i + 1
  print(current_sum); print(i-1)
}

# Ex 13-2.
current_sum <- 0
i <- 1
repeat
{
  current_sum <- current_sum + i
  i <- i + 1
  if(current_sum > 1000) break
  print(current_sum); print(i-1)
}




