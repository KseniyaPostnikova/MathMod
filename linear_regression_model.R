#Построение моделей. Корелляция
A=1:3
B=c(3,6,7)
diff_A <- A - mean(A)
diff_B <- B - mean(B)
cov <- sum(diff_A*diff_B)/ (length(A)-1)
sq_diff_A <- diff_A^2
sq_diff_B <- diff_B^2
sd_A <- sqrt(sum(sq_diff_A)/(length(A)-1))
sd_B <- sqrt(sum(sq_diff_B)/(length(B)-1))
correlation <- cov/(sd_A*sd_B)
correlation
cor(A,B)
PE <- read.table("http://assets.datacamp.com/course/Conway/Lab_Data/Stats1.13.Lab.04.txt", header=TRUE)
library("psych")
describe(PE)
plot(PE$age~PE$activeyears)
plot(PE$endurance~PE$activeyears)
plot(PE$endurance~PE$age)
round(cor(PE[,-1]), 2)  
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
impact=read.csv("https://dl.dropboxusercontent.com/s/7ubjig9z5hmv858/impact.csv?dl=0")
describe(impact)
entirecorr <- round(cor(impact$vismem2,impact$vermem2),2)
describeBy(impact, impact$condition)
control <- subset(impact, condition=="control")
concussed <- subset(impact, condition=="concussed")
controlcorr <- round(cor(control$vismem2,control$vermem2),2)
concussedcorr <- round(cor(concussed$vismem2,concussed$vermem2),2)
correlations <- cbind(entirecorr, controlcorr, concussedcorr)
correlations
#Линейная регрессия. Теория
y <- c( 109.14, 117.55, 106.76, 115.26, 117.13, 125.39, 121.03, 114.03, 124.83, 113.92, 122.04, 109.41, 131.61, 103.93, 116.64, 117.06, 111.73, 120.41, 112.98, 101.20, 120.19, 128.53, 120.14, 108.70, 130.77, 110.16, 129.07, 123.46, 130.02, 130.31, 135.06, 129.17, 137.08, 107.62, 139.77, 121.47, 130.95, 138.15, 114.31, 134.58, 135.86, 138.49, 110.01, 127.80, 122.57, 136.99, 139.53, 127.34, 132.26, 120.85, 124.99, 133.36, 142.46, 123.58, 145.05, 127.83, 140.42, 149.64, 151.01, 135.69, 138.25, 127.24, 135.55, 142.76, 146.67, 146.33, 137.00, 145.00, 143.98, 143.81, 159.92, 160.97, 157.45, 145.68, 129.98, 137.45, 151.22, 136.10, 150.60, 148.79, 167.93, 160.85, 146.28, 145.97, 135.59, 156.62, 153.12, 165.96, 160.94, 168.87, 167.64, 154.64, 152.46, 149.03, 159.56, 149.31, 153.56, 170.87, 163.52, 150.97)
# среднее значение и станд. отклонение
c(mean(y), sd(y))
shapiro.test(y)
# графическое изображение распределения данных
library(ggplot2)
ggplot(data = data.frame(y), aes(x = y)) + geom_histogram() +
  ylab("Частота") + xlab("Давление, мм рт. ст.")
set.seed(101)
y.new.1 <- rnorm(n = 100, mean = 135.16, sd = 16.96)
set.seed(101)
y.new.2 <- 135.16 + rnorm(n = 100, mean = 0, sd = 16.96)
all(y.new.1 == y.new.2)
# формула для оценки только свободного члена
y.lm <- lm(y ~ 1)
summary(y.lm)
library(arm)
set.seed(102)
y.sim <- sim(y.lm, 5)         
str(y.sim)
y.sim@coef
y.sim@sigma
set.seed(102)
y.sim <- sim(y.lm, 1000)
y.rep <- array(NA, c(1000, 100))
for(s in 1:1000){
  y.rep[s, ] <- rnorm(100, y.sim@coef[s], y.sim@sigma[s])
}
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for(s in 1: 12){ hist(y.rep[s, ], xlab = "", ylab = "",
                      breaks = 20, main = "")}
test.IQR <- apply(y.rep, MARGIN = 1, FUN = IQR)
hist(test.IQR, xlim = range(IQR(y), test.IQR), main = "ИКР", xlab = "", ylab = "Частота", breaks = 20) 
lines(rep(IQR(y), 2), c(0, 100), col = "blue", lwd = 4)
# Значения возраста:
x <- rep(seq(16, 65, 1), each = 2)
# Объединяем значения возраста и давления крови в одну таблицу
Data <- data.frame(Age = x, BP = y)
ggplot(data = Data, aes(x = Age, BP)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_rug(color = "gray70", sides = "tr") +
  ylab("Частота") + xlab("Возраст, лет")
summary(lm(BP ~ Age, data = Data))
set.seed(101)
y <- rnorm(100, mean = 97.078 + 0.949*x, 9.563)
#Линейная регрессия. Практика
library(ggplot2)
library(dplyr)
options(stringsAsFactors = FALSE)
wages <- read.csv("https://dl.dropboxusercontent.com/s/oq4dc134y8xdyce/wages.csv?dl=0")
tbl_df(wages)
crime <- read.csv("https://dl.dropboxusercontent.com/s/cecck1nuo217zy1/crime.csv?dl=0")
tbl_df(crime)
mod = lm(tc2009 ~ low, data = crime)
tc2009 ~ low
class(tc2009 ~ low)
mod
names(mod)
summary(mod)
predict(mod)
resid(mod)
#Интерпретируем данные моделей
coef(mod)
qplot(low, predict(mod), data = crime, geom = "line")
qplot(low, tc2009, data = crime) + 
  geom_smooth(method = lm)
qplot(low, tc2009, data = crime) +
  geom_smooth(se = FALSE, method = lm)
lm(tc2009 ~ 1 + low, data = crime)
lm(tc2009 ~ low, data = crime)
qplot(low, predict(lm(tc2009 ~ 1 + low, data = crime)), data = crime, geom = "line")
lm(tc2009 ~ low - 1, data = crime)
lm(tc2009 ~ 0 + low, data = crime)
qplot(low, predict(lm(tc2009 ~ low - 1, data = crime)), data = crime, geom = "line")
hmod <- lm(earn ~ height, data = wages)
coef(hmod)
qplot(height, earn, data = wages, alpha = I(1/4)) +
  geom_smooth(se = FALSE, method = lm) + theme_bw()
