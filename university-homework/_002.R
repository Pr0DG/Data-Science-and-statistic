# Exercise 1.2
# The file samochody.csv contains data about 
# car parameters from few pointed car brands. 

# a) Variable "mpg" contains data corresponding to 
# the numbers of miles traveled by car per fuel gallon
# Create variable "zp", which will hold a value of fuel
# usage (in liters) per 100 km

#Importing data from file.
cars <- read.csv2("C:\\Users\\user\\Desktop\\SWD\\swd_lab\\smwd\\samochody.csv")
# zp variable appending
cars$zp <- (100*3.785)/(cars$mpg*1.609)
cars <- cars[-117,]
# b) Create stemplot for cars$zp variable
stem(cars$zp, na.rm = TRUE)

# c) Create histogram for fluel usage zp variable
hist(cars$zp, na.rm = TRUE)

# d) Create boxplot for zp data
boxplot(cars$zp, horizontal = TRUE, na.rm = TRUE)

# e) Find basic sample statistics for data which describes
# fuel usage (mean, median, variance, standard deviation, gap, quantiles,
# quantiles gap, extremal values, asymmetry factor, coefficient of variantion)
median(cars$zp, na.rm =TRUE) # Ans.: 8.139865
mean(cars$zp, na.rm = TRUE) # Ans.: 8.766693
var(cars$zp, na.rm = TRUE) # Ans.: 5.895066
sd(cars$zp, na.rm = TRUE) # Ans.: 2.427976
cv <- sd(cars$zp, na.rm = TRUE) / var(cars$zp, na.rm = TRUE) # Ans.: cv = 0.4118657
range(cars$zp, na.rm = TRUE) # Ans.: Min = 5.048053, Max = 15.176728

# installing e1071 package which contains kurtosis function
install.packages('e1071', dependencies =TRUE)
# load e1071 library
library(e1071) 
kurtosis(cars$zp, na.rm = TRUE) # Ans.: -5847272
remove.packages('e1071', "C:/Users/user/Documents/R/win-library/3.5")

# asymmetry factor
# A = mi3 / sig^3
# M3 = 1/n* sum(xi - m)^3 - from i = 1, to n
which(is.na(cars2$zp))
cars2 <- read.csv2("C:\\Users\\user\\Desktop\\SWD\\swd_lab\\smwd\\samochody.csv")
cars2$zp <- (100*3.785)/(cars2$mpg*1.609)
cars2 <- cars2[-117,]
summary(cars2$zp)
n <- nrow(cars2)
m <- mean(cars2$zp)
M3 <- 1/n * sum((cars2$zp - m)^3)
A <- M3 / sd(cars2$zp)^3 # Ans.: 06801540971178899
# d) Count 5, 10, 90, 95 prercentile and interpret result 
quantile(cars$zp, c(0.05, 0.1, 0.9, 0.95), na.rm = TRUE)
# 5% - 5.760735 
# 10% - 6.1905507
# 90% - 12.296949
# 95% - 13.355868 

#Exercise 1.3 

#Using data from previous exercise, create variable zp_kat, taking only
# three enum values, which describe fuel usage level, with the schema:
# TO TO: END excercise desc
# Add new collumn to cars - cateory definied by cars$zp
cars$zp_kat <- ifelse(cars$zp <= 7, "malo",  
                      ifelse(cars$zp > 7 & cars$zp <= 10, "srednio", "duzo"))
# cars$zp_kat Pie chart
smallUsage <- length(which(cars$zp_kat == "malo"))
mediumUsage <- length(which(cars$zp_kat == "srednio"))
largeUsage <- length(which(cars$zp_kat == 'duzo'))

fuelUsageVec <- c(smallUsage, mediumUsage, largeUsage)
labels <- c(paste("Malo - ",sep = "", round(smallUsage / length(cars$zp)* 100, 2), "%"),
            paste("Srednio - ",sep = "", round(mediumUsage / length(cars$zp)* 100, 2), "%"),
            paste("Duzo - ",sep = "", round(largeUsage / length(cars$zp)* 100, 2), "%"))

pie(fuelUsageVec, labels, radius = 1, col = c('orangered3', 'white', 'palevioletred3'))

# Bar Plot
counts <- table(cars$zp)
barplot(counts, main="Fuel usage", xlab ="amount") #Somethig's wrong!!!

#Exercise 1.4
europeCars  <- cars[cars$producent == 2,]
usCars <- cars[cars$producent == 1,]
japanCars <-cars[cars$producent == 3,]

europeCarsZpMean <- mean(europeCars$zp)
usCarsZpMean <- mean(usCars$zp)
japanCarsZpMean <- mean(japanCars$zp)

europeCarsZpSd <- sd(europeCars$zp)
usCarsZpSd <- sd(usCars$zp)
japanCarsZpSd <- sd(japanCars$zp)
par(mfrow=c(3,1))
boxplot(europeCars$zp,
        col = 'purple',
        main = 'Europe Cars',
        horizontal = TRUE,
        ylim = c(5, 16))
boxplot(usCars$zp,
        col = 'purple',
        main = 'USA Cars',
        horizontal = TRUE,
        ylim = c(5, 16))
boxplot(japanCars$zp,
        col = 'purple',
        main = 'Japan Cars',
        horizontal = TRUE,
        ylim = c(5, 16))

#Exercise 1.5
# Compare acceleration of US's, Europian, Jananese cars.
carsFromUSOrJpn <- subset(cars, producent==1 | producent==3)
boxplot(carsFromUSOrJpn$przysp~carsFromUSOrJpn$producent,
        col = 'purple',
        horizontal = TRUE,
        ylim = c(5, 40))

#Exercise 1.6
#tapply(cars$zp, cars$waga < 2500, mean) - cool feature
lightCars <- subset(cars, cars$waga < 2500)
mean(lightCars$zp)
median(lightCars$zp)
var(lightCars$zp)
sd(lightCars$zp)
# Don't know what wspolczynnik asymetrii is!!!

#Exercise 1.7
carsFrom79to81 <- subset(cars, rok >= 79 & rok <= 81)
#a)
boxplot(carsFrom79to81$moc,
        col = 'purple',
        horizontal = TRUE)
points(carsFrom79to81$moc, rnorm(length(carsFrom79to81$moc),1,0.05))
#b)
hist(carsFrom79to81$moc, col='purple')
#c)
which(is.na(carsFrom79to81$moc)) # 51 57 75
carsFrom79to81 <- carsFrom79to81[-c(51, 57, 75),]
quantile(carsFrom79to81$moc, 0.95)

#Exercise 1.8
par(mfrow = c(2,1))
carsFrom25To30hp <- subset(cars, waga >= 2500 & waga <= 3000)
which(is.na(carsFrom25To30hp$przysp))
#a)
boxplot(carsFrom25To30hp$przysp,
        horizontal =TRUE,
        col = 'purple')
# b)
hist(carsFrom25To30hp$przysp, 
     col = 'purple',
     main = '',
     xlab = 'Acceleration')
# c)
quantile(carsFrom25To30hp$przysp, 0.75)

#Exercise 1.9
carsWithHighFUsage <- subset(cars, mpg > 26)
which(is.na(carsWithHighFUsage$mpg))

# a) 
boxplot(carsWithHighFUsage$waga, 
        horizontal =TRUE,
        col = 'purple')
# b)
hist(carsWithHighFUsage$waga, 
     col = 'purple',
     main = '',
     xlab = 'Weight')
# c)
quantile(carsWithHighFUsage$waga, 0.95)
abline(v=quantile(carsWithHighFUsage$waga, 0.95), col = 'red')