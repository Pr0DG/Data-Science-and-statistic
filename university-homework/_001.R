#Excercise 1.1
# There is given a list, which shows a number of creations new account
# during ten days. 
myVector <- c(43, 37, 50, 51, 58, 105, 52, 45, 10)
# a) Count mean (avg), median, quantiles and standard deviation 
mean(myVector) # Ans.: 50.11111
median(myVector) # Ans.: 50
quantile(myVector, c(0.25, 0.75)) #Ans.: 43, 52
sd(myVector) # Ans.: 24.84172

# b) Check if there are outliers in vector, according to the 1,5IQR rule
IQR(myVector) # is 9 
outliers <- which(median(myVector) + IQR(myVector) < myVector | 
                  median(myVector) - IQR(myVector) > myVector)

# c) Delete identified outliers and count mean, media, quantiles,
# and standatd deviation again.
myVector2 <- myVector[-outliers]
mean(myVector2) # Ans.: 49.83333
median(myVector2) # Ans.: 50.5
quantile(myVector2, c(0.25, 0.75)) # Ans.: 46.25, 51.75
sd(myVector2) # Ans.: 4.344779

# d) What effect had that outliers for pointed statistics?

# Ans. Data collected in 1st vector has a little bit higher mean, lower median, 1st and 3rd quantile.
# Furthermore, standard deviation in sectond vector is a lot lower that in first one.
