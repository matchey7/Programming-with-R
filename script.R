#=================================
#  Analyzing Patient Data
#=================================

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
head(dat)
class(dat)
dim(dat)
# first row, all of the columns
patient_1 <- dat[1, ]
# max inflammation for patient 1
max(patient_1)
# max inflammation for patient 2
max(dat[2, ])
# minimum inflammation on day 7
min(dat[ , 7])
# mean inflammation on day 7
mean(dat[ , 7])
# median inflammation on day 7
median(dat[ , 7])
# standard deviation of inflammation on day 7
sd(dat[ , 7])
# some calculations do not work on rows of data frames. For example...
mean(dat[1, ])
# ...will not work, it has to be treated as follows
mean(as.numeric(dat[1, ]))
# Summarize function
summary(dat[, 1:40])
help(apply)
avg_patient_inflammation <- apply(dat,1,mean)
avg_day_inflammation <- apply(dat, 2, mean)
avg_patient_inflammation
avg_day_inflammation
rowMeans(dat)
colMeans(dat)
animal <- c("m", "o", "n", "k", "e", "y")
animal[1:3]
animal[4:6]
max(dat[5,3:7])
?seq
dat[seq(from = 2, to = 60, by = 2), ]

whichPatients <- seq(1,5) # i.e. which rows
whichDays <- seq(1,5) # i.e. which columns
dat2 <- dat
# check the size of your subset: returns `30 5`, that is 30 [rows=patients] by 5 [columns=days]
# dim(dat2[whichPatients,whichDays])
apply(dat2[whichPatients,],1,mean)

whichPatients <- seq(1,5) # i.e. which rows
whichDays <- seq(1,10) # i.e. which columns
dat2 <- dat
# check the size of your subset: returns `30 5`, that is 30 [rows=patients] by 5 [columns=days]
# dim(dat2[whichPatients,whichDays])
apply(dat2[,whichDays],2,mean)

whichPatients <- seq(1,5) # i.e. which rows
whichDays <- seq(2,40,2) # i.e. which columns
dat2 <- dat
# check the size of your subset: returns `30 5`, that is 30 [rows=patients] by 5 [columns=days]
# dim(dat2[whichPatients,whichDays])
apply(dat2[,whichDays],2,mean)

# 1.
apply(dat[1:5, ], 1, mean)
# 2.
apply(dat[, 1:10], 2, mean)
# 3.
apply(dat[, seq(2, 40, by = 2)], 2, mean)

plot(avg_day_inflammation)
max_day_inflammation <- apply(dat,2,max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat,2,min)
plot(min_day_inflammation)

sd_day_inflammation <- apply(dat,2,sd)
plot(sd_day_inflammation)

#=================================
#  Creating Functions
#=================================

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

#Freezing point of water
fahrenheit_to_celsius(32)

# Boiling point of water
fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

# freezing point of water in Kelvin
celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}

# freezing point of water in Kelvin
fahrenheit_to_kelvin(32.0)

x <- c("A","B","C")

highlight_content <- function(content, wrapper) {
  highlight <- c(wrapper, content, wrapper)
  return(highlight)
}

highlight_content(best_practice, asterisk)

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***" # R interprets a variable with a single value as a vector
                  # with one element.
highlight(best_practice, asterisk)

edges <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
edges(dry_principle)

center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint 
  return(new_data)
}

z <- c(0,0,0,0)
z

center(z,3)

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
centered <- center(dat[,4],0)
(centered)

# original mean
mean(dat[, 4])

# centered mean
mean(centered)

# original standard deviation
sd(dat[,4])
sd(centered)
sd(dat[,4]) - sd(centered)
all.equal(sd(dat[,4]), sd(centered))

# new data object and set one value in column 4 to NA
datNA <- dat
datNA[10,4] <- NA 

# returns all NA values
center(datNA[,4],0)

center <- function(data, midpoint) {
  # return a new vector containing the original data centered around the 
  # midpoint.
  # Example: center(c(1, 2, 3), 0) => c(-1, 0, 1)
  new_data <- (data - mean(data, na.rm=TRUE)) + midpoint 
  return(new_data)
}

center(datNA[,4], 0)

datNA[,1] <- as.factor(datNA[,1])
datNA[,2] <- as.character(datNA[,2])

center(datNA[,2], 0)

?read.csv

analyze <- function(filename) {
  # return graphs of average, min and max inflammation over time.
  # Input is character string of a csv file (so it has to be in "quotation 
  # marks")
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
    
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
}

analyze("data/inflammation-03.csv")

dat <- read.csv(file = "data/inflammation-02.csv", header = FALSE)
dat

rescale <- function(vector) {
  # re scales the input vector so that the values lie between 0 and 1.
  # The replacement for a value v is (v-L)/(H-L), where H and L are the
  # highest and lowest values of the vector, respectively.
  H <- max(vector)
  L <- min(vector)
  vector <- (vector-L) / (H - L)
  return(vector)
}
a <- c(2,3,4,5,6,7,8,9,10)
rescale(a)
plot(rescale(a))

dat <- read.csv("data/inflammation-01.csv", FALSE)
dat
dat <- read.csv(header = FALSE, file = "data/inflammation-01.csv")
dat
dat <- read.csv(FALSE, "data/inflammation-01.csv")

center <- function(data, midpoint = 0) {
  # return a new vector containing the original data centred around the 
  # midpoint (0 by default).
  # Example: center(c(1,2,3), 0) => c(-1,0,1)
  new_data <- (data - mean(data)) + midpoint 
  return(new_data)
}

test_data <- c(0,0,0,0)
center(test_data,3)
more_data <- 5 + test_data
more_data
center(more_data)

display <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("a", "b", "c") # This names each element of the vector
  return(result)
}

# no arguments
display()

# one argument
display(55)

# two arguments
display(55, 66)

# three arguments
display(55, 66, 77)

# only setting the value of c
display(c = 77)

?read.csv

rescale <- function(vector, lower = 0, upper = 1) {
  # re scales the input vector so that the values lie between 0 and 1.
  # The replacement for a value v is (v-L)/(H-L), where H and L are the
  # highest and lowest values of the vector, respectively.
  H <- max(vector)
  L <- min(vector)
  vector <- (vector-L) / (H - L) * (upper - lower) + lower
  return(vector)
}

test <- c(2, 3, 6, 4, 1, 0, 8, 9)
rescale(test)
rescale(test, 2, 10)
