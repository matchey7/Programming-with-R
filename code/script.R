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
  # Input is character string of a csv file
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

#=================================
#  Analyzing Multiple Data Sets
#=================================

analyze <- function(filename) {
  # return graphs of average, min and max inflammation over time.
  # Input is character string of a csv file
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
}

analyze("data/inflammation-01.csv")

analyze("data/inflammation-02.csv")

best_practice_2 <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)
print_words(best_practice_2)

best_practice_2[-6]

print_words(best_practice_2[-6])
?NA

print_words <- function(sentence) {
  for (word in sentence) {
    print(word)
  }
}

print_words(best_practice_2)
print_words(best_practice_2[-6])

len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  len <- len + 1
}
# number of vowels
len

letter <- "z"
for (letter in c("a", "b", "c")) {
  print(letter)
}
letter
length(vowels)

seq(3)

print_N <- function(nat_number) {
  sequence <- seq(nat_number)
  for (number in sequence) {
    print(number)
  }
}

print_N(3)

total <- function(vector) {
  # calculates the sum of the values in a vector
  sum <- 0
  for (n in vector) {
    sum <- sum + n
  }
  print(sum)
}
  
ex_vec <- c(4, 8, 15, 16, 23, 42)
total(ex_vec)

2^4

expo <- function(base, power) {
  result <- 1
  for (i in seq(power)) {
    result <- result * base
  }
  return(result)
}

expo(2,4)
  
?append
x <-c(2,2)
x <- append(2, x)
x

list.files(path = "data", pattern = "csv")

?list.files

list.files(path = "data", pattern = "inflammation")

list.files(path = "data", pattern = "csv", full.names = TRUE)

list.files(path = "data", pattern = "inflammation", full.names = TRUE)

filenames <- list.files(path = "data",
                        # Now follows a regular expression that matches:
                        pattern = "inflammation-[0-9]{2}.csv",
                        #          /            /        the standard file extension of 
                        # comma-separated values
                        #          /            the variable parts (two digits, 
                        # each between 0 and 9)
                        #          the static part of the filenames
                        full.names = TRUE)
filenames <- filenames[1:3]
for (f in filenames) {
  print(f)
  analyze(f)
}

analyze_all <- function(folder = "data", pattern) {
  # Runs the function analyze for each file in the given folder
  # that contains the given pattern.
  filenames <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    print(f)
    analyze(f)
  }
}

analyze_all(,"inflammation-[0-9]{2}.csv")

?read.csv

pdf("inflammation-01.pdf")
analyze("data/inflammation-01.csv")
dev.off()

dev.cur()

num <- 37
if (num > 100) {
  print("greater")
} else {
  print("not greater")
}
print("done")

num <- 53
if (num > 100) {
  print("num is greater than 100")
}

sign <- function(num) {
  if (num > 0) {
    return(1)
  } else if (num == 0) {
    return(0)
  } else {
    return(-1)
  }
}

sign(-3)

sign(0)

sign(2/3)

if(1 > 0 && -1 > 0) {
  print("both parts are true")
} else {
  print("at least one part is not true")
}

if(1 > 0 || -1 > 0) {
  print("at least one part is true")
} else {
  print("neither part is true")
}

a <- NA
a == 1
a == NA
is.na(a)

if(is.na(a)) {
  print("Hi!")
}

plot_dist <- function(dataframe, threshold = 10) {
  if (length(dataframe) >= 10) {
    print(boxplot(dataframe))
  } else {
    print(stripchart(dataframe))
  }
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[,10], threshold = 10)   # day (column) 10

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[1:5,10], threshold = 10)   # day (column) 10

plot_dist <- function(dataframe, threshold = 10, use_boxplot = TRUE) {
  if (length(dataframe) >= 10 && use_boxplot) {
    print(boxplot(dataframe))
  } else if (length(dataframe) >= 10 && !use_boxplot) {
    print(hist(dataframe))
  } else {
    print(stripchart(dataframe))
  }
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10, use_boxplot = TRUE)   # day (column) 10 - create boxplot

plot_dist(dat[, 10], threshold = 10, use_boxplot = FALSE)  # day (column) 10 - create histogram

plot_dist(dat[1:5, 10], threshold = 10)                    # samples (rows) 1-5 on day (column) 10

filenames <- list.files(path = "data", pattern = "inflammation-[0-9]{2}.csv", full.names = TRUE)
filename_max <- "" # filename where the maximum average inflammation patient is found
patient_max <- 0 # index (row number) for this patient in this file
average_inf_max <- 0 # value of the average inflammation score for this patient
for (f in filenames) {
  dat <- read.csv(file = f, header = FALSE)
  dat.means <- apply(dat, 1, mean)
  for (patient_index in 1:length(dat.means)){
    patient_average_inf <- dat.means[patient_index]
    if (patient_average_inf > average_inf_max) {
      average_inf_max <- patient_average_inf
      patient_max <- patient_index
      filename_max <- f
    }
  }
}
print(filename_max)
print(patient_max)
print(average_inf_max)

analyze <- function(filename, output = NULL) {
  # Plot the average, min and max inflammation over time.
  # Input:
  #   filename: character string of a csv file
  #   output: character string of a csv file for saving
  if(!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  if (!is.null(output)) {
    dev.off()
  }
}

analyze("data/inflammation-01.csv")

analyze("data/inflammation-01.csv", output = "inflammation-01.pdf")

dir.create("results")

analyze("data/inflammation-01.csv", output = "results/inflammation-01.pdf")

analyze_all <- function(pattern) {
    # Directory name containing the data
   data_dir <- "data"
   # Directory name for results
   results_dir <- "results"
   # Runs the function analyze for each file in the given working directory
   # that contains the given pattern.
   filenames <- list.files(path = data_dir, pattern = pattern)
   for (f in filenames) {
     pdf_name <- file.path(results_dir, sub("csv", "pdf", f))
     analyze(file.path(data_dir, f), output=pdf_name)
   }
}

analyze_all("inflammation.*csv")
?plot

analyze <- function(filename, output = NULL) {
  # Plot the average, min and max inflammation over time.
  # Input:
  #   filename: character string of a csv file
  #   output: character string of a csv file for saving
  if(!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, type = "l")
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation, type = "l")
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation, type = "l")
  if (!is.null(output)) {
    dev.off()
  }
}

analyze_all("inflammation.*csv")

#===============================
# Dynamic Reports with knitr
#===============================

install.packages("knitr")

#===============================
# Making packages in R
#===============================

install.packages(c("devtools", "roxygen2"))  # installations can be `c`ombined
library("devtools")
library("roxygen2")

?setwd
getwd()
setwd("C:/Users/Lenovo User/Desktop/r-novice-inflammation")
create_package("tempConvert")
1
setwd("./tempConvert")
document()
setwd("..")
install("tempConvert")
?fahrenheit_to_kelvin
search()
fahrenheit_to_celsius(32)
celsius_to_kelvin(-273.15)
fahrenheit_to_kelvin(-459.67)
kelvin_to_celsius(273.15)

create_package("analyze")
3

setwd("./analyze")
document()
setwd("..")
install("analyze")
?analyze

analyze_all("inflammation.*csv")

search()

#===============================
# Introduction to RStudio
#===============================

# In RStudio, typing Alt + "-" will write <- in a single key stroke
# <-
  
#===============================
# Addressing Data
#===============================

dat <- read.csv(file = 'data.sample.csv', header = TRUE, stringsAsFactors = FALSE)