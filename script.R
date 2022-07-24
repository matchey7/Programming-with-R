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

