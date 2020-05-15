unzip("rprog_data_ProgAssignment3-data.zip", exdir = 'data')

outcome <- read.csv("data\\outcome-of-care-measures.csv", colClasses = "character",na.strings='Not Available', stringsAsFactors=FALSE)

head(outcome)
str(outcome)

ncol(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

str(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)


## You may get a warning about NAs being introduced; that is okay

hist(outcome[, 11])

