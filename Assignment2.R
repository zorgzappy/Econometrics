Opened <- read.csv('rutgers_distances.csv')
mean(Opened$d)
var(Opened$d)
hist(Opened$d)
mean(Opened$d[1:10])
mean(Opened$d[1:25])
mean(Opened$d[1:300])

sampleMeans <- c()


sampleMeans <- replicate(500, mean(sample(Opened$d, size = 2, replace = TRUE)))
mean(sampleMeans)
hist(sampleMeans)

sampleMeans2 <- replicate(500, mean(sample(Opened$d, size = 250, replace = TRUE)))
mean(sampleMeans2)
hist(sampleMeans2)


Housing <- read.csv('acs_jersey_city.csv')
mean(Housing$renter)
library(tidyverse)



renter <- Housing %>%
  filter(income > 0) %>%
  filter(renter == 1)
mean(renter$income)

nonrenter <- Housing %>%
  filter(income > 0) %>%
  filter(renter == 0)
mean(nonrenter$income)

renter <- renter %>%
  mutate(rent_to_income_ratio = (rent * 12)/income * 100)
hist(renter$rent_to_income_ratio)
mean(renter$rent_to_income_ratio)
var(renter$rent_to_income_ratio)
zstat <- (mean(renter$rent_to_income_ratio) - 28.5)/(sd(renter$rent_to_income_ratio) / sqrt(length(renter$rent_to_income_ratio)))
pstat <- 2*pnorm(zstat)
rm(pstat)

zstat2 <- (mean(renter$rent_to_income_ratio) - 30)/(sd(renter$rent_to_income_ratio) / sqrt(length(renter$rent_to_income_ratio)))
pstat2 <- pnorm(zstat2)


SampleSub <- Housing %>%
  filter (pid < 200) %>%
  filter (income > 0) %>%
  filter(renter == 1)

SampleSub <- SampleSub %>%
  mutate(rent_to_income_ratio = (rent * 12)/income)

mean(SampleSub$rent_to_income_ratio, na.rm = T)
zstat3 <- (mean(SampleSub$rent_to_income_ratio) - .3)/(sd(SampleSub$rent_to_income_ratio) / sqrt(length(SampleSub$rent_to_income_ratio)))
pstat3 <- pnorm(zstat3)


Diploma <- Housing %>%
  filter (high_school == 1) %>%
  filter (income > 0) %>%
  filter(renter == 1)

noDiploma <- Housing %>%
  filter (high_school == 0) %>%
  filter (income > 0) %>%
  filter(renter == 1)

Diploma <- Diploma %>%
  mutate(rent_to_income_ratio = (rent * 12)/income*100)
noDiploma <- noDiploma %>%
  mutate(rent_to_income_ratio = (rent * 12)/income*100)

difference <- mean(Diploma$rent_to_income_ratio) - mean(noDiploma$rent_to_income_ratio)

zstat4 <- (difference - 0)/(sqrt(var(Diploma$rent_to_income_ratio)/length(Diploma$rent_to_income_ratio) + var(noDiploma$rent_to_income_ratio)/length(noDiploma$rent_to_income_ratio)))
pstat4 <- 2*pnorm(zstat4)

plot(x=renter$wage_hourly, y=renter$rent_to_income_ratio, xlab="Hourly Wage", ylab = "Rent-to-income-ratio")
renter <- renter %>%
  filter(wage_hourly > 0)
cor(renter$wage_hourly, renter$rent_to_income_ratio)                
