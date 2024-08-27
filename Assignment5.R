healths <-read.csv('health_insurance.csv')
ols1 <- lm(insured ~ health, data = healths)
vcov <- vcovHC(ols1, type ="HC3") 
robust_se <- sqrt(diag(vcov))


ols2 <- lm(insured ~ health + log(inc), data = healths)
vcov2 <- vcovHC(ols2, type ="HC3") 
robust_se2 <- sqrt(diag(vcov2))
cor(healths$health, log(healths$inc))
cor(healths$insured, log(healths$inc))


ols3 <- lm(insured ~ health + age + educ + empl + log(inc) , data = healths)
vcov3 <- vcovHC(ols3, type ="HC3") 
robust_se3 <- sqrt(diag(vcov3))

myprobit <- glm(insured ~ health + age + educ + empl + log(inc) , family =
                  binomial(link = "probit"), data = healths)
new_data <- data.frame(
  health = 5,
  age = 21,
  educ = 10,
  empl = 0,
  inc = 1000
)

predicted_probs <- predict(myprobit, newdata = new_data, type = "response")


cri <-read.csv('crime_and_schools.csv')
ols4 <- lm(mathpct ~ crime_exposure , data = cri)
vcov4 <- vcovHC(ols4, type ="HC3")/ 
robust_se4 <- sqrt(diag(vcov4))
summary(ols4)
library(plm)
install.packages("plm")
install.packages("rbibutils")
library(plm)
model_with_fe <- plm(mathpct ~ crime_exposure, data = cri, index = c("nhoodid", "year"), model = "within", effect = "twoways")
model_with_fe2 <- plm(mathpct ~ crime_exposure + crime_exposure*, data = cri, index = c("nhoodid", "year"), model = "within", effect = "twoways")

cri$high_income <- ifelse(cri$med_inc_hh > 40000, 1, 0)


model_with_fe3 <- plm(mathpct ~ crime_exposure + crime_exposure*high_income, data = cri, index = c("nhoodid", "year"), model = "within", effect = "twoways")

