Cigar = read.csv('cigarettes_1995.csv')
plot(x=Cigar$price, y=Cigar$packs, xlab="Average price per pack", ylab = "Pack per capita")
ggplot(data = Cigar, aes(x = price, y = packs)) + 
  geom_point(color = 'green', size = 2, alpha = 0.8) + 
  ylab('Number of packs per capita') + xlab('Average price per pack of cigarettes') + theme_classic()




olstemp<- lm(packs ~ price, data = Cigar)
summary(olstemp)
plot(ols1)

install.packages('sandwich')
library(sandwich)
vcov <- vcovHC(olstemp, type ="HC3") 
r <- sqrt(diag(vcov))
r
install.packages('modelsummary')
library(modelsummary)
modelsummary(list(ols1), gof_map = c("nobs", "r.squared"), vcov = "robust", output = "My_first_regression.docx")



ols1$coefficients #print coefficients


House <- read.csv('house_prices.csv')
ols_2 <- lm(price ~ lotsize, data = House)
summary(ols_2)

cor(House$bedrooms, House$lotsize)
cor(House$bedrooms, House$price)

vcov <- vcovHC(ols_2, type ="HC3") 
robust_se <- sqrt(diag(vcov))
robust_se


ols_3 <- lm(price ~ lotsize + bedrooms, data = House)
summary(ols_3)
vcov <- vcovHC(ols_3, type ="HC3") 
robust_ses <- sqrt(diag(vcov))
robust_ses




ols_4 <- lm(price ~ lotsize + bedrooms + bathrooms + stories + garage, data = House)
summary(ols_4)
vcov <- vcovHC(ols_4, type ="HC3") 
robust_ses <- sqrt(diag(vcov))
robust_ses
