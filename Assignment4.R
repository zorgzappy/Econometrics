Cigar = read.csv('cigarettes_1995.csv')
# setwd("/Users/anuragpathak/Desktop/R_stuff")

ols1 <- lm(packs ~ price + I(price^2), data = Cigar)
vcov <- vcovHC(ols1, type ="HC3") 
robust_se <- sqrt(diag(vcov))
tstat <- 0.011145/.004965683


ols2 <- lm(log(packs) ~ log(price), data = Cigar)
vcov2 <- vcovHC(ols2, type ="HC3") 
robust_se2 <- sqrt(diag(vcov2))



World <- read.csv('DDCG_dataset.csv')
democ <- World %>%
  filter(dem == 1) %>%
  filter(year == 2010)
democmean <- mean(democ$gdp_capita)
nodemoc <- World %>%
  filter(dem == 0) %>%
  filter(year == 2010)

y2010 <- World %>%
  filter (year == 2010)
ols2010 <- lm(gdp_capita ~ dem, data = y2010)
summary(ols2010)



nodemocmean <- mean(nodemoc$gdp_capita)
t <- (democmean-nodemocmean)/(sqrt(var(democ$gdp_capita)/78 + var(nodemoc$gdp_capita)/33))
ols3 <- lm(log(packs) ~ log(price), data = World)




ols4 <- lm(log(gdp_capita) ~ dem, data = World)
vcov4 <- vcovHC(ols4, type ="HC3") 
robust_se4 <- sqrt(diag(vcov4))
tst <- 1.3672/0.1571190 
ols5 <- lm(log(gdp_capita) ~ dem + lp_bl + lh_bl, data = World)
vcov5 <- vcovHC(ols5, type ="HC3") 
robust_se5 <- sqrt(diag(vcov5))
ols6 <- lm(log(gdp_capita) ~ dem + lp_bl + lh_bl + ginv, data = World)
vcov6 <- vcovHC(ols6, type ="HC3") 
robust_se6 <- sqrt(diag(vcov6))


cor(World$lp_bl, World$gdp_capita)
cor(World$lp_bl, World$dem)
cor(World$lh_bl, World$gdp_capita)
cor(World$lh_bl, World$dem)

cor(World$ginv, World$gdp_capita)
cor(World$ginv, World$dem)

# Extract R-squared and adjusted R-squared for part 4
r_squared_part4 <- summary(ols5)$r.squared
adj_r_squared_part4 <- summary(ols5)$adj.r.squared

# Extract R-squared and adjusted R-squared for part 5
r_squared_part5 <- summary(ols6)$r.squared
adj_r_squared_part5 <- summary(ols6)$adj.r.squared



ols7 <- lm(log(gdp_capita) ~ dem + lp_bl + lh_bl + ginv + tradewb, data = World)
vcov7 <- vcovHC(ols7, type ="HC3") 
robust_se7 <- sqrt(diag(vcov7))
library(car)


linearHypothesis(ols7, c("ginv = 0", "tradewb = 0"), vcov. = vcov7)
