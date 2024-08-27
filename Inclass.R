x <- 5
hen = 3
y = 1
Center = 4
Cen = 2 + y*12 + Center * 6

rm (Cen,hen, Center,x,y)

# log function uses ln, not base 10
# vector

v <- c(x,y,4)
v[2] #gives item at index 2, starts from 1, not 0, when counting indecies

heigh <- c(168, 177, 177, 165, 178, 172, 177, 171)
heigh[1:3] #return numbers 1 - 3
heigh[c(1,4)]

id <- 1:8 
#num and int types can be converted into each other, num are for larger numbers
# and int is for smaller numbers 

weight <- c(88, 72, 85, 52, 71, 69, 61, 61)
length(weight)

bmi<- weight/(heigh/100)^2
#to see all of the values, in case its cut off, all you have to do is type the variable name in 
#the console


m <-cbind(id, heigh, weight) #Has to be same size
dim(m) #gives dimensions of matrix

#Scatterplot
png('simple_plot.png') # need to create the png file before adding shit
plot(x=weight, y=heigh, xlab="Weight, in kg", ylab = "Height, in cm")
abline(v=70, h= 170, col = "blue")
dev.off()
#type getwd() to find where R is saving your plots and shit
#Session -> Seting WORKING DIRECTORY
# you can also use the command setwd("<FileExpansionName>")


#Data.frames
df <- data.frame(m)
#df[2,3] prints out the exact point
df[2,3]
#df$heigh prints out all of the heights
df$heigh/df$weight
df$bmi2 <- df$weight/(df$heigh/100)^2

df
head(df,3) # prints first 3 rows 

df_short <- df[df$heigh <=172,]
df_short2 <- df[df$heigh<=172, c("id", "weight")]



schools <- read.csv('CASchools.csv')
mean(df$heigh)
sd(df$heigh)
cov(df$heigh, df$weight)
cor(df$heigh, df$weight)
t_statistic <- (mean(df$heigh) - 171)/(sd(df$heigh/sqrt(length(df$heigh))))
p_value <- 2*pnorm(-abs(t_statistic))
