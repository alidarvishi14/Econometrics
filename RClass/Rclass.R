#getting help
help (lm)
? lm
example(lm)

#data types
c(1,2,3)
Inf
c(1:10)
1:10
as.character(1:10)
as.numeric(1:10)
as.complex(1:10)
as.logical(0:10)
c("a","b","c")
matrix(11:16, nrow = 2, ncol = 3,byrow = TRUE)

m = 1:10
dim(m) = c(2, 5)

a=factor(c("yes","no","yes"))

#dataframe
rnorm(n=10,mean=10)
my_data = data.frame( x=1:20 , y=1:20+rnorm(20,mean=0,sd = 1) )
head(my_data)
tail(my_data)
dim(my_data)
nrow(my_data)
ncol(my_data)
str(my_data)
names(my_data)

#preloaded data
help(mtcars)
mtcars
View(mtcars)
mtcars=mtcars
mtcars[1,1] # first row, first column
mtcars[2,] # the second row 
mtcars[,3] # the third column
mtcars[,"mpg"] # Columns can also be called by their name
mtcars$mpg # Use the dollar sign $ to call a column
mtcars[c(5,7,9), c(1,2)] # Get the 5th, 7th, and 9th rows and the first two columns.
mtcars$cyl > 4
mtcars[mtcars$gear > 3, ]
mtcars[mtcars$gear == 4,]
mtcars[mtcars$mpg > 4 & mtcars$hp ==100,]
mtcars[mtcars$cyl == 3,]
mtcars[!mtcars$cyl == 3,]

#some functions
log (10)       # Natural logarithm with base e=2.7182
log10(5)      # Common logarithm with base 10
5^2             # 5 raised to the second power
5/8             # Division
sqrt (16)      # Square root
abs (3-7)     # Absolute value 
pi                # 3.14
exp(2)         # Exponential function 
round(pi,0)  # Round pi to a whole number
round(pi,1)  # Round pi to 1 decimal place
floor(15.9)   # Rounds down
ceiling(15.1)  # Rounds up
sin(.5)        # Sine Function
tan(.5)       # Tangent Function
acos(0.8775826)        # Inverse Cosine
atan(0.5463025)      # Inverse Tangent

#readfile libraries
# haven - SPSS, Stata, and SAS files
# readxl - excel files (.xls and .xlsx)
# DBI - databases
# jsonlite - json
# xml2 - XML
# httr - Web APIs
# rvest - HTML (Web Scraping)

install.packages("readr")
library(readr)
library(readxl)

read_csv(file = "Desktop/home-data-for-ml-course/test.csv") -> test_data
test_data <- read_csv(file = "Desktop/home-data-for-ml-course/test.csv")
head(test_data)
which.max(test_data$YearBuilt)
test_data[which.max(test_data$YearBuilt),]

#plot
library(ggplot2)
help(aes)
ggplot(data = test_data, aes(x =LotArea , y =GrLivArea ))
p = ggplot(data = test_data, aes(x =LotArea , y =GrLivArea ))
p+geom_point()
p+geom_point(size = 4, alpha = 0.6, color = "blue")
ggplot(test_data)+geom_point(aes(x=LotArea,y=GrLivArea,color=Foundation,size=LotFrontage))

p=ggplot(data=mtcars,aes(x=disp,y=mpg,color=gear))
p+geom_point()+geom_abline(slope=-1/20,intercept = 32)
p+geom_point()+geom_smooth(method = "lm")
p+geom_point()+geom_smooth(aes(group=am),method="lm")

#hist
hist(test_data$LotArea)
p=ggplot(test_data,aes(x=LotArea))
p+geom_histogram()
p+geom_histogram(binwidth = 100)
p+geom_histogram(binwidth = 1000,linetype = "dashed",colour="black", fill="red",alpha  = 0.3)
p+geom_histogram(binwidth = 1000,aes(fill=BsmtQual))
ggplot(test_data,aes(x=LotArea,y=..density..))+geom_histogram(binwidth = 1000)+geom_density(color="gold",size=1)

#bar
ggplot(test_data,aes(TotRmsAbvGrd))+geom_bar()
ggplot(test_data,aes(TotRmsAbvGrd))+geom_bar(aes(fill=HeatingQC))

#linear_model
car_data=mtcars
linear_model=lm(data=car_data,formula = mpg~disp)
summary(linear_model)
linear_model$coefficients
linear_model$residuals
plot(linear_model)

#prediction and graphing
prediction_X=data.frame(disp=c(100:110),lkdsf=c(110:120))
predict(linear_model,prediction_X)
predict(linear_model,prediction_X,interval = "confidence")
predict(linear_model,prediction_X,interval = "prediction",level=0.95)
prediction_XY=cbind(prediction_X,predict(linear_model,prediction_X,interval = "confidence"))
mtcars_with_prediction=cbind(mtcars,predict(linear_model,interval = "confidence"))
ggplot(mtcars_with_prediction,aes(x=disp))+geom_point(aes(y=mpg))+geom_line(aes(y=upr))+geom_line(aes(y=lwr))
ggplot(mtcars_with_prediction,aes(x=disp))+geom_point(aes(y=mpg))+
  geom_line(aes(y=upr),linetype="dashed")+geom_line(aes(y=lwr),linetype="dashed")+
  geom_abline(intercept = linear_model$coefficients[1],slope = linear_model$coefficients[2])
linear_model$coefficients[2]

#multivariable regression
pairs(mtcars)
pairs(mtcars[c(1,2,3,4)])
cor(mtcars[c(1,2,3,4)])
linear_model=lm(data=mtcars,formula = mpg~disp+hp)
summary(linear_model)
linear_model=lm(data=mtcars,formula = mpg~disp+log(hp))
summary(linear_model)
linear_model=lm(data=mtcars,formula = mpg~disp+hp+disp:hp)
summary(linear_model)
linear_model=lm(data=mtcars,formula = mpg~disp*hp)
summary(linear_model)
linear_model=lm(data=mtcars,formula = mpg~disp*hp-1)
summary(linear_model)
linear_model=lm(data=mtcars,formula = mpg~.-gear-1)
summary(linear_model)
plot(linear_model)

library(xlsx)
write.xlsx(mydata, "Desktop/mydata.xlsx")

for (i in 1:10){
  i
}
