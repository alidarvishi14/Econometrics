#save and load RData and RDS format
x = 1:10
y = 10 * x + 4
saveRDS(x, file = "Desktop/untitled folder/x.RDS")
newx = readRDS(file = "Desktop/untitled folder/x.RDS")

save(x, y, file = "Desktop/untitled folder/xy.RData")
rm(x)
rm(list = ls(all.names = TRUE))
load(file = "Desktop/untitled folder/xy.RData")

#function
add = function(x, y) {
  print(x)
  print(y)
  return(x + y)
}

power = function(x, y) {
  return(x ^ y)
}

#loop
#for
for (i in 1:10) {
  print(i)
}

for (ch in c("asd", "test", 1, "word", 10)) {
  print(ch)
}

for (i in 1:10) {
  print(paste("this is sentence number:", i))
}

x = rep(1:9, 3)
dim(x) = c(3, 3, 3)
for (i in 1:3) {
  for (j in 1:3) {
    for (k in 1:3) {
      print(x[i, j, k])
    }
  }
}

for (i in 1:4) {
  assign(paste0("test", i), i)
}

for (i in 1:4) {
  saveRDS(
    object = get(paste0("test", i)),
    file = paste0("Desktop/untitled folder/", i, ".RDS")
  )
}

for (year in 2010:2013) {
  data = data.frame(x = 1:10, y = 10 * (1:10) + rnorm(10))
  write.csv(data , file = paste0("Desktop/untitled folder/", year, ".csv"))
}

for (year in 2010:2013) {
  csvfile = read.csv(file = paste0("Desktop/untitled folder/", year, ".csv"))
  assign (paste0 ("year", year, "data"), csvfile)
}

filenames = lapply(2010:2013, function(i) {
  return(paste0(i, ".csv"))
})

all_data <-  lapply(filenames, function(filename) {
  filepath = paste0("Desktop/untitled folder/", filename)
  read.csv(filepath, header = TRUE)
})

i = 1
while (i < 10) {
  i = i + 1
  print(i)
}

library(ggplot2)
library(dplyr)

#mutate() adds new variables that are functions of existing variables
#select() picks variables based on their names.
#filter() picks cases based on their values.
#summarise() reduces multiple values down to a single summary.
#arrange() changes the ordering of the rows.
#group_by() aggregates values by group categories.

filter(mtcars, mpg > 25 & cyl > 3)

select(mtcars, mpg, hp, cyl)
select(mtcars, hp:qsec)

arrange(mtcars, desc(mpg))

mutate(mtcars, newcol = hp + mpg * 10)

sample_n(mtcars, 3)

sample_frac(mtcars, 0.2)

grouped_by_cyl = group_by(mtcars, cyl)
group_by(mtcars, cyl, vs)

summarise(
  grouped_by_cyl,
  hp_mean = mean(hp),
  number = n(),
  qsec_up = mean(qsec) + 1.96 * sd(qsec),
  qsec_low = mean(qsec) - 1.96 * sd(qsec)
)

#x %>% f is equivalent to f(x)
#x %>% f(y) is equivalent to f(x, y)
#x %>% f %>% g %>% h is equivalent to h(g(f(x)))
round(sum(rnorm(20)), 1)
20 %>% rnorm() %>% sum() %>% round(1) 
#x %>% f(y, z = .) is equivalent to f(y, z = x)
#x %>% f(y, .) is equivalent to f(y, x)

mtcars %>% group_by(. ,  am, vs, cyl) %>%
  summarise(., hp_mean = mean(hp), disp_mean =mean(disp)) %>%
  mutate(., prod = hp_mean * disp_mean) %>% arrange(., desc(hp_mean)) -> new_data

new_data
new_data %>% ungroup()

mtcars %>% select(cyl,vs,am) %>% distinct()

new_data %>% ggplot(data = .) + geom_point(aes(x = hp_mean, y = disp_mean))

mtcars %>% group_by(gear, cyl) %>%
  summarise(., mean_mpg = mean(mpg), mean_disp = mean(disp)) %>%
  lm(data = ., formula = mean_mpg ~ mean_disp) %>% summary()

library(ISLR)
Smarket
glm.fit <- Smarket %>% glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=. ,family=binomial)
summary(glm.fit)


t.test(rnorm(n = 10, mean = 0, sd = 3), rnorm(10, mean = 1, sd = 1))
t.test(rnorm(n = 100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1))

