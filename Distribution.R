library('dplyr')
library('corrr')
library("data.table")
library('tidyverse')

rmp = read.csv("/Users/emiliamorgan/desktop/Spring 2023/CompStat/rmp.csv", header = TRUE)

x <- c('Katherine  Jones', 'Leslie  Looney', 'Jans  Wager', 'William  Hollinrake')

hist(rmp$student_star, main = "Student Star Distributions", xlab = "Student Star Ratings")
hist(rmp$student_difficult, main = "Student Difficulty Distribution", xlab = "Student Difficulty Ratings")
ggplot(data = rmp, aes(x = student_star, y = student_difficult)) +
  geom_point() +
  labs(x = "student star rating",
       y = "student difficulty rating") 

rmp.1 <- rmp %>% filter(professor_name == x[1])
rmp.2 <- rmp %>% filter(professor_name == x[2])
rmp.3 <- rmp %>% filter(professor_name == x[3])
rmp.4 <- rmp %>% filter(professor_name == x[4])


rmp.1$post_date <- format(as.Date(rmp.1$post_date, "%m/%d/%Y"))
rmp.1 <- rmp.1[rev(order(as.Date(rmp.1$post_date, format="%m/%d/%Y"))),]
rmp.1 <- rmp.1 %>% mutate(order_date=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

ggplot(data = rmp.1, aes(x = order_date, y = student_star)) +
  geom_point() +
  labs(x = "date",
       y = "star rating")

lm(rmp.1$order_date ~rmp.1$student_star)

rmp.2 <- rmp %>% filter(professor_name == x[2])
rmp.2$post_date <- format(as.Date(rmp.2$post_date, "%m/%d/%Y"))
rmp.2 <- rmp.2[rev(order(as.Date(rmp.2$post_date, format="%m/%d/%Y"))),]
rmp.2 <- rmp.2 %>% mutate(order_date=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

ggplot(data = rmp.2, aes(x = order_date, y = student_star)) +
  geom_point() +
  labs(x = "date",
       y = "star rating")

lm(rmp.2$order_date ~rmp.2$student_star)

rmp.3 <- rmp %>% filter(professor_name == x[3])
rmp.3$post_date <- format(as.Date(rmp.3$post_date, "%m/%d/%Y"))
rmp.3 <- rmp.3[rev(order(as.Date(rmp.3$post_date, format="%m/%d/%Y"))),]
rmp.3 <- rmp.3 %>% mutate(order_date=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

ggplot(data = rmp.3, aes(x = order_date, y = student_star)) +
  geom_point() +
  labs(x = "date",
       y = "star rating")

lm(rmp.3$order_date ~rmp.3$student_star)

rmp.4 <- rmp %>% filter(professor_name == x[4])
rmp.4$post_date <- format(as.Date(rmp.4$post_date, "%m/%d/%Y"))
rmp.4 <- rmp.4[rev(order(as.Date(rmp.4$post_date, format="%m/%d/%Y"))),]
rmp.4 <- rmp.4 %>% mutate(order_date=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

ggplot(data = rmp.4, aes(x = order_date, y = student_star)) +
  geom_point() +
  labs(x = "date",
       y = "star rating") 

lm(rmp.4$order_date ~rmp.4$student_star)