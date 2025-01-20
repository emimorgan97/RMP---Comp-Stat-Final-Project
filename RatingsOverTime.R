x <- c()
date_plot <-function(x){
  rmp.1 <- rmp %>% filter(professor_name == rmpnew$professor_name[x])
  rmp.1$post_date <- format(as.Date(rmp.1$post_date, "%m/%d/%Y"))
  rmp.1 <- rmp.1[rev(order(as.Date(rmp.1$post_date, format="%m/%d/%Y"))),]
  sd <- sd(rmp.1$student_star)
  x <- nrow(rmp.1)
  y <- rep(1:x)
  rmp.1 <- rmp.1 %>% mutate(order_date=y)
  z <- summary(lm(rmp.1$order_date ~rmp.1$student_star))
  if (is.na(sd) != TRUE & sd != 0 & x > 7){
  print(z$coefficients[2,4])
  }
}

x<- c()
for (i in 1:1413){
  x <- append(x, date_plot(i))
}

#print(x)

#lm(rmp.1$order_date ~rmp.1$student_star)

#date_plot(1)
x <- na.omit(x)

hist(x, main = "Histogram of P-Values for Slope")

xbar <- mean(x)
n <- length(x)
s <- sd(x)
q <- (xbar - .0025)/(s/sqrt(n))

2*pnorm(q, lower.tail=FALSE)

date_plot1 <-function(x){
  rmp.1 <- rmp %>% filter(professor_name == rmpnew$professor_name[x])
  rmp.1$post_date <- format(as.Date(rmp.1$post_date, "%m/%d/%Y"))
  rmp.1 <- rmp.1[rev(order(as.Date(rmp.1$post_date, format="%m/%d/%Y"))),]
  sd <- sd(rmp.1$student_star)
  x <- nrow(rmp.1)
  y <- rep(1:x)
  rmp.1 <- rmp.1 %>% mutate(order_date=y)
  z <- summary(lm(rmp.1$order_date ~rmp.1$student_star))
  if (is.na(sd) != TRUE & sd != 0 & x > 7){
    print(z$coefficients[2])
  }
}

y<- c()
for (i in 1:1413){
  y <- append(y, date_plot1(i))
}

hist(y, main = "Histogram of Slope Coefficients")
y <- na.omit(y)

ybar <- mean(y)
ny <- length(y)
sy <- sd(y)
qy <- (ybar)/(sy/sqrt(ny))

2*pnorm(abs(qy), lower.tail=FALSE)
