n <- 100
theta <- 1
omega <- 2
e3 <- function(X){ X[1] }
e4 <- function(X){
  X<- sort(X)
  ANS<-c()
  iter <- 0
  for(i in 1:length(X)){
    ANS <- append(ANS, (dnorm(qnorm(iter+0.000000001)) -dnorm(qnorm(iter+ 1/length(X)-0.000000001))))
    iter <- iter + 1/length(X)
  }
  return(sum(X*ANS))
}
mean_ans <- c()
median_ans <- c()
first_ans <- c()
weigh_ans <- c()
for(i in 1:10000){
  data <- rnorm(n,theta,omega)
  mean_ans <- append(mean_ans,mean(data))
  median_ans <- append(median_ans,median(data))
  first_ans <- append(first_ans,e3(data))
  weigh_ans <- append(weigh_ans,e4(data))
}
var_mean_ans <- var(mean_ans)
var_median_ans <- var(median_ans)
var_first_ans <- var(first_ans)
var_weigh_ans <- var(weigh_ans)

mse_mean_ans <- mean((mean_ans-theta)^2)
mse_median_ans <- mean((median_ans-theta)^2)
mse_first_ans <- mean((first_ans-theta)^2)
mse_weigh_ans <- mean((weigh_ans-theta)^2)

b_mean_ans <- mean(mean_ans)-theta
b_median_ans <- mean(median_ans)-theta
b_first_ans <- mean(first_ans)-theta
b_weigh_ans <- mean(weigh_ans)-theta

X <- c()
n<-50
newtons_method <- function(f,df,x,k){
  for(i in 1:k){
    x <- x-10^(-1)*(f(x)/df(x))
    #print(x)
  }
  if(is.na(x) || is.infinite(x)){
    return(NaN)
  }
  if(f(x) < 10^(-6)&&abs(x-0.7)<0.5){
    return (x)
  }else {
      return(NaN)
  }
}

location <- 1
scale <- 1
log_fun <- function(theta){ -sum(tanh((theta-X)/(2*scale))/scale)}
dlog_fun <- function(theta){ sum(-1/((scale^2*(cosh((theta*X)/scale)+1))))}
cau_fun <- function(theta){ sum((2*(X-theta)/(theta^2-2*theta*X+X^2+scale^2)))}
dcau_fun <- function(theta){ sum((2*(theta^2-2*theta*X+X^2-scale^2)/(theta^2-2*theta*X+X^2+scale^2)^2))}
odp <- c()
i<-0
while(i < 10000){
  #X <- rlogis(n, location, scale)
  X <- rcauchy(n, location, scale)
  tmp <- newtons_method(cau_fun,dcau_fun,0.7,10^3)
  if(!is.na(tmp)){
    odp <- append(odp,tmp)
    i <- i+1
  }
  print(length(odp))
}
odp_var <- var(odp)
odp_mse <- mean((odp-location)^2)
odp_b <- mean(odp)-location
location <- 1
scale <- 1
mse1 <- c()
mse2 <- c()
odp <- c()
for(j in 25:100){
  i <- 0
  while(i < 100){
    X <- rlogis(j, location, scale)
    tmp <- newtons_method(log_fun,dlog_fun,0.7,20)
    if(!is.na(tmp)){
      odp <- append(odp,tmp)
      i <- i+1
    }
  }
  print(j)
  mse1 <- append(mse1,mean((odp-location)^2))
  odp <- c()
}
mse2 <- c()
odp <- c()
for(j in 25:100){
  i <- 0
  while(i < 100){
    X <- rcauchy(j, location, scale)
    tmp <- newtons_method(cau_fun,dcau_fun,0.7,20)
    if(!is.na(tmp)){
      odp <- append(odp,tmp)
      i <- i+1
    }
  }
  print(j)
  mse2 <- append(mse2,mean((odp-location)^2))
  odp <- c()
}
library(ggplot2)
xValue <- 25:100
yValue <- mse1
data <- data.frame(xValue,yValue)
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()

xValue <- 25:100
yValue <- mse2
data <- data.frame(xValue,yValue)
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()

