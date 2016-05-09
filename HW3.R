
# 3.5 
## Data
transfers <- c(1,0,2,0,3,1,0,1,2,0)
breaks <- c(16,9,17,12,22,13,8,15,19,11)
first <- data.frame(transfers, breaks)
res <-c(1.8,-1.2,-1.2,1.8,-0.2,-1.2,-2.2,0.8,0.8,0.8)
res_sq <- res^2
## c. Stem-and-Leaf plot for residuals
model1 <- lm(breaks~transfers)
summary(model1)
plot(model1)

# The regression line is: y_hat = 4


# Breusch-Pagan test
bptest_result <- bptest(breaks~transfers, data=first, studentize=FALSE)


#3.17
X <- c(0:9)
Y <- c(98,135,162,178,221,232,283,300,374,395)
data_17 <- data.frame(X,Y)
plot(X,Y)
model_17 <- (Y~X)

# b

library(MASS)
lambda_seq <- seq(0.3,0.7,0.1)
boxcox(model_17, lambda = lambda_seq)



# c,d
Y_sqrt <- sqrt(Y)
df <- data.frame(X, Y_sqrt)
mylm <- lm(df$Y_sqrt~df$X)
plot(X, Y_sqrt)
abline(lm(Y_sqrt~X))

plot(mylm)


# 3.18
data18 <- read.csv("data19.csv")
head(data18)

plot(data18$X,data18$Y)

## a.
mylm <- lm(data18$Y~data18$X)
plot(mylm)
summary(mylm)


## b. 
data18$X <- (data18$X)^2 
mylm <- lm(data18$Y~data18$X)
plot(mylm)
plot(data18$X,data18$Y)
summary(mylm)

plot(data18$Y~data18$X)
abline(mylm)
mylm$residuals
