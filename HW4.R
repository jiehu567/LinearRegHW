# HW4
# b. join family confidence interval
transfers <- c(1,0,2,0,3,1,0,1,2,0)
breaks <- c(16,9,17,12,22,13,8,15,19,11)
data_4 <- data.frame(transfers, breaks)

model_4 <- lm(breaks~transfers,data=data_4)

confint(model_4,level = 0.995)




# 5.1
A = matrix(c(1,4,2,6,3,8),ncol=2)
B = matrix(c(1,3,1,4,2,5),ncol=2)
A+B
A-B
A%*%B
A%*%t(B)
t(B)%*%A

# 5.9
A = matrix(c(0,1,8,0,3,1,0,5,5), ncol = 3)

rank


# 6.6

data_6 <- matrix(c(64.0,4.0,2.0,
                   73.0,4.0,4.0,
                   61.0,4.0,2.0,
                   76.0,4.0,4.0,
                   72.0,6.0,2.0,
                   80.0,6.0,4.0,
                   71.0,6.0,2.0,
                   83.0,6.0,4.0,
                   83.0,8.0,2.0,
                   89.0,8.0,4.0,
                   86.0,8.0,2.0,
                   93.0,8.0,4.0,
                   88.0,10.0,2.0,
                   95.0,10.0,4.0,
                   94.0,10.0,2.0,
                   100.0,10.0,4.0), nrow = 3)

data_6 <- data.frame(t(data_6))

names(data_6)=c("Y","X1","X2")

model_6 <- lm(Y~X1+X2, data = data_6)
summary(model_6)

# 6.8 a
confint(model_6,level = 1-0.01/4)
v <- vcov(model_6)
Xh <- c(1,5,4)
se <- t(Xh) %*% v %*% Xh
Yh <- 37.65 + 4.4250 * 5 + 4.3750 * 4
c(Yh - qt(0.995,13)*se, Yh + qt(0.995,13)*se)


#6.8 b
v <- vcov(model_6)
Xh <- c(1,5,4)
B <- qt(1-0.01/(2*2),13)
s_pred <- t(Xh) %*% v %*% Xh + 1
Yh <- 37.65 + 4.4250 * 5 + 4.3750 * 4
c(Yh - B*s_pred, Yh + B*s_pred)


