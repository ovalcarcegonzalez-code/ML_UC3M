library(MASS)
set.seed(2)
## simulate the two Gaussian groups (this data is taken from the original slides in aula global)
make_data <- function(n) {
  S <- matrix(c(3, 0.2, 0.2, 0.2), 2, 2)
  X <- rbind(mvrnorm(n, c(4, 1), S), mvrnorm(n, c(1, 3), S))
  data.frame(x1 = X[, 1], x2 = X[, 2], class = factor(rep(c("C1", "C2"), each = n)))
}
train <- make_data(100)
test <- make_data(25)
## fit models
fit.lda <- lda(class ~ x1 + x2, data = train)
train$t <- as.numeric(train$class == "C1") # t = 1 -> C1
fit.lr <- glm(t ~ x1 + x2, family = binomial, data = train)
## misclassification rate on the test set
pred.lda <- predict(fit.lda, test)$class
p.lr <- predict(fit.lr, newdata = test, type = "response") # P(C1 | x)
pred.lr <- factor(ifelse(p.lr >= 0.5, "C1", "C2"), levels = levels(test$class))
c(LDA = mean(pred.lda != test$class), LR = mean(pred.lr != test$class))
## decision boundaries
b <- coef(fit.lr) # b0 + b1*x1 + b2*x2 = 0
w <- fit.lda$scaling[, 1] # Fisher direction (arbitrary scale/sign)
c0 <- sum(w * colMeans(fit.lda$means)) # midpoint threshold: w’(m1 + m2)/2
plot(train$x1, train$x2, pch = 19, col = ifelse(train$class == "C1", "blue", "red"))
abline(a = -b[1] / b[3], b = -b[2] / b[3], lwd = 2) # LogReg
abline(a = c0 / w[2], b = -w[1] / w[2], lwd = 2, lty = 2) # LDA (cortada)


