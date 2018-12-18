## Basketball Porject ##
## Hongyu Zhang ##

# Read data.
# Dataset source: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/mlr09.html
data <- read.csv("basketball.csv", header=TRUE)
points = data$X5
height = data$X1
weight = data$X2
field = data$X3
free = data$X4
n = length(points)   # 54 players

# Summary statistics of the dataset.
head(data)
summary(points)
summary(height)
summary(weight)
summary(field)
summary(free)

### Simple approach ###
x = cbind(1, field, free)
xtx = t(x) %*% x

# Check multicollinearity
lambda = eigen(xtx)$values
lambda                  # 3 eigenvalues 94.7953878,  0.3816589,  0.1344273
lambda[1] / lambda[3]   # Condition number is 705.1798

xtxi = solve(xtx)
xtx %*% xtxi      # close to identity matrix
# 1.000000e+00  1.199041e-14    0
# 7.105427e-15  1.000000e+00    0
# 7.105427e-15 -7.327472e-15    1

# Calculate beta hat.
beta.hat = xtxi %*% t(x) %*% points
beta.hat
# intercept -15.27738
# field     35.82503
# free      14.79905

hat.matrix = x %*% xtxi %*% t(x)
residual = (diag(n) - hat.matrix) %*% points
# sum of square residuals
SS.res = sum(residual * residual)
SS.res # 1516.422
# mean square residuals
sigma.squared.hat = SS.res/(n - 2)
sigma.squared.hat # 29.16196

# Hypothethis Testing
tvaluebeta1hat <- beta.hat[2] / sqrt(sigma.squared.hat * xtxi[2, 2])
tvaluebeta1hat
pvalue1 = 2 * (1 - pt(abs(tvaluebeta1hat), df = n - 2))
pvalue1

### Stepwise search to find a better model ###

# Forward selection based on BIC
mint <- lm(points~1)
forwardBIC <- step(mint,
                   scope=list(
                     lower=~1,upper=~height+weight+field+free+field:free),
                   direction="forward", k=log(n))

# Create bar plot of these BICs.
fBIC <- c(194.66, 192.07, 191.32, 191.32)
barplot(fBIC, main="Forward selection by BIC: BICs of different models",
        ylab="BIC", space = 0.5,names.arg=c("Start","+ field","+ height", "+ none"),
        ylim = c(180, 200), border="red")

# Calculate the final model
x = cbind(1, height, field)
xtx = t(x) %*% x
xtxi = solve(xtx)

# Check multicollinearity
xtx %*% xtxi
#  1.000000e+00 -7.105427e-15 -2.842171e-14
# -2.842171e-14  1.000000e+00  0.000000e+00
# -6.217249e-15 -5.329071e-15  1.000000e+00

# Calculate beta hat.
beta.hat = xtxi %*% t(x) %*% points
beta.hat
# 15.209793
# -4.034628
# 51.562276

hat.matrix = x %*% xtxi %*% t(x)
residual = (diag(n) - hat.matrix) %*% points
# sum of square residuals
SS.res = sum(residual * residual)
SS.res # 1495.732
# mean square residuals
sigma.squared.hat = SS.res/(n - 2)
sigma.squared.hat # 28.76407

# Check normal assumption.
s = residual/sqrt(sigma.squared.hat)
qqnorm(s, main = "Normal QQ Plot of Standardized Residuals")
qqline(s, col = "blue")
abline(a = 0, b = 1, col = 'red')

# Check homogeneity.
fit_value = x%*%beta.hat
plot(fit_value, residual, main = "Residuals vs. fitted values")
abline(0, 0, col = "red")

# Hypothethis Testing
tvaluebeta1hat <- beta.hat[2] / sqrt(sigma.squared.hat * xtxi[2, 2])
tvaluebeta1hat
pvalue1 = 2 * (1 - pt(abs(tvaluebeta1hat), df = n - 2))
pvalue1

# Confirm the results by using ml function.
m2 <- lm(points~height+field)
summary(m2)

# Player A
predict1 = beta.hat[1] + beta.hat[2] * 6.3 + beta.hat[3] * 0.45
predict1 # 12.99466

# Player B
predict2 = beta.hat[1] + beta.hat[2] * 6.4 + beta.hat[3] * 0.46
predict2 # 13.10682


