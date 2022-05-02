
data = iris

fix(data)
head(data)
summary(data)

fit = lm(data$Petal.Width~data$Sepal.Length+data$Sepal.Width+data$Petal.Length, data=data)
summary(fit)

### hat matrix

ones = matrix(1, nrow=nrow(data), ncol=1)
x = as.matrix(iris[,1:3])
y = as.matrix(iris[,4])
x = cbind(ones,x)
solve(t(x)%*%x) %*% t(x) %*% y # hat matrix
