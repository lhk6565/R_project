### 중간고사 4번
f = function(x){
  result = -3*x^2 + 8*x + 7
  return(result)
}
plot(f(seq(-10,10)))

integrate(f,0.01,0.99)

###중간고사 5번

df = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/mid-term-sales.csv')

sum(is.na(df))
View(df)
summary(df)
new = na.omit(df)
rm(new)

new = as.matrix(new)
x = new[,2:4]
y = new[,5]

ha = solve(t(x) %*% x) %*% t(x) %*% y
predict = x%*%ha
plot(predict)
plot(y,predict)
cor(y,predict)
