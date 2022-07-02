mysigmoid = function(x){
  return(1/(1+exp(-x)))
}
x = c(-10:10)
x
plot(x, type="l")
plot(mysigmoid(x),type="l")

