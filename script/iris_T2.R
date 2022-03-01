head(iris)

df = iris[,1:4]

df_mat = matrix(0,nrow(df),1)
for(i in 1:nrow(df)){
  df_mat[i,] = as.numeric(df[i,]-colMeans(df))%*%solve(cov(df))%*%as.numeric(df[i,]-colMeans(df))
}
plot(df_mat,type='o')

plot(df[,1])
plot(df[,2])
plot(df[,3])
plot(df[,4])



ph1 = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph1.csv', header = T)
ph2 = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph2.csv', header = T)
ph2_out = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph2_out.csv', header = T)

######
ph1# tr
ph2_in # test_norm
ph2_out # test_abnorm
######

plot(ph_t2$Tsq_mat,type='o',ylim=c(0,2000))

#####
##EDA 할때, 상관관계를 잘 봐라!
library(corrplot)
corrplot(cor(ph1))

ir_t2 = fasthtsq(iris[1:50,1:4],iris[,1:4],0.05)

par(mfrow=c(2,1))
plot(ir_t2$Tsq_mat)
plot(df_mat,type='o')

######