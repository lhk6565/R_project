breast_cancer = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/breast-cancer.csv', header = T)

brt_cancer_M = breast_cancer[breast_cancer$diagnosis=='M',]
brt_cancer_B = breast_cancer[breast_cancer$diagnosis=='B',]

brt_cancer_test = brt_cancer_B[1:300,]
brt_cancer_B = brt_cancer_B[1:150,3:32]
brt_cancer_M = brt_cancer_M[1:150,3:32]
brt_cancer_test = brt_cancer_test[,3:32]


brt_total = rbind(brt_cancer_B,brt_cancer_M)

brt_t2 = fasthtsq(brt_cancer_test,brt_total,0.05)

plot(brt_t2$Tsq_mat,ylim=c(0,3000))
abline(v=c(150),col='blue',lwd=3)
abline(h=c(brt_t2$CL),col='red',lwd=3,lty=2)

#1사분면
sum(brt_t2$Tsq_mat[1:150]>brt_t2$CL)/150 #Alpha error

#4사분면
sum(brt_t2$Tsq_mat[151:300]<brt_t2$CL)/150 #Beta error
########

#fasthtsq


ab_error = matrix(0,100,2)

for(i in 1:100){
  aa = fasthtsq(brt_cancer_test,brt_total,i/100)
  
  al_error = sum(aa$Tsq_mat[1:150]>aa$CL)/1000
  be_error = sum(aa$Tsq_mat[151:300]<aa$CL)/1000
  
  ab_error[i,1] = al_error
  ab_error[i,2] = be_error
}

plot(ab_error)
