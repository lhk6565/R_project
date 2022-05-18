breast_cancer = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/breast-cancer.csv', header = T)

brt_cancer_M = breast_cancer[breast_cancer$diagnosis=='M',]
brt_cancer_B = breast_cancer[breast_cancer$diagnosis=='B',]

sample_tr =  sample(1:nrow(brt_cancer_B),300)
sample_t =  sample(1:nrow(brt_cancer_B),150)
sample_f =  sample(1:nrow(brt_cancer_M),150)

brt_tr = brt_cancer_B[sample_tr,3:32]
brt_B = brt_cancer_B[sample_t,3:32]
brt_M = brt_cancer_M[sample_f,3:32]

Ttest(brt_B,brt_M)
brt_tr = brt_tr[,c(1:9,11,13:14,16:18,21:30)]
brt_B = brt_B[,c(1:9,11,13:14,16:18,21:30)]
brt_M = brt_M[,c(1:9,11,13:14,16:18,21:30)]

ksk2(brt_tr,brt_M,1000)
brt_tr = brt_tr[,c(28,1,26,21,3,25,23,5,22,6,4,9,27,7,2)]
brt_B = brt_B[,c(28,1,26,21,3,25,23,5,22,6,4,9,27,7,2)]
brt_M = brt_M[,c(28,1,26,21,3,25,23,5,22,6,4,9,27,7,2)]

tsquare(brt_tr,brt_B,brt_M,500)

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
