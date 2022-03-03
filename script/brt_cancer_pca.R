
dim(brt_total)
brt_total = rbind(brt_cancer_test,brt_cancer_M)

brt_test_pca = prcomp(brt_cancer_test,scale=T)
plot(brt_test_pca$x[,1:2])

brt_M_pca = prcomp(brt_cancer_M,scale=T)
points(brt_M_pca$x[,1:2],col='red')

#################

#########
plot(colMeans(brt_cancer_test),type='o', ylim=c(0,2000))
points(colMeans(brt_cancer_M),type='o',col='red')

brt_test_stdev = matrix(0,ncol(brt_cancer_test),1)
brt_M_stdev = matrix(0,ncol(brt_cancer_M),1)
###
i=1
for(i in 1:ncol(brt_cancer_test)){
  brt_test_stdev[i,] = sd(brt_cancer_test[,i])
}

for(i in 1:ncol(brt_cancer_M)){
  brt_M_stdev[i,] = sd(brt_cancer_M[,i])
}
######
plot(brt_test_stdev,type='o',ylim=c(0,1000))
points(brt_M_stdev,type='o',col='red')
