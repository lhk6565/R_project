wine_tr = wine_good[wine_sampletr,1:11]
wine_g = wine_good[wine_sampleg,1:11]
wine_b = wine_bad[wine_sampleb,1:11]

######################
# FDR
wine_tr = wine_tr[,c(1:3,6:8,10:11)]
wine_g = wine_g[,c(1:3,6:8,10:11)]
wine_b = wine_b[,c(1:3,6:8,10:11)]

# SDV_solve
wine_ksk2 = ksk2(wine_g,wine_b,30000)
wine_ksk2$or.mat

wine_tr = wine_tr[,c(4,6,7)]
wine_g = wine_g[,c(4,6:7)]
wine_b = wine_b[,c(4,6:7)]

#ginv
wine_tr = wine_tr[,c(6:8)]
wine_g = wine_g[,c(6:8)]
wine_b = wine_b[,c(6:8)]

#scale
wine_tr = wine_tr[,c(2,5:7,9:11)]
wine_g = wine_g[,c(2,5:7,9:11)]
wine_b = wine_b[,c(2,5:7,9:11)]
######################

wine_total = rbind(wine_g,wine_b)
wine_t2 = fasthtsq(wine_tr,wine_total,0.05)

plot(wine_t2$Tsq_mat,ylim=c(0,500))
abline(v=c(250),col='blue',lwd=3)
abline(h=c(wine_t2$CL),col='red',lwd=3,lty=2)

#ACC
tp = sum(wine_t2$Tsq_mat[1:250]<wine_t2$CL)
tn = sum(wine_t2$Tsq_mat[251:500]>wine_t2$CL)
fn = sum(wine_t2$Tsq_mat[1:250]>wine_t2$CL)
fp = sum(wine_t2$Tsq_mat[251:500]<wine_t2$CL)

#Accuracy 정분류율
(tn+tp)/(tn+tp+fn+fp)

#1사분면
sum(wine_t2$Tsq_mat[1:250]>wine_t2$CL)/250 #Alpha error

#4사분면
sum(wine_t2$Tsq_mat[251:500]<wine_t2$CL)/250 #Beta error

