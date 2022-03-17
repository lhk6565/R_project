
tr_sample = sample(1:nrow(heart_disease_F),2000)
f_sample = sample(1:nrow(heart_disease_F),1000)
t_sample = sample(1:nrow(heart_disease_T),1000)

disease_tr = heart_disease_F[tr_sample,2:22]
disease_f = heart_disease_F[f_sample,2:22]
disease_t = heart_disease_T[t_sample,2:22]

######################
# FDR
disease_tr = disease_tr[,c(1:8,10:11,13:21)]
disease_f = disease_f[,c(1:8,10:11,13:21)]
disease_t = disease_t[,c(1:8,10:11,13:21)]

# SDV_solve
disease_ksk2 = ksk2(disease_f,disease_t,30000)
disease_ksk2$or.mat

disease_tr = disease_tr[,c(1:2,4:5,7:10,12:14,16:19)]
disease_f = disease_f[,c(1:2,4:5,7:10,12:14,16:19)]
disease_t = disease_t[,c(1:2,4:5,7:10,12:14,16:19)]

#ginv
disease_tr = disease_tr[,c(2,4:5,7:11,14:16,18:21)]
disease_f = disease_f[,c(2,4:5,7:11,14:16,18:21)]
disease_t = disease_t[,c(2,4:5,7:11,14:16,18:21)]

#scale
disease_tr = disease_tr[,c(1:4,6,8:9,10:13,14:19,20)]
disease_f = disease_f[,c(1:4,6,8:9,10:13,14:19,20)]
disease_t = disease_t[,c(1:4,6,8:9,10:13,14:19,20)]
######################

disease_total = rbind(disease_f,disease_t)
disease_t2 = fasthtsq(disease_tr,disease_total,0.05)

plot(disease_t2$Tsq_mat,ylim=c(0,3000))
abline(v=c(1000),col='blue',lwd=3)
abline(h=c(disease_t2$CL),col='red',lwd=3,lty=2)

#ACC
tp = sum(disease_t2$Tsq_mat[1:1000]<disease_t2$CL)
tn = sum(disease_t2$Tsq_mat[1001:2000]>disease_t2$CL)
fn = sum(disease_t2$Tsq_mat[1:1000]>disease_t2$CL)
fp = sum(disease_t2$Tsq_mat[1001:2000]<disease_t2$CL)

#Accuracy 정분류율
(tn+tp)/(tn+tp+fn+fp)

#1사분면
sum(disease_t2$Tsq_mat[1:1000]>disease_t2$CL)/1000 #Alpha error

#4사분면
sum(disease_t2$Tsq_mat[1001:2000]<disease_t2$CL)/1000 #Beta error

