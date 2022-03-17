heart_disease = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/heart_disease_health_indicators_BRFSS2015.csv')

heart_disease_F = heart_disease[heart_disease$HeartDiseaseorAttack == 0,]
heart_disease_T = heart_disease[heart_disease$HeartDiseaseorAttack == 1,]

tr_sample = sample(1:nrow(heart_disease_F),2000)
f_sample = sample(1:nrow(heart_disease_F),1000)
t_sample = sample(1:nrow(heart_disease_T),1000)

disease_tr = heart_disease_F[tr_sample,2:22]
disease_f = heart_disease_F[f_sample,2:22]
disease_t = heart_disease_T[t_sample,2:22]

disease_tr_pca = prcomp(disease_tr,scale=T)
plot(disease_tr_pca$x[,1:2])

disease_t_pca = prcomp(disease_t,scale=T)
points(disease_t_pca$x[,1:2],col='red')

########################
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
