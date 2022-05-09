heart_disease = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/heart_disease_health_indicators_BRFSS2015.csv')

heart_disease_F = heart_disease[heart_disease$HeartDiseaseorAttack == 0,]
heart_disease_T = heart_disease[heart_disease$HeartDiseaseorAttack == 1,]

tr_sample = sample(1:nrow(heart_disease_T),2000)
t_sample = sample(1:nrow(heart_disease_T),1000)
f_sample = sample(1:nrow(heart_disease_F),1000)

disease_tr = heart_disease_T[tr_sample,2:22]
disease_f = heart_disease_F[f_sample,2:22]
disease_t = heart_disease_T[t_sample,2:22]

disease_tr_pca = prcomp(disease_tr,scale=T)
plot(disease_tr_pca$x[,1:2])

disease_t_pca = prcomp(disease_t,scale=T)
points(disease_t_pca$x[,1:2],col='red')

Ttest(disease_t,disease_f)
disease_tr = disease_tr[,c(1:8,10:21)]
disease_t = disease_t[,c(1:8,10:21)]
disease_f = disease_f[,c(1:8,10:21)]

ksk2(disease_tr,disease_f,50000)
disease_tr = disease_tr[,c(15,16,4,19,21,20,7,14,5,9,18,8,2,10,1)]
disease_t = disease_t[,c(15,16,4,19,21,20,7,14,5,9,18,8,2,10,1)]
disease_f = disease_f[,c(15,16,4,19,21,20,7,14,5,9,18,8,2,10,1)]

tsquare(disease_tr,disease_t,disease_f,100)
