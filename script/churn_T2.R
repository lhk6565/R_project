churn = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/customer_churn_train.csv')

churn_T = churn[churn$Churn. == 'True.',]
churn_F = churn[churn$Churn. == 'False.',]

sample_tr =  sample(1:nrow(churn_F),800)
sample_f =  sample(1:nrow(churn_F),400)
sample_t =  sample(1:nrow(churn_T),400)
View(churn)
churn_tr = churn_F[sample_tr,c(7:20)]
churn_f = churn_F[sample_f,c(7:20)]
churn_t = churn_T[sample_t,c(7:20)]

Ttest(churn_f,churn_t)
churn_tr = churn_tr[,c(1:2,4:5,7,11:14)]
churn_f = churn_f[,c(1:2,4:5,7,11:14)]
churn_t = churn_t[,c(1:2,4:5,7,11:14)]

ksk2(churn_tr,churn_t,30000)
churn_tr = churn_tr[,c(5,2,11,4,10,7,13)]
churn_f = churn_f[,c(5,2,11,4,10,7,13)]
churn_t = churn_t[,c(5,2,11,4,10,7,13)]

tsquare(churn_tr,churn_f,churn_t,100)
