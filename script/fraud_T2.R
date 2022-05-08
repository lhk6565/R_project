fraud = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/fraud_dataset_example.csv')

fraud_T = fraud[fraud$isFraud == '1',]
fraud_F = fraud[fraud$isFraud == '0',]

sample_tr =  sample(1:nrow(fraud_F),200)
sample_f =  sample(1:nrow(fraud_F),100)
sample_t =  sample(1:nrow(fraud_T),100)

fraud_tr = fraud_F[sample_tr,c(3,5:6,8:9)]
fraud_f = fraud_F[sample_f,c(3,5:6,8:9)]
fraud_t = fraud_T[sample_t,c(3,5:6,8:9)]

Ttest(fraud_f,fraud_t)
fraud_tr = fraud_tr[,c(1,3)]
fraud_f = fraud_f[,c(1,3)]
fraud_t = fraud_t[,c(1,3)]

ksk2(fraud_f,fraud_t,30000)
fraud_tr = fraud_tr[,c(1:3)]
fraud_f = fraud_f[,c(1:3)]
fraud_t = fraud_t[,c(1:3)]

tsquare(fraud_tr,fraud_f,fraud_t,100)
