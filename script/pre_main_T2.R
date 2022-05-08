pre_main = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/predictive_maintenance.csv')

pre_T = pre_main[pre_main$Target == '1',]
pre_F = pre_main[pre_main$Target == '0',]

sample_tr =  sample(1:nrow(pre_F),600)
sample_f =  sample(1:nrow(pre_F),300)
sample_t =  sample(1:nrow(pre_T),300)

pre_tr = pre_F[sample_tr,c(4:8)]
pre_f = pre_F[sample_f,c(4:8)]
pre_t = pre_T[sample_t,c(4:8)]

Ttest(pre_f,pre_t)
pre_tr = pre_tr[,c(1:2,4:5)]
pre_f = pre_f[,c(1:2,4:5)]
pre_t = pre_t[,c(1:2,4:5)]

ksk2(pre_f,pre_t,30000)
pre_tr = pre_tr[,c(1,3:5)]
pre_f = pre_f[,c(1,3:5)]
pre_t = pre_t[,c(1,3:5)]

tsquare(pre_tr,pre_f,pre_t,100)
