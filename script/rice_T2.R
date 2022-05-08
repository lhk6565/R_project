rice = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/riceClassification.csv')

rice_T = rice[rice$Class == '1',]
rice_F = rice[rice$Class == '0',]

sample_tr =  sample(1:nrow(rice_T),4000)
sample_f =  sample(1:nrow(rice_T),2000)
sample_t =  sample(1:nrow(rice_T),2000)

rice_tr = rice_T[sample_tr,c(3:5,7:11)]
rice_t = rice_T[sample_t,c(3:5,7:11)]
rice_f = rice_F[sample_f,c(3:5,7:11)]

tsquare(rice_tr,rice_t,rice_f,100)
