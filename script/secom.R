
uci_secom_p = uci_secom[uci_secom$Pass.Fail=='-1',]
uci_secom_f = uci_secom[uci_secom$Pass.Fail=='1',]

secom_sampletr = sample(1:nrow(uci_secom_p), 1000)
secom_sampleP = sample(1:nrow(uci_secom_p), 900)
secom_sampleF = sample(1:nrow(uci_secom_f), 100)

secom_tr = uci_secom_p[secom_sampletr,2:591]
secom_p = uci_secom_p[secom_sampleP,2:591]
secom_f = uci_secom_f[secom_sampleF,2:591]

colSums(secom_tr)
secom_tr <-uci_secomtr[ , which(apply(uci_secomtr, 2, var) != 0)]
secom_p <-uci_secom_p[ , which(apply(uci_secom_p, 2, var) != 0)]
secom_f <-uci_secom_f[ , which(apply(uci_secom_f, 2, var) != 0)]



secom_total = rbind(secom_tr,secom_f)

secomtr_pca = prcomp(secom_tr,scale=T)
plot(secomtr_pca$x[,1:2])

secom_F_pca = prcomp(secom_f,scale=T)
points(secom_F_pca$x[,1:2],col='red')
