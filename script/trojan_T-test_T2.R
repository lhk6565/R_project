

trojan_train = trojan_T[trojan_sampletrain,10:85]
trojan_Ta = trojan_T[trojan_sampleT,10:85]
trojan_Ba = trojan_B[trojan_sampleB,10:85]

###분산이 0인 열을 지워주는것
trojan_train <-trojan_train[ , which(apply(trojan_train, 2, var) != 0)]
trojan_Ta <-trojan_Ta[ , which(apply(trojan_Ta, 2, var) != 0)]
trojan_Ba <-trojan_Ba[ , which(apply(trojan_Ba, 2, var) != 0)]

##### FDR
trojan_train = trojan_train[,c(1,2)]
trojan_Ta = trojan_Ta[,c(1,2)]
trojan_Ba = trojan_Ba[,c(1,2)]

##### KSK2
trojan_ksk2 = ksk2(trojan_Ta,trojan_Ba,30000)
trojan_ksk2$or.mat
##### ginv
trojan_train = trojan_train[,c(1:2,5:8,10:13,17,19,24,29,34,36:37,39:40,41:47,49,51,55:56)]
trojan_Ta = trojan_Ta[,c(1:2,5:8,10:13,17,19,24,29,34,36:37,39:40,41:47,49,51,55:56)]
trojan_Ba = trojan_Ba[,c(1:2,5:8,10:13,17,19,24,29,34,36:37,39:40,41:47,49,51,55:56)]
##### scale
trojan_train = trojan_train[,c(1:3,7:10,12,14,16,20:21,26:27,29,32,35:37,40:41,43,45:46,49,51,53:54,57,63)]
trojan_Ta = trojan_Ta[,c(1:3,7:10,12,14,16,20:21,26:27,29,32,35:37,40:41,43,45:46,49,51,53:54,57,63)]
trojan_Ba = trojan_Ba[,c(1:3,7:10,12,14,16,20:21,26:27,29,32,35:37,40:41,43,45:46,49,51,53:54,57,63)]

trojan_total = rbind(trojan_Ta,trojan_Ba)
trojan_t2 = fasthtsq(trojan_train,trojan_total,0.05)

plot(trojan_t2$Tsq_mat,ylim=c(0,3000))
abline(v=c(1000),col='blue',lwd=3)
abline(h=c(trojan_t2$CL),col='red',lwd=3,lty=2)

#ACC
tp = sum(trojan_t2$Tsq_mat[1:1000]<trojan_t2$CL)
tn = sum(trojan_t2$Tsq_mat[1001:2000]>trojan_t2$CL)
fn = sum(trojan_t2$Tsq_mat[1:1000]>trojan_t2$CL)
fp = sum(trojan_t2$Tsq_mat[1001:2000]<trojan_t2$CL)

#Accuracy 정분류율
(tn+tp)/(tn+tp+fn+fp)

#1사분면
sum(trojan_t2$Tsq_mat[1:1000]>trojan_t2$CL)/1000 #Alpha error

#4사분면
sum(trojan_t2$Tsq_mat[1001:2000]<trojan_t2$CL)/1000 #Beta error

