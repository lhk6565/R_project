trojan_detection = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/Trojan_Detection.csv')

trojan_T = trojan_detection[trojan_detection$Class=='Trojan',]
trojan_B = trojan_detection[trojan_detection$Class=='Benign',]

trojan_sampletrain = sample(1:nrow(trojan_T), 2000)
trojan_sampleT = sample(1:nrow(trojan_T), 1000)
trojan_sampleB = sample(1:nrow(trojan_B), 1000)

trojan_train = trojan_T[trojan_sampletrain,10:85]
trojan_Ta = trojan_T[trojan_sampleT,10:85]
trojan_Ba = trojan_B[trojan_sampleB,10:85]


str(trojan_train)
str(trojan_Ba)
plot(colMeans(trojan_train))
plot(colMeans(trojan_Ba))
colSums(trojan_train)
colSums(trojan_T)
colSums(trojan_B)
sum(is.na(trojan_detection))

###분산이 0인 열을 지워주는것
trojan_train <-trojan_train[ , which(apply(trojan_train, 2, var) != 0)]
trojan_Ta <-trojan_Ta[ , which(apply(trojan_Ta, 2, var) != 0)]
trojan_Ba <-trojan_Ba[ , which(apply(trojan_Ba, 2, var) != 0)]

trojan_train <-trojan_train[ , colMeans(trojan_train) >= 1]
trojan_Ta <-trojan_Ta[ ,  colMeans(trojan_Ta) >= 1]
trojan_Ba <-trojan_Ba[ ,  colMeans(trojan_Ba) >= 1]

trojan_train_pca = prcomp(trojan_train,scale=T)
plot(trojan_train_pca$x[,1:2])

trojan_Ba_pca = prcomp(trojan_Ba,scale=T)
points(trojan_Ba_pca$x[,1:2],col='red')
cor(trojan_Ba)

plot(colMeans(trojan_train),type='o' ,ylim=c(0,2))
points(colMeans(trojan_Ba),type='o',col='red')
###############
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
