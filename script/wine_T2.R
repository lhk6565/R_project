wine = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/wine.csv')

wine_good = wine[wine$quality == 'good',]
wine_bad = wine[wine$quality == 'bad',]

wine_sampletr = sample(1:nrow(wine_good),500)
wine_sampleg = sample(1:nrow(wine_good),250)
wine_sampleb = sample(1:nrow(wine_bad),250)

wine_tr = wine_good[wine_sampletr,1:11]
wine_g = wine_good[wine_sampleg,1:11]
wine_b = wine_bad[wine_sampleb,1:11]

###############################
wine_tr_pca = prcomp(wine_tr,scale=T)
plot(wine_tr_pca$x[,1:2])

wine_b_pca = prcomp(wine_b,scale=T)
points(wine_b_pca$x[,1:2],col='red')
###############################

wine_total = rbind(wine_g,wine_b)
wine_t2 = fasthtsq(wine_tr,wine_total,0.05)

plot(wine_t2$Tsq_mat,ylim=c(0,3000))
abline(v=c(250),col='blue',lwd=3)
abline(h=c(wine_t2$CL),col='red',lwd=3,lty=2)

#ACC
tp = sum(wine_t2$Tsq_mat[1:250]<wine_t2$CL)
tn = sum(wine_t2$Tsq_mat[251:500]>wine_t2$CL)
fn = sum(wine_t2$Tsq_mat[1:250]>wine_t2$CL)
fp = sum(wine_t2$Tsq_mat[251:500]<wine_t2$CL)

#Accuracy 정분류율
(tn+tp)/(tn+tp+fn+fp)

#1사분면
sum(wine_t2$Tsq_mat[1:250]>wine_t2$CL)/250 #Alpha error

#4사분면
sum(wine_t2$Tsq_mat[251:500]<wine_t2$CL)/250 #Beta error
