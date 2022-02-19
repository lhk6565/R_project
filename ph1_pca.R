##공정 데이터로 방향을 바꾸려고 합니다
## 상기한테 받은 데이터로 지금 EDA 진행하고 있습니다
#########################
## PCA
## 상관관계
## 통계량 비교
##

##[,8:49] -> 숫자 변수에요
read.csv(ph1)

ph1 = ph1[,8:49]

dim(ph_total)
ph_total = rbind(ph1,ph2_out)

ph1_pca = prcomp(ph1,scale=T)
plot(ph1_pca$x[,1:2])

ph2_pca = prcomp(ph2,scale=T)
plot(ph2_pca$x[,1:2])

ph2_out_pca = prcomp(ph2_out,scale=T)
points(ph2_out_pca$x[,1:2],col='red')

#################
ph1_prin = princomp(ph1, cor=T)
ph2_out_prin = princomp(ph2_out, cor=T)
summary(ph1_prin)
summary(ph2_out_prin)
screeplot(ph1_prin, npcs=25, type='lines')
screeplot(ph2_out_prin, npcs=25, type='lines')
#################

############
library(corrplot)
###########
names(ph1) = c(1:42)
corrplot(cor(ph1))
names(ph2) = c(1:42)
corrplot(cor(ph2))
names(ph2_out) = c(1:42)
corrplot(cor(ph2_out))
###########
##평균, 표준편차만 보여드려도 낫밷
#########
plot(colMeans(ph1),type='o')
points(colMeans(ph2_out),type='o',col='red')

ph1_stdev = matrix(0,ncol(ph1),1)
ph2_out_stdev = matrix(0,ncol(ph2_out),1)
###
i=1
for(i in 1:ncol(ph1)){
  ph1_stdev[i,] = sd(ph1[,i])
}

for(i in 1:ncol(ph2_out)){
  ph2_out_stdev[i,] = sd(ph2_out[,i])
}
######
plot(ph1_stdev,type='o',ylim=c(0,2000))
points(ph2_out_stdev,type='o',col='red')

####
## 몇 몇의 변수만 표준편차 차이가 크므로, 변수선택법을 고려할 수 있음
## EDA 역량을 기르고 있다!
## 이렇게 결론을 내시면 좋심다