#ph1 -> 정상 훈련데이터
#ph2_in -> 정상 검정데이터
#ph2_out -> 비정상 검정데이터
#fasthtsq -> tr, te, alpha 라는 3가지 파라미터가 필요합니다
# trdat -> ph1
# tedat -> ph2_in, ph2_out
# 0.05

###
ph1 = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph1.csv')
ph2 = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph2.csv')
ph2_out = read.csv('C:/Users/lhk65/OneDrive/Desktop/Data/ph2_out.csv')

str(ph1)
ph1 = ph1[,8:49]
ph2 = ph2[,8:49]
ph2_out = ph2_out[,8:49]

ph_total = rbind(ph2,ph2_out)

ph_t2 = fasthtsq(ph1,ph_total,0.05)

plot(ph_t2$Tsq_mat,ylim=c(0,3000))
abline(v=c(1000),col='blue',lwd=3)
abline(h=c(ph_t2$CL),col='red',lwd=3,lty=2)

#1사분면
sum(ph_t2$Tsq_mat[1:1000]>ph_t2$CL)/1000 #Alpha error

#4사분면
sum(ph_t2$Tsq_mat[1001:2000]<ph_t2$CL)/1000 #Beta error
########
#ph_t2$CL

#fasthtsq

dim(ph2)
dim(ph2_out)

names(ph2)
names(ph2_out) = names(ph2)
names(ph2_out)
ph_total = rbind(ph2,ph2_out)

ab_error = matrix(0,100,2)

for(i in 1:100){
  aa = fasthtsq(ph1,ph_total,i/100)
  
  al_error = sum(aa$Tsq_mat[1:1000]>aa$CL)/1000
  be_error = sum(aa$Tsq_mat[1001:2000]<aa$CL)/1000
  
  ab_error[i,1] = al_error
  ab_error[i,2] = be_error
}

plot(ab_error)
