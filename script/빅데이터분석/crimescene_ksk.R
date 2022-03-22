drug = matrix(0,10,3)

drug[1:10,1] = c(1:10)
drug[1:10,2] = c(2,5,5,5,1,5,7,4,5,6)
drug[1:10,3] = c(0,4,1,2,8,9,5,2,4,1)


plot(drug[,2:3], cex=2)

d.seller = matrix(0,3,4)

d.seller[1:3,1] = c('Mike','David','Chulsoo')
d.seller[1:3,2] = c(1,4,8)
d.seller[1:3,3] = c(5,7,1)
d.seller[1:3,4] = c(5,1,3)
d.seller

drug = as.data.frame(drug)
d.seller = as.data.frame(d.seller)

str(drug)
names(drug) = c('site','x','y')
names(d.seller) = c('names','x','y','The number of crime')

drug
d.seller

plot(drug[,2:3], cex=2, ylim=c(0,10), xlim=c(0,10))
points(d.seller[,2],d.seller[,3], col='red', cex=3)



who = function(drug,d.seller,q){
dist_mat = matrix(0,nrow(drug),nrow(d.seller))
by.dist = matrix(0,nrow(drug),nrow(d.seller))
for(i in 1:nrow(drug)){
 for(j in 1:nrow(d.seller)){
  d1 =  sqrt(t(as.matrix((as.double(drug[i,2:3]) - as.double(d.seller[j,2:3])))) %*% as.matrix((as.double(drug[i,2:3]) - as.double(d.seller[j,2:3]))))
dist_mat[i,j] = d1
by.dist[,j] = dist_mat[,j]/as.double(d.seller$`The number of crime`)[j] 
 }
}
  dist_mat = as.data.frame(dist_mat)
  names(dist_mat) = c(d.seller$names)
  by.dist = as.data.frame(by.dist)
  names(by.dist) = c(d.seller$names)
  who = matrix(0,nrow(by.dist),1)
  
  
  for(k in 1:nrow(by.dist)){
  who[k,] = which.min(by.dist[k,])
  }
  answer_that_you_want_to_know = which.min(by.dist[q,])
ret = list(dist_mat = dist_mat,
           by.dist = by.dist,
           who = who,
           answer_that_you_want_to_know  = answer_that_you_want_to_know )
return(ret)
}

crime = who(drug,d.seller,10)
crime

## dist_mat -> 각 범죄자로부터 모든 범죄 장소까지의 거리를 구한 뒤, 범죄 횟수로 나누기 전의 거리
## by.dist ->각 범죄자로부터 모든 범죄 장소까지의 거리를 구한 뒤, 범죄 횟수로 나눈 거리
## who ->각 범죄 장소로부터 범죄 횟수로 나눈 뒤, 가장 가까운 인물의 번호를 나타내는 matrix(1==Mike, 2==David, 3==Chulsoo)
## answer that you want to know -> 입력한 번호 q(범죄 장소 번호)의 범인이 누구인지 나타냄