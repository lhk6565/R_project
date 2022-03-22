
site = data.frame(site = c(1:10),
                  x = c(2,5,5,5,1,5,7,4,5,6),
                  y = c(0,4,1,2,8,9,5,2,4,1))
seller = data.frame(x = c(1,4,8),
                    y = c(5,7,1),
                    history = c(5,1,3))

row.names(seller) = c('Axy','Bxy','Cxy')

######################
str(site)
seller

distance = function(seller,site){
  dist_mat = matrix(0,10,1)
  for(i in 1:10){
    temp = sqrt( (seller[1]-site[i,2])^2+(seller[2]-site[i,3])^2 )
    dist_mat[i] = temp
  }
  return(dist_mat)
}

a = distance(seller,site)
a = as.data.frame(a)
rownames(a) = c('A','B','C')
colnames(a) = c(1:10)
a = a/seller$history
a = t(a)
a

who = function(dis){
  crimescene = data.frame(who = nrow(dis))
  for(i in 1:10){
    b = names(which.min(dis[i,]))
    crimescene[i,] = b
  }
  return(crimescene)
}

who(a)
