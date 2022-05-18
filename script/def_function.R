tsquare = function(tr,te,te_out,num){
  te_total = rbind(te,te_out)
  tr_t2 = fast_t2(tr,te_total,0.05)
  obs = nrow(tr)/2 #1000
  obs2 = nrow(tr) #2000
  obs3 = obs2+1 #2001
  obs6 = obs2*2 #4000
  obs4 = obs6-obs #3000
  obs5 = obs4+1 #3001
  plot(tr_t2$Tsq_mat[obs3:obs6],ylim=c(0,num))
  abline(v=c(obs),col='blue',lwd=3)
  abline(h=c(tr_t2$CL),col='red',lwd=3,lty=2)
  
  #1사분면
  alpha_er = sum(tr_t2$Tsq_mat[obs3:obs4]>tr_t2$CL)/obs #Alpha error
  #4사분면
  beta_er = sum(tr_t2$Tsq_mat[obs5:obs6]<tr_t2$CL)/obs #Beta error
  
  #Accuracy 정분류율
  tp = sum(tr_t2$Tsq_mat[obs3:obs4]<tr_t2$CL)
  tn = sum(tr_t2$Tsq_mat[obs5:obs6]>tr_t2$CL)
  fn = sum(tr_t2$Tsq_mat[obs3:obs4]>tr_t2$CL)
  fp = sum(tr_t2$Tsq_mat[obs5:obs6]<tr_t2$CL)
  acc = (tn+tp)/(tn+tp+fn+fp)
  
  result = list(alpha = alpha_er, beta = beta_er, ACC = acc)
  return(result)
}

Ttest = function(te,te_out){
  Ttest <- c()
  for (i in 1:dim(te)[2]) {
    Ttest[i] <- t.test(te[,i], te_out[,i])$p.value
  }
  which(Ttest<0.05)
}

fasthtsq=
  function(trdat, tedat, alpha) {
    library(MASS)
    library(pracma)
    obs = nrow(trdat);
    dim = ncol(trdat);
    
    mu = colMeans(trdat);
    
    # Control Limit from F-Dist Based on In-control data
    
    
    CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)));
    
    
    sinv = solve(cov(trdat));   ######상황에 따라 ginv - solve 변환######
    
    
    mu_mat = repmat(mu, nrow(tedat),1);
    dte = tedat-mu_mat;
    
    
    
    Tsq_mat = matrix(numeric(0), nrow(tedat),1) 
    
    for( i in 1:nrow(tedat)) {
      Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])));
    }
    
    ret <- list(
      Tsq_mat =Tsq_mat,
      CL = CL
    )
    return (ret)                                    
  }

###############################

fast_t2 = function(tr,te,alpha){
  library(MASS)
  library(pracma)
  
  s1 = rbind(tr,te)
  s2 = solve(cov(tr))
  
  mu = colMeans(tr)
  mu_mat = repmat(mu, nrow(s1),1);
  dte = s1-mu_mat;
  
  
  Tsq_mat = matrix(0,nrow(dte),1)
  
  for(i in 1:nrow(dte)){
    Tsq_mat[i,]= as.numeric(dte[i,])%*%s2%*%as.numeric(dte[i,])
  }
  
  
  b_cl = bootcl(Tsq_mat[1:nrow(tr),1],alpha)
  CL = b_cl$CL
  return(list(Tsq_mat=Tsq_mat,
              CL=CL))
}


###############################

bootcl = function(tr,alpha){
  s1_mat = matrix(0,1000,1)
  tr =as.numeric(tr)
  for(i in 1:1000){
    s1 = sample(tr,100,replace = T)
    
    s1_mat[i,]=quantile(s1,1-alpha) 
  }
  CL = mean(s1_mat)
  
  return(list(CL=CL))
  
}

###############################

ksk2 = function(tr,te,k){
  library(MASS)
  gap_mat = matrix(0,k,ncol(tr))
  for(i in 1:k){
    tr.sa = tr[sample(1:nrow(tr),1),]
    te.sa = te[sample(1:nrow(te),1),]
    gap = tr.sa - te.sa
    gap_mat[i,] = as.numeric(gap)
  }
  
  
  gap_mat = gap_mat%*%solve(cov(gap_mat))      ######상황에 따라 ginv - solve 변환######
  gap_mat = abs(gap_mat)
  #gap_mat = scale(gap_mat)
  cm.gap_mat = colMeans(gap_mat)
  cm.gap_mat = as.matrix(cm.gap_mat)
  or.mat = cbind(cm.gap_mat,c(1:ncol(tr)))
  or.mat = or.mat[order(or.mat[,1],decreasing=F),]
  
  ret = list(or.mat=or.mat)
  return(ret)
}

###############################
## 수정 ksk2
ksk2 = function(tr,te,k){
  library(MASS)
  gap_mat = matrix(0,k,ncol(tr))
  for(i in 1:k){
    tr.sa = tr[sample(1:nrow(tr),1),]
    te.sa = te[sample(1:nrow(te),1),]
    gap = abs(tr.sa) - abs(te.sa)
    gap_mat[i,] = as.numeric(gap)
  }
  
  
  #gap_mat = gap_mat%*%ginv(cov(gap_mat))
  gap_mat = scale(gap_mat)
  gap_mat = abs(gap_mat)
  
  #gap_mat = scale(gap_mat)
  cm.gap_mat = colMeans(gap_mat)
  cm.gap_mat = as.matrix(cm.gap_mat)
  or.mat = cbind(cm.gap_mat,c(1:ncol(tr)))
  or.mat = or.mat[order(or.mat[,1],decreasing=T),]
  
  ret = list(or.mat=or.mat)
  return(ret)
}

###################################

score = function(t2,pred){
  confusion_matrix = table(y_test_data,pred)
  #cat(confusion_matrix,"\n","\n")
  
  accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
  Precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
  Recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])
  f1score = 2*(Precision*Recall)/(Precision+Recall)
  cat("accuracy: ",accuracy,"\nPrecision: ",Precision,"\nRecall: ",Recall,"\n")
  cat("F1-score: ",f1score,"\n")
  
  error_rate = 1 - accuracy
  cat("error_rate:",error_rate,"\n")
  
  auc = performance(prediction(pred,t2),measure = "auc")
  cat("AUC: ", auc@y.values[[1]])   
}
