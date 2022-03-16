fasthtsq=
  function(trdat, tedat, alpha) {
    library(MASS)
    library(pracma)
    obs = nrow(trdat);
    dim = ncol(trdat);
    
    mu = colMeans(trdat);
    
    # Control Limit from F-Dist Based on In-control data
    
    
    CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)));
    
    
    sinv = ginv(cov(trdat));   ######상황에 따라 ginv - solve 변환######
    
    
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

ksk2 = function(tr,te,k){
  library(MASS)
  gap_mat = matrix(0,k,ncol(tr))
  for(i in 1:k){
    tr.sa = tr[sample(1:nrow(tr),1),]
    te.sa = te[sample(1:nrow(te),1),]
    gap = tr.sa - te.sa
    gap_mat[i,] = as.numeric(gap)
  }
  
  
  #gap_mat = gap_mat%*%ginv(cov(gap_mat))      ######상황에 따라 ginv - solve 변환######
  gap_mat = abs(gap_mat)
  gap_mat = scale(gap_mat)
  cm.gap_mat = colMeans(gap_mat)
  cm.gap_mat = as.matrix(cm.gap_mat)
  or.mat = cbind(cm.gap_mat,c(1:ncol(tr)))
  or.mat = or.mat[order(or.mat[,1],decreasing=F),]
  
  ret = list(or.mat=or.mat)
  return(ret)
}

###############################

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
