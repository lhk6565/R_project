fasthtsq=
  function(trdat, tedat, alpha) {
    library(MASS)
    library(pracma)
    obs = nrow(trdat);
    dim = ncol(trdat);
    
    mu = colMeans(trdat);
    
    # Control Limit from F-Dist Based on In-control data
    
    
    CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)));
    
    
    sinv = ginv(cov(trdat));
    
    
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

