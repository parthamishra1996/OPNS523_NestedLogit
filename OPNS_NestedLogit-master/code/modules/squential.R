squential <- function(n_row, n_col, n_depth, n_samples){
  
  # Sampling with replacement of Gumbels eta
  temp = rgev(n = (n_row)*n_depth,xi = 0,mu = 0,beta = 1)
  samples <- replicate(n_samples, sample(temp, n_row*n_depth, replace = TRUE))
  
  dim(samples)= (n_row)*n_depth*n_samples
  #Converting parameters into suitable dimesion: (n_row X n_col X n_depth X n_sample)
  
  eta = array(samples, dim=c(n_row, n_depth, n_samples))
  
  value =  array(values$val, dim=c(n_row,n_col,n_depth,n_samples))
  lambda = array(values$lambda, dim=c(n_row,n_col,n_depth,n_samples))
  
  LSE = log(colSums(aperm(exp(value/lambda), c(2,1,3,4))))
  lambda2 = array(values$lambda, dim=c(n_col,n_depth,n_samples))
                  
  #Compute nested utility objective(after eta is resolved)
  obj = lambda2*LSE + eta
  
  b =(apply(obj,c(2,3),function(obj) which(obj == max(obj), arr.ind = TRUE)))
  
  flag1 = b
  dim(flag1) = c(n_depth*n_samples,1)
  dim(b) = c(1,n_depth,n_samples)
  index = b
  
  # generate epsilon samples
  
  temp = rgev(n = (n_row)*n_depth,xi = 0,mu = 0,beta = 1)
  epsilon2 <- replicate(n_samples, sample(temp, n_row*n_depth, replace = TRUE))
  
  dim(epsilon2) = c(n_row,n_depth,n_samples)
  
  
  #epsilon2 = rgev(n = (n_row)*n_depth*n_samples,xi = 0,mu = 0,beta = 1)
  
  # genrate correspoinding V_{ik}
  value2 =  sample(0, (n_row)*n_depth*n_samples,replace = TRUE)
  dim(value2) = c(n_row,n_depth,n_samples)
  value2[1,,] = values$val[index] 
  value2[2,,] = values$val[index+3]
  value2[3,,] = values$val[index+6]
  
  # genrate correspoinding lambda_{i}
  lambda2 =  sample(0, (n_row)*n_depth*n_samples,replace = TRUE)
  dim(lambda2) = c(n_row,n_depth,n_samples)
  lambda2[1,,] = values$lambda[index] 
  lambda2[2,,] = values$lambda[index+3]
  lambda2[3,,] = values$lambda[index+6]
  
  utility2 = value2/lambda2 + epsilon2
  
  
  flag2 =(apply(utility2,c(2,3),function(utility2) which(utility2 == max(utility2), arr.ind = TRUE)))
  
  dim(flag2) = c(n_depth*n_samples,1)
  flag2 = as.data.frame(flag2)
  flag1 = as.data.frame(flag1)
  choice_seq= data.frame(flag1, flag2)
  
  colnames(choice_seq)<-c("nest", "item")
  choice_seq$mapping = choice_seq$nest*10 + choice_seq$item
  choice_seq$sample_id = rep(seq.int(n_samples), each=n_depth)
  
  x = as.data.frame(choice_seq %>% group_by(sample_id, nest, item) %>% summarize(count=n()))
  x$prob = x$count/n_depth
  
  sample_id = rep(seq.int(n_samples), each = n_row*n_col)
  nest  = rep(rep(seq.int(1,n_row), each = n_col), times = n_samples)
  item = rep(seq.int(n_col), n_row*n_samples)
  
  choice_set = data.frame(sample_id, nest, item)
  choice_set = merge(choice_set, x, all.x=TRUE)
  choice_set[is.na(choice_set)]<- 0
  
  # Final choice probability and covariance matrix
  choice_prob_seq = as.data.frame(do.call(cbind, tapply(choice_set$prob, choice_set$sample_id , I)))
  Covariance_seq = cov(t(choice_prob_seq))
  
  #Saving in RDS file
  saveRDS(Covariance_seq, file = "../variables/squential_covariance.rds")
  saveRDS(choice_prob_seq, file = "../variables/squential_prob.rds")
}

