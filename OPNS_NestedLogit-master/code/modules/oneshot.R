oneshot <- function(n_row, n_col, n_depth, n_samples){
  
  # Sampling with replacement of Gumbels eta and epsilon
  temp = rgev(n = (n_row*n_row + n_col)*n_depth,xi = 0,mu = 0,beta = 1)
  samples <- replicate(n_samples, sample(temp, (n_row*n_row + n_col)*n_depth, replace = TRUE))
  
  dim(samples)= (n_row*n_row + n_col)*n_depth*n_samples
  
  #samples = rgev(n = (n_row*n_row + n_col)*n_depth*n_samples,xi = 0,mu = 0,beta = 1)
  
  len1 = n_row*n_col*n_depth*n_samples
  sample1 = samples[1:len1]
  sample2 = samples[(len1+1):length(samples)]
  
  #Converting parameters into suitable dimesion: (n_row X n_col X n_depth X n_sample)
  value =  array(values$val, dim=c(n_row,n_col,n_depth,n_samples))
  value = aperm(value, c(2,1,3,4))
  lambda = array(values$lambda, dim=c(n_row,n_col,n_depth,n_samples))
  epsilon = array(sample1, dim=c(n_row,n_col,n_depth,n_samples))
  eta = aperm(array((rep(sample2,each=n_col)), dim=c(n_row,n_col,n_depth,n_samples)), c(2,1,3,4))
  
  # Utility compute
  utility = value + lambda*epsilon + eta
  b = (apply(utility,c(3,4),function(utility) which(utility == max(utility), arr.ind = TRUE)))
  choice = as.data.frame(t(as.data.frame(b)))
  colnames(choice)<-c("nest", "item")
  
  #Unique mapping of 9 choice probabilities
  choice$mapping = choice$nest*10 + choice$item
  choice$sample_id = rep(seq.int(n_samples), each=n_depth)
  x = as.data.frame(choice %>% group_by(sample_id, nest, item) %>% summarize(count=n()))
  
  #Computing choice probability
  x$prob = x$count/n_depth

  sample_id = rep(seq.int(n_samples), each = n_row*n_col)
  nest  = rep(rep(seq.int(1,n_row), each = n_col), times = n_samples)
  item = rep(seq.int(n_col), n_row*n_samples)
  choice_set = data.frame(sample_id, nest, item)
  choice_set = merge(choice_set, x, all.x=TRUE)
  choice_set[is.na(choice_set)]<- 0
  
  # Final choice probabilities and covariance matrix
  choice_prob = as.data.frame(do.call(cbind, tapply(choice_set$prob, choice_set$sample_id , I)))
  Covariance = cov(t(choice_prob))
  
  #Saving these to RDS files
  saveRDS(Covariance, file = "../variables/one_shot_covariance.rds")
  saveRDS(choice_prob, file = "../variables/one_shot_prob.rds")
}