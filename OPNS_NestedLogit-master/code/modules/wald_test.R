wald_test <- function(n_row, n_col, n_depth, n_samples){
  
  # Loading RDS values of probability and variance estimates for both cases
  prob_oneshot <- readRDS("../variables/one_shot_prob.rds")
  cov_oneshot <- readRDS("../variables/one_shot_covariance.rds") 
  prob_seq <- readRDS("../variables/squential_prob.Rds")
  cov_seq <- readRDS("../variables/squential_covariance.Rds")
    
  data = values
  data$t1 = exp(data$val/data$lambda)
  
  # Computing point estimates for mean and reducing a degree of freedom(dropping th last prob)
  prob_est_seq = rowMeans(prob_seq)
  prob_est_seq = prob_est_seq[-length(prob_est_seq)]
  cov_est_seq = cov_seq
  cov_est_seq = cov_est_seq[-length(prob_est_seq), -length(prob_est_seq)]
  
  prob_est_oneshot = rowMeans(prob_oneshot)
  prob_est_oneshot = prob_est_oneshot[-length(prob_est_oneshot)]
  cov_est_oneshot = cov_oneshot
  cov_est_oneshot = cov_est_oneshot[-length(prob_est_oneshot), -length(prob_est_oneshot)]
  
  # Theoritical probabilities are computed in excel file at path: "../variables/theoritical_probability"
  prob_theoritical = (c(0.363099998, 0.002107598,  0.043964278, 0.030077712, 0.17967068, 0.115676774, 0.030114138, 0.060764031))
  
  seq_out = wald.test(Sigma = cov_est_seq, b = prob_est_seq, Terms = 1:length(prob_est_seq) , H0 = prob_theoritical)
  oneshot_out = wald.test(Sigma = cov_est_oneshot, prob_est_oneshot, Terms = 1:length(prob_est_oneshot), H0 = prob_theoritical)
  
  # Displaying final output of both the tests
  print(seq_out)
  print(oneshot_out)
}