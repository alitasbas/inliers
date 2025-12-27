setwd("D:/ODTU/Sem 7/STAT 499/R")
library(moments)

source("utils.R")

set.seed(499)

N <- 1000
norm_kurtosis <- numeric(N)
norm_avg_cluster_size <- numeric(N)
norm_avg_cluster_count <- numeric(N)
norm_avg_cluster_length <- numeric(N)
norm_summ_list <- vector("list", N)

nidali_kurtosis <- numeric(N)
nidali_avg_cluster_size <- numeric(N)
nidali_avg_cluster_count <- numeric(N)
nidali_avg_cluster_length <- numeric(N)
nidali_summ_list <- vector("list", N)

akkaya_kurtosis <- numeric(N)
akkaya_avg_cluster_size <- numeric(N)
akkaya_avg_cluster_count <- numeric(N)
akkaya_avg_cluster_length <- numeric(N)
akkaya_summ_list <- vector("list", N)

for (i in 1:N) {
  
  # --- Generation ---
  norm_sample <- rnorm(250)
  nidali_sample <- inject_inlier(original_data = norm_sample, 0.1)
  akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.01)
  
  # --- Normal Processing ---
  norm_clust_res <- clusters(norm_sample) 
  
  norm_summ_list[[i]] <- summary(norm_sample)
  norm_kurtosis[i] <- kurtosis(norm_sample)
  norm_avg_cluster_size[i] <- mean(norm_clust_res$cluster_size)
  norm_avg_cluster_count[i] <- nrow(norm_clust_res)
  norm_avg_cluster_length[i] <- mean(norm_clust_res$cluster_length)
  
  # --- Nidali Processing ---
  nidali_clust_res <- clusters(nidali_sample)
  
  nidali_summ_list[[i]] <- summary(nidali_sample)
  nidali_kurtosis[i] <- kurtosis(nidali_sample)
  nidali_avg_cluster_size[i] <- mean(nidali_clust_res$cluster_size)
  nidali_avg_cluster_count[i] <- nrow(nidali_clust_res)
  nidali_avg_cluster_length[i] <- mean(nidali_clust_res$cluster_length)
  
  # --- Akkaya Processing ---
  akkaya_clust_res <- clusters(akkaya_sample)
  
  akkaya_summ_list[[i]] <- summary(akkaya_sample)
  akkaya_kurtosis[i] <- kurtosis(akkaya_sample)
  akkaya_avg_cluster_size[i] <- mean(akkaya_clust_res$cluster_size)
  akkaya_avg_cluster_count[i] <- nrow(akkaya_clust_res)
  akkaya_avg_cluster_length[i] <- mean(akkaya_clust_res$cluster_length)
}

norm_summary_df <- bind_rows(norm_summ_list)
nidali_summary_df <- bind_rows(nidali_summ_list)
akkaya_summary_df <- bind_rows(akkaya_summ_list)

plot_and_quant(norm_avg_cluster_size, nidali_avg_cluster_size)
################# END ###########################


norm_sample <- rnorm(100)
nidali_sample <- inject_inlier(original_data=norm_sample, 0.1)
akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.01)

hist(norm_sample, col="lightblue", freq=F)
lines(density(norm_sample), col="darkmagenta", lwd=3)


hist(nidali_sample, col="lightblue", freq=F)
lines(density(norm_sample), col="darkmagenta", lwd=3)

hist(akkaya_sample, col="lightblue", freq=F)
lines(density(norm_sample), col="darkmagenta", lwd=3)

compare_dists(norm_sample, nidali_sample)
compare_dists(norm_sample, akkaya_sample)

# Kurtosis
sapply(list(norm_sample, nidali_sample, akkaya_sample), kurtosis)

# Clusters
c_norm <- clusters(norm_sample)
c_new <- clusters(nidali_sample)
c_akkaya <- clusters(akkaya_sample)

############## ------EXPONENTIAL DISTRIBUTION--------- ########################
# Don't mind the variable names. I got lazy to change them

set.seed(499)

N <- 1000
norm_kurtosis <- numeric(N)
norm_avg_cluster_size <- numeric(N)
norm_avg_cluster_count <- numeric(N)
norm_avg_cluster_length <- numeric(N)
norm_summ_list <- vector("list", N)

nidali_kurtosis <- numeric(N)
nidali_avg_cluster_size <- numeric(N)
nidali_avg_cluster_count <- numeric(N)
nidali_avg_cluster_length <- numeric(N)
nidali_summ_list <- vector("list", N)

akkaya_kurtosis <- numeric(N)
akkaya_avg_cluster_size <- numeric(N)
akkaya_avg_cluster_count <- numeric(N)
akkaya_avg_cluster_length <- numeric(N)
akkaya_summ_list <- vector("list", N)

for (i in 1:N) {
  
  # --- Generation ---
  norm_sample <- rexp(250)
  nidali_sample <- inject_inlier(original_data = norm_sample, 0.1)
  akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.01)
  
  # --- Normal Processing ---
  norm_clust_res <- clusters(norm_sample) 
  
  norm_summ_list[[i]] <- summary(norm_sample)
  norm_kurtosis[i] <- kurtosis(norm_sample)
  norm_avg_cluster_size[i] <- mean(norm_clust_res$cluster_size)
  norm_avg_cluster_count[i] <- nrow(norm_clust_res)
  norm_avg_cluster_length[i] <- mean(norm_clust_res$cluster_length)
  
  # --- Nidali Processing ---
  nidali_clust_res <- clusters(nidali_sample)
  
  nidali_summ_list[[i]] <- summary(nidali_sample)
  nidali_kurtosis[i] <- kurtosis(nidali_sample)
  nidali_avg_cluster_size[i] <- mean(nidali_clust_res$cluster_size)
  nidali_avg_cluster_count[i] <- nrow(nidali_clust_res)
  nidali_avg_cluster_length[i] <- mean(nidali_clust_res$cluster_length)
  
  # --- Akkaya Processing ---
  akkaya_clust_res <- clusters(akkaya_sample)
  
  akkaya_summ_list[[i]] <- summary(akkaya_sample)
  akkaya_kurtosis[i] <- kurtosis(akkaya_sample)
  akkaya_avg_cluster_size[i] <- mean(akkaya_clust_res$cluster_size)
  akkaya_avg_cluster_count[i] <- nrow(akkaya_clust_res)
  akkaya_avg_cluster_length[i] <- mean(akkaya_clust_res$cluster_length)
}

norm_summary_df <- bind_rows(norm_summ_list)
nidali_summary_df <- bind_rows(nidali_summ_list)
akkaya_summary_df <- bind_rows(akkaya_summ_list)

plot_and_quant(norm_avg_cluster_size, nidali_avg_cluster_size)
plot_and_quant(norm_avg_cluster_size, akkaya_avg_cluster_size)
################## THE END #################


exp_sample <- rexp(100, rate=1/30)
nidali_exp_sample <- inject_inlier(exp_sample, 0.1)
akkaya_exp_sample <- generate_contaminated_data(exp_sample, delta=0.8)

hist(exp_sample)
hist(exp_sample)
hist(nidali_exp_sample)
hist(akkaya_exp_sample)
compare_dists(exp_sample, nidali_exp_sample)
compare_dists(exp_sample, akkaya_exp_sample)
sapply(list(exp_sample, nidali_exp_sample, akkaya_exp_sample), kurtosis)




############## NIDA'S SAMPLE ##################

exam_score_df <- read.csv("data/student_exam_scores.csv")
scores <- exam_score_df$exam_score

hist(scores)
# shapiro.test(exam_score_df$exam_score) # Normal

nidali_contaminated_scores <- inject_inlier(scores, mu=34, sigma = 4, ratio = 0.07, min = 30, max = 39)
akkaya_contaminated_scores <- generate_contaminated_data(scores, delta = 0.75)

plot(density(scores))
plot(density(nidali_contaminated_scores))
plot(density(akkaya_contaminated_scores))
compare_dists(scores, nidali_contaminated_scores)
compare_dists(scores, akkaya_contaminated_scores)

sapply(list(scores, nidali_contaminated_scores, akkaya_contaminated_scores), kurtosis)
qqnorm(scores)
qqplot(scores, nidali_contaminated_scores)
qqplot(scores, akkaya_contaminated_scores)


