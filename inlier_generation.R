setwd("D:/ODTU/Sem 7/STAT 499/R")
library(moments)

source("utils.R")

set.seed(499)

N <- 1000
norm_kurtosis <- numeric(N)
norm_avg_cluster_size <- numeric(N)
norm_avg_cluster_count <- numeric(N)
norm_summ_list <- vector("list", N)

nidali_kurtosis <- numeric(N)
nidali_avg_cluster_size <- numeric(N)
nidali_avg_cluster_count <- numeric(N)
nidali_summ_list <- vector("list", N)

akkaya_kurtosis <- numeric(N)
akkaya_avg_cluster_size <- numeric(N)
akkaya_avg_cluster_count <- numeric(N)
akkaya_summ_list <- vector("list", N)

# nidali_summary_df <- data.frame(q1 = c(), median = c(), 
#                          xbar = c(), q3 = c(),
#                          kurtosis = c())
# nidali_kurtosis <- c()
# nidali_avg_cluster_size = c()
# nidali_avg_cluster_count = c()
# 
# 
# norm_summary_df <- data.frame(q1 = c(), median = c(), 
#                          xbar = c(), q3 = c(),
#                          kurtosis = c())
# norm_kurtosis <- c()
# norm_avg_cluster_size = c()
# norm_avg_cluster_count = c()
# 
# 
# akkaya_summary_df <- data.frame(q1 = c(), median = c(), 
#                          xbar = c(), q3 = c(),
#                          kurtosis = c())
# akkaya_kurtosis <- c()
# akkaya_avg_cluster_size = c()
# akkaya_avg_cluster_count = c()


for (i in 1:N) {
  
  # --- Generation ---
  norm_sample <- rnorm(100)
  nidali_sample <- inject_inlier(original_data = norm_sample, 0.1)
  akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.01)
  
  # --- Normal Processing ---
  # Calculate expensive operations (clusters) ONLY ONCE per iteration
  norm_clust_res <- clusters(norm_sample) 
  
  norm_summ_list[[i]] <- summary(norm_sample)
  norm_kurtosis[i] <- kurtosis(norm_sample)
  norm_avg_cluster_size[i] <- mean(norm_clust_res$cluster_size)
  norm_avg_cluster_count[i] <- nrow(norm_clust_res)
  
  # --- Nidali Processing ---
  nidali_clust_res <- clusters(nidali_sample)
  
  nidali_summ_list[[i]] <- summary(nidali_sample)
  nidali_kurtosis[i] <- kurtosis(nidali_sample)
  nidali_avg_cluster_size[i] <- mean(nidali_clust_res$cluster_size)
  nidali_avg_cluster_count[i] <- nrow(nidali_clust_res)
  
  # --- Akkaya Processing ---
  # (Fixed copy-paste errors from original code here)
  akkaya_clust_res <- clusters(akkaya_sample)
  
  akkaya_summ_list[[i]] <- summary(akkaya_sample)
  akkaya_kurtosis[i] <- kurtosis(akkaya_sample)
  akkaya_avg_cluster_size[i] <- mean(akkaya_clust_res$cluster_size)
  akkaya_avg_cluster_count[i] <- nrow(akkaya_clust_res)
}

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

# Exp Sample

exp_sample <- rexp(1000, rate=1/30)
Nidali_exp_sample <- inject_inlier(exp_sample, 30, 1, 0.1, min=28, max=31.5)
akkaya_exp_sample <- generate_contaminated_data(exp_sample, delta=0.8)

hist(exp_sample)
hist(exp_sample)
hist(Nidali_exp_sample)
hist(akkaya_exp_sample)
compare_dists(exp_sample, Nidali_exp_sample)
compare_dists(exp_sample, akkaya_exp_sample)
sapply(list(exp_sample, Nidali_exp_sample, akkaya_exp_sample), kurtosis)

############## NIDA'S SAMPLE

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


