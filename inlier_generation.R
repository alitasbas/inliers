setwd("D:/ODTU/Sem 7/STAT 499/R")
library(moments)

source("utils.R")

set.seed(499)


# Normal Sample

norm_sample <- rnorm(100)
# Nidali_sample <- inject_inlier(norm_sample, 0.5, 0.75, 0.1, min=-0.375, max=0.375)
new_sample <- inject_inlier(original_data=norm_sample, 0.1)
akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.01)

hist(norm_sample)
# hist(norm_sample)
hist(new_sample)
hist(akkaya_sample)
# compare_dists(norm_sample, Nidali_sample)
compare_dists(norm_sample, new_sample)
compare_dists(norm_sample, akkaya_sample)
sapply(list(norm_sample, new_sample, akkaya_sample), kurtosis)
kurtosis(norm_sample)
kurtosis(new_sample)
kurtosis(akkaya_sample)

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


