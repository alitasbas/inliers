source("utils.R")

set.seed(499)


# Normal Sample

norm_sample <- rnorm(100)
Nidali_sample <- inject_inlier(norm_sample, 0.5, 0.75, 0.1, min=-0.375, max=0.375)
akkaya_sample <- generate_contaminated_data(norm_sample, delta = 0.75)

hist(norm_sample)
hist(norm_sample)
hist(Nidali_sample)
hist(akkaya_sample)
compare_dists(norm_sample, Nidali_sample)
compare_dists(norm_sample, akkaya_sample)


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

############## NIDA'S SAMPLE

exam_score_df <- read.csv("data/student_exam_scores.csv")
scores <- exam_score_df$exam_score

hist(scores)
# shapiro.test(exam_score_df$exam_score) # Normal

nidali_contaminated_scores <- inject_inlier(scores, mu=20, sigma = 4, ratio = 0.07, min = 15, max = 25)
akkaya_contaminated_scores <- generate_contaminated_data(scores, delta = 0.75)

plot(density(scores))
plot(density(nidali_contaminated_scores))
plot(density(akkaya_contaminated_scores))
compare_dists(scores, nidali_contaminated_scores)
compare_dists(scores, akkaya_contaminated_scores)

