library(dplyr)

set.seed(238)

inlier_gen <- function(alpha, beta, n=1000, min, max) {
  all_sample <- rnorm(n*100, alpha, beta)
  
  index <- ifelse(between(all_sample, alpha - beta, alpha + beta) & between(all_sample, min, max), TRUE, FALSE)
  
  real_sample <- sample(all_sample[index], n) # replace = FALSE or TRUE?
  
  return(real_sample)
}

inlier_gen_akkaya <- function(sample, delta = 0.8, ratio) {
  n <- round(length(sample) * ratio, 0)
  xbar <- mean(sample)
  sigma <- sd(sample)
  sample <- sort(sample)
  index <- 1
  
  for (i in 1:n) {
    if(i %% 2 == 1) {
      index <- i %/% 2 + 1
      
    } else {
      index <- length(sample) - (i/2) + 1
    }
    
    sample[index] <- xbar + (-delta)^i * sigma # ^index or i
  }
  
  return(sample)
}

compare_dists <- function(main_sample, inliers, ratio=0.1) {
  n <- length(main_sample)
  u <- as.integer(runif(n) > ratio)
  
  print(sum(u))
  contaminated_sample <- u * main_sample + (1 - u) * inliers # after this step we lose trace of inlier points
  
  main_dens <- density(main_sample)
  inlier_dens <- density(inliers)
  
  max_y <- max(main_dens$y, inlier_dens$y)
  
  par(mfrow=c(1, 2))
  
  plot(main_dens, col = "blue", lwd = 2,
       ylim = c(0, max_y))
  
  lines(inlier_dens, col = "red", lwd = 2)
  # legend("topright", legend = c("normal", "inlier"), col = c("blue", "red"), lwd = 2)
  
  plot(density(c(contaminated_sample)), col="red", lwd=2)
  lines(density(main_sample), col = "blue", lwd = 2)
  # legend("topright", legend = c("clean", "contaminated"), col = c("blue", "red"), lwd = 2)
}


# Normal Sample

norm_sample <- rnorm(1000, 21, 2.5)
inliers <- inlier_gen(21, 1.25, 1000, min=20, max=22)
norm_akkaya_sample <- inlier_gen_akkaya(norm_sample, delta=0.8, ratio = 0.1)

hist(inliers)
hist(norm_sample)
compare_dists(norm_sample, inliers, ratio=1/10)


# Exp Sample

exp_sample <- rexp(1000, rate=1/30)
inliers <- inlier_gen(30, 1, 1000, min=28, max=31)
exp_akkaya_sample <- inlier_gen_akkaya(norm_sample, delta=0.8, ratio = 0.1)

hist(inliers)
hist(exp_sample)
compare_dists(exp_sample, inliers, ratio=1/15)




