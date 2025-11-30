library(dplyr)
library(ggplot2)

# NidAli Inlier Generation

inlier_gen <- function(mu, sigma, n=1000, min=-Inf, max=Inf) {
  inlier_sample <- rnorm(n*100, mu, sigma)
  
  index <- ifelse(between(inlier_sample, mu - sigma, mu + sigma) & between(inlier_sample, min, max), TRUE, FALSE)
  
  inlier_sample <- sample(inlier_sample[index], n) # replace = FALSE or TRUE?
  
  return(inlier_sample)
}

inject_inlier <- function(original_data, mu, sigma, ratio=0.1, min=-Inf, max=Inf) {
  n <- length(original_data)
  inliers <- inlier_gen(mu, sigma, n, min, max)
  
  u <- as.integer(runif(n) > ratio)
  contaminated_sample <- u * original_data + (1 - u) * inliers # after this step we lose trace of inlier points
  
  return(contaminated_sample)
}



# Akkaya Inlier Generation
"
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
"

generate_contaminated_data <- function(x, delta, ratio=0.1) {
  # 1. SVRGN: Sort the data
  # In your code, X1 is sorted and copied to X.
  # So effectively, X becomes the sorted array.
  x <- sort(x)
  
  # 3. Calculate Mean (Loop 566)
  emean <- mean(x)
  
  # 4. Calculate Bounds (n2)
  # int(0.5 + 0.1*n) is the manual way to round to the nearest integer
  n <- length(x)
  n2 <- round( (ratio / 2) * n) # was 20%
  
  # Initialize X1 to match X initially
  x1 <- x
  
  # 5. Modify the Tails (Loop 50)
  # This replaces the top and bottom 10% with a deterministic value
  for (i in 1:n2) {
    j <- n - i + 1
    
    # (-0.01)**i
    term_i <- (-delta)^i
    term_j <- (-delta)^j
    
    x1[i] <- emean + term_i
    x1[j] <- emean + term_j
  }
  
  # Loop 51 (Copying middle) is redundant here because we
  # initialized x1 <- x, but logically, the middle 80% remains x.
  
  return(x1)
}



# Detection by Clustering Method

clusters <- function(sample) {
  n <- length(sample)
  sample <- sort(sample)
  diffs <- diff(sample)
  
  
  # Find the ideal diff length
  round_index <- 5
  for (i in round_index:-5) {
    if (between(as.matrix(table(round(diffs, i)))[1], 0.08 * n, 0.12 * n)) {
      
      round_index <- i
      print(round_index)
      break
    }
    
  }
  diffs_table <- as.matrix(table(round(diffs, round_index)))
  diffs_table <- as.data.frame(diffs_table)
  diffs_table$difference <- as.double(rownames(diffs_table))
  colnames(diffs_table) <- c("frequency", "difference")
  
  threshold <- diffs_table[3, 2]
  binary_diffs <- c(as.integer(diffs <= threshold), 0, 0) # 1 if the distance < threshold
  
  # Find the number of connected data points
  clusters_df <- data.frame(lower_boundary = numeric(0),
                            upper_boundary = numeric(0),
                            cluster_size = numeric(0))
  c_size <- 0
  next_point_index <- 0
  for (point_index in 1:(n-1)) {
    cat("checking index", point_index, "\n")
    if (point_index <= next_point_index) {
      next
    }
    
    if (binary_diffs[point_index] == 1) {
      
      for (next_point_index in (point_index + 1): (n-1)) {
        cat("Matched. Now checking trailing indexes", next_point_index, "\n")
        
        print(binary_diffs[next_point_index:(next_point_index + 2)])
        print(sum(binary_diffs[next_point_index:(next_point_index + 2)]))
        if (sum(binary_diffs[next_point_index:(next_point_index + 2)]) == 0) {
          
          cluster_boundaries <- c(point_index, (next_point_index - 1))
          cat("cluster_boundaries: ", cluster_boundaries, "\n")
          c_size <- sum(binary_diffs[cluster_boundaries[1]: cluster_boundaries[2]])
          
          if (c_size < 3) {
            cat("skipped at", point_index, "\n")
            
          }
          else {
            print("Adding to dataframe")
            clusters_df <- add_row(clusters_df, 
                                   lower_boundary=cluster_boundaries[1],
                                   upper_boundary=cluster_boundaries[2],
                                   cluster_size = c_size)
          }
          break
        }
        
      } # end of for(next_point_index) loop 
    }
    
  }
  return(clusters_df)
}



# Comparing distribution of 2 data sets

compare_dists <- function(before_injection, after_injection) {
  
  before_dens <- density(before_injection)
  after_dens <- density(after_injection)
  
  max_y <- max(before_dens$y, after_dens$y)
  
  plot(before_dens, col = "blue", lwd = 2,
       ylim = c(0, max_y),
       main="Comparison of Distributions",
       xlab="Value", ylab="Density")
  
  lines(after_dens, col = "red", lwd = 2)
  
  legend("topright", legend = c("before", "after"), col = c("blue", "red"), lwd = 2)
}
