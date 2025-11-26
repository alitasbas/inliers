library(dplyr)

normal_sample <- rnorm(100)

x <- table(round(diff(sort(normal_sample)), 2))

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
  binary_diffs <- as.integer(diffs <= threshold) # 1 if the distance < threshold
  
  # Find the number of connected data points
  clusters_df <- data.frame(lower_boundary = numeric(0),
                            upper_boundary = numeric(0),
                            cluster_size = numeric(0))
  cluster_size <- 0
  for (point_index in 1:length(binary_diffs)) {
    
    if (binary_diffs[point_index] == 1) {
      
      for (next_point_index in (point_index + 1): length(binary_diffs)) {
        
        if (sum(binary_diffs[next_point_index:next_point_index + 2]) == 0) {
          
          cluster_boundaries <- c(point_index, next_point_index - 1)
          c_size <- sum(binary_diffs[point_index: next_point_index])
          
          if (c_size < 3) {
            cat("skipped at", point_index)
            break
          }
          else {
            clusters_df <- add_row(clusters_df, 
                                   lower_boundary=point_index,
                                   upper_boundary=next_point_index - 1,
                                   cluster_size = c_size)
          }
          
          break
        }
      }
    }
    
  }
  return(clusters_df)
}

x <- clusters(normal_sample)

x
