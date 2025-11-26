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

x <- clusters(normal_sample)

x
