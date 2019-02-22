do_lineup <- function(data, col_offset) {
  real <- sample(1:9,1)
  for(i in 1:9) {
    if(i==real) {
      data <- cbind(data, data$value)
      colnames(data)[i+col_offset] <- paste0("p", i)
    }
    else {
      permutation <- sample(data$value,nrow(data))
      data <- cbind(data, permutation)
      colnames(data)[i+col_offset] <- paste0("p", i)
    }
  }
  return(data %>% select(-value) %>% mutate(real=paste0("p", real)))
}