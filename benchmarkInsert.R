library(data.table)
library(microbenchmark)

insertNormal <- function(){
  df <- data.frame(a = rep(0, 10000),
                   b = rep(0, 10000),
                   stringsAsFactors = FALSE)
  for (i in 1:nrow(df)){
    df[i,] <- c(999, 999)
  }
  return(df)
}

insertDt <- function(){
  dt <- data.table(a = rep(0, 314000),
                   b = rep(0, 314000))
  for (i in 1:nrow(dt)){
    set(dt, i, "a", 999)
    set(dt, i, "b", 999)
  }
  return(dt)
}

m <- microbenchmark(insertNormal(), insertDt(), times = 10)
test_df <- insertNormal()
test_dt <- insertDt()
