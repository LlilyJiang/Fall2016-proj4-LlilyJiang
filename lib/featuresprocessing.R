
### define feature processing functions ###
feature_truncate_1d <- function(ls, len){
  if(length(ls) >= len){
    ls <- ls[1:len]
  }
  else{
    t <- ceiling(len/length(ls))
    ls <- rep(ls, t)
    ls <- ls[1:len]
  }
  return(ls)
}

feature_truncate_2d <- function(df, ncols){
  if(dim(df)[2] >= ncols){
    df <- df[,1:ncols]
  }
  else{
    t <- ceiling(ncols/dim(df)[2])
    df <- do.call("cbind", replicate(t, df, simplify = FALSE))
    df <- df[,1:ncols]
  }
  ls <- as.vector(t(df))
  return(ls)
}