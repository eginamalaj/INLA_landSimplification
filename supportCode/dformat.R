###
###
### Function to gather and spread at the same time using tidyverse
###
### Source: web (cannot find the link anymore)
#
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
#
# Code that removes polygons from shapefile that are not present in the table
#
join.sp.df <- function(x, y, xcol, ycol) {
  x$sort_id <- 1:nrow(as(x, "data.frame"))  
  x.dat <- as(x, "data.frame")  
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  
  x2.dat <- as(x2, "data.frame") 
  row.names(x.dat2.ord) <- row.names(x2.dat)  
  x2@data <- x.dat2.ord  
  return(x2)
}
#