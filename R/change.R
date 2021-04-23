# Positive change function
# INPUT: x: a vector of <num> 
#        direction: what numbers to aggregate (default: positive)
#        .fun : function to execute, defaults to mean
# OUTPUT: An aggregated number that represents the aggregated change
# EXAMPLE: 
# df1 <- tibble(x = c(10, 11, 9, 8, 16, 7, 8, 9, 10, 5))
# summarise(df1, pc = change(x), nc = change(x, "negative"))
    
change <- function(x, direction = c("positive","negative"), .fun = mean){
    
    difference <- diff(x)
    if (direction[1] == "positive") difference[difference < 0] = 0
    if (direction[1] == "negative") difference[difference > 0] = 0
    .fun(difference, na.rm = TRUE)
    
}
