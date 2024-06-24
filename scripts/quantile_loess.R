# The code was published on: http://www.r-statistics.com/2010/04/quantile-lowess-combining-a-moving-quantile-window-with-lowess-r-function
# Based on the algorithm published here: http://www.e-publications.org/ims/submission/index.php/AOAS/user/submissionFile/4295?confirm=37ca4b72
# http://en.wikipedia.org/wiki/Moving_average

# install.packages("zoo")
library(zoo) # rollmean
#?rollapply

quantile_loess	 <- function(Y, X = NULL, 
                            number.of.splits = NULL,
                            window.size = 20,
                            percent.of.overlap.between.two.windows = NULL,
                            the.distance.between.each.window = NULL,
                            the.quant = .95,
                            window.alignment = c("center"), 
                            window.function = function(x) {quantile(x, the.quant)},
                            ...)
{
  # input: Y and X, and smothing parameters
  # output: new y and x
  
  # Extra parameter "..." goes to the loess	
  
  # window.size ==  the number of observation in the window (not the window length!)
  
  # "number.of.splits" will override "window.size"
  # let's compute the window.size:	
  if(!is.null(number.of.splits)) {window.size <- ceiling(length(Y)/number.of.splits)}
  
  # If the.distance.between.each.window is not specified, let's make the distances fully distinct
  if(is.null(the.distance.between.each.window)) {the.distance.between.each.window <- window.size}
  
  # If percent.of.overlap.between.windows is not null, it will override the.distance.between.each.window 
  if(!is.null(percent.of.overlap.between.two.windows)) 
  {
    the.distance.between.each.window <- window.size * (1-percent.of.overlap.between.two.windows)
  }
  
  
  
  # loading zoo
  if(!require(zoo)) 	
  {
    print("zoo is not installed - please install it.")
    install.packages("zoo")
  }
  
  
  
  if(is.null(X)) {X <- index(Y)} # if we don't have any X, then Y must be ordered, in which case, we can use the indexes of Y as X.
  
  # creating our new X and Y
  zoo.Y <- zoo(x = Y, order.by = X)
  #zoo.X <- attributes(zoo.Y)$index
  
  # left align 
  new.Y <- rollapply(zoo.Y, width = window.size, 
                     FUN = window.function,
                     by = the.distance.between.each.window,
                     align = "center" 
                    # , fill = c("extend", "extend", "extend")
  )
  new.X <- attributes(new.Y)$index
  new.Y.mod <- loess(new.Y~new.X, family = "symmetric", ...)
  new.Y.loess <- new.Y.mod$fitted
  
  # newdata.loess.left <- data.frame(new.X.left = seq(min(new.X.left), max(new.X.left), 0.005)) # test this 
  # new.Y.loess.left <- predict(new.Y.mod.left, newdata.loess.left)
  
  # right align 
  # new.Y.right <- rollapply(zoo.Y, width = window.size, 
  #                         FUN = window.function,
  #                         by = the.distance.between.each.window,
  #                         align = "right")
  # new.X.right <- attributes(new.Y.right)$index
  # new.Y.mod.right <- loess(new.Y.right~new.X.right, family = "sym", ...)
  # 
  # new.Y.loess.right <- new.Y.mod.right$fitted
  
  # newdata.loess.right <- data.frame(new.X.right = seq(min(new.X.right), max(new.X.right), 0.005)) # test this 
  # new.Y.loess.right <- predict(new.Y.mod.right, newdata.loess.right)
  
  # return(list(
  #   x = c(new.X.left, new.X.right), 
  #   y.loess = c(new.Y.loess.left, new.Y.loess.right))
  # )
  
  return(list(
    x = new.X, 
    y.loess = new.Y.loess
  ))
}









