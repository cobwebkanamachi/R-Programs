#https://github.com/cran/phaseR/blob/master/R/flowField.R
flowField <- function(deriv, x.lim, y.lim, parameters = NULL, points = 11, 
                      system = "two.dim", colour = "gray",
					  arrow.type = "equal", arrow.head = 0.05, frac = 1,
					  add = TRUE, xlab = "x", ylab = "y", ...){
  if ((is.vector(x.lim) == FALSE) | (length(x.lim) != 2)){
    stop("x.lim is not a vector of length 2 as required")
  }
  if (x.lim[2] <= x.lim[1]){
    stop("x.lim[2]  is less than or equal to x.lim[1]")
  }
  if ((is.vector(y.lim) == FALSE) | (length(y.lim) != 2)){
    stop("y.lim is not a vector of length 2 as required")
  }
  if (y.lim[2] <= y.lim[1]){
    stop("y.lim[2]  is less than or equal to y.lim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to one.dim or two.dim")
  }
  if (is.vector(colour) == FALSE){
    stop("colour is not a vector as required")
  }
  if (length(colour) > 1){
    colour <- colour[1]
    print("Note: colour has been reset as required")
  }
  if (!(arrow.type %in% c("proportional", "equal"))){
    stop("arrow.type must either be set to proportional or equal")
  }
  if (arrow.head <= 0){
    stop("arrow.head is less than or equal to zero")
  }
  if (frac <= 0){
    stop("frac is less than or equal to zero")
  }
  if (!is.logical(add)){
    stop("add must be logical")
  }
  x <- seq(from = x.lim[1], to = x.lim[2], length = points)
  y <- seq(from = y.lim[1], to = y.lim[2], length = points)
  dx <- matrix(0, ncol = points, nrow = points)
  dy <- matrix(0, ncol = points, nrow = points)
  x.max.length <- x[2] - x[1]
  y.max.length <- y[2] - y[1]
  if (add == FALSE){
    plot(1, xlim = c(x.lim[1] - x.max.length, x.lim[2] + x.max.length),
	     ylim = c(y.lim[1] - y.max.length, y.lim[2] + y.max.length),
		 type = "n", xlab = xlab, ylab = ylab, ...)
  }
  if (system == "two.dim"){
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        df <- deriv(t = 0, y = c(x[i], y[j]), parameters = parameters)
        dx[i, j] <- df[[1]][1]
        dy[i, j] <- df[[1]][2]
      }
    }
    abs.dx <- abs(dx)
    abs.dy <- abs(dy)
    abs.dx.non <- abs.dx[which((abs.dx != 0) & (abs.dy != 0))]
    abs.dy.non <- abs.dy[which((abs.dx != 0) & (abs.dy != 0))]
    max.length <- max(sqrt(dx^2 + dy^2))
    coefficient <- frac*min(x.max.length, y.max.length)/
	                 (2*sqrt(2)*max(sqrt(2*(abs.dy.non/abs.dx.non)/
		     		   ((abs.dy.non/abs.dx.non) +
					     (abs.dx.non/abs.dy.non))),
					       sqrt(2*(abs.dx.non/abs.dy.non)/
						     ((abs.dy.non/abs.dx.non) +
						       (abs.dx.non/abs.dy.non)))))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        if ((dx[i, j] != 0) | (dy[i, j] != 0)){
          if ((dx[i, j] != 0) & (dy[i, j] != 0)){
            factor <- sqrt(2/((abs.dy[i, j]/abs.dx[i, j]) +
			            (abs.dx[i, j]/abs.dy[i, j])))
            y.shift <- coefficient*factor*
	      	             sqrt(abs.dy[i, j]/abs.dx[i, j])
            x.shift <- coefficient*factor/
			             sqrt(abs.dy[i, j]/abs.dx[i, j])
            if (dy[i, j] < 0){
              y.shift <- -abs(y.shift)
            }
            if (dx[i, j] < 0){
              x.shift <- -abs(x.shift)
            }
          }
          if ((dx[i, j] == 0) & (dy[i, j] != 0)){
            y.shift <- coefficient*sqrt(2)
            x.shift <- 0
            if (dy[i, j] < 0){
              y.shift <- -abs(y.shift)
            }
          }
          if ((dx[i, j] != 0) & (dy[i, j] == 0)){
            y.shift <- 0
            x.shift <- coefficient*sqrt(2)
            if (dx[i, j] < 0){
              x.shift <- -abs(x.shift)
            }
          }
          if (arrow.type == "proportional"){
            prop <- sqrt((abs.dx[i, j]^2 + abs.dy[i, j]^2))/max.length
            y.shift <- y.shift*prop
            x.shift <- x.shift*prop
          }
          arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
	             y[j] + y.shift, length = arrow.head, 
                 col = colour, ...)
        }
      }
    }
  }
  if (system == "one.dim"){
    for (i in 1:length(y)){
      dy[1, i] <- deriv(t = 0, y = y[i], parameters = parameters)[[1]]
    }
    for (i in 2:length(x)){
      dy[i, ] <- dy[1, ]
    }
    abs.dy <- abs(dy)
    abs.dy.non <- abs.dy[which(abs.dy != 0)]
    max.abs.dy <- max(abs(dy))
    coefficient <- frac*min(x.max.length, y.max.length)/
                     (2*sqrt(2)*max(sqrt(2*abs.dy.non/
	    			   (abs.dy.non + (1/abs.dy.non))),
					     sqrt(2*(1/abs.dy.non)/
						   (abs.dy.non + (1/abs.dy.non)))))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        if (dy[i, j] != 0){
          factor <- sqrt(2/(abs.dy[i, j] + (1/abs.dy[i, j])))
          y.shift <- coefficient*factor*sqrt(abs.dy[i, j])
          x.shift <- coefficient*factor/sqrt(abs.dy[i, j])
          if (dy[i, j] < 0){
            y.shift <- -y.shift
          }
        }
        if (dy[i, j] == 0){
          y.shift <- 0
          x.shift <- coefficient*sqrt(2)
        }
        if (arrow.type == "proportional"){
          if (dy[i, j] != 0){
            prop <- abs.dy[i, j]/max.abs.dy
            y.shift <- y.shift*prop
            x.shift <- x.shift*prop
          }
          if (dy[i, j] == 0) {
            x.shift <- y.shift*mean(abs.dy)/max.abs.dy
          }
        }
        arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
		       y[j] + y.shift, length = arrow.head, 
               col = colour, ...)
      }
    }
  }
  output            <- list()
  output$colour     <- colour
  output$deriv      <- deriv
  if (system == "two.dim") {
    output$dx     <- dx
  }
  output$dy         <- dy
  output$parameters <- parameters
  output$points     <- points
  output$system     <- system
  output$x.lim      <- x.lim
  output$y.lim      <- y.lim
  output$x          <- x
  output$y          <- y
  return(output)
}

#http://www.magesblog.com/2014/11/phase-plane-analysis-in-r.html
#install.packages("src/phaseR_1.3.zip", lib = ".", repos = NULL, verbose = TRUE) success <- library("phaseR", lib.loc = ".", logical.return = TRUE, verbose = TRUE) library(tm) library(stringr)
#install.packages("phaseR_1.3.zip", lib=".",verbose=TRUE) 
#install.packages('phaseR', lib=".",verbose=TRUE)
# finally this is effective.
#http://stackoverflow.com/questions/27568624/installing-additional-r-package-on-azure-ml
install.packages("src/phaseR_1.3.zip", lib = ".", repos = NULL, verbose = TRUE)
success <- library("phaseR", lib.loc = ".", logical.return = TRUE, verbose = TRUE)
#library('phaseR')

FHN <- function(t, y, parameters) {
  I_0 <- parameters
  dy <- numeric(2)
  dy[1] <- 2 *  (y[2] + y[1] - 1/3 * y[1]^3) + I_0
  dy[2] <- (1 - y[1] - y[2])/2   
  return(list(dy))
}
 
phasePlot <- function(FHN, I_0=-1){
  FHN.flowField  <- flowField(FHN, x.lim = c(-3, 3), 
                              y.lim = c(-3, 3),
                              xlab="v", ylab="w",
                              main=paste0("I=", I_0),
                              parameters = I_0, 
                              points = 15, add = FALSE)  
  FHN.nullclines <- nullclines(FHN, x.lim = c(-3, 3), 
                               y.lim = c(-3, 3),
                               parameters = I_0, 
                               points = 500)  
  y0 <- matrix(c(-2, -2, 0, 0, 0.5, 0.5), 
               ncol = 2, nrow = 3, 
               byrow = TRUE)  
  FHN.trajectory <- trajectory(FHN, y0 = y0, t.end = 500, 
                               parameters = I_0)
}
 
op <- par(mfrow=c(2,2))
phasePlot(FHN, I_0= 0)
phasePlot(FHN, I_0=-1)
phasePlot(FHN, I_0=-2)
phasePlot(FHN, I_0=-3)
par(op)
