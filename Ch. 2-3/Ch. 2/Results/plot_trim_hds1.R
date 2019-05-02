# Function 1: For first graphs, no x-axes
plot.trim.hds.overall_yo1 <- function(x, yrange = c(0, 100), imputed=TRUE, ...) {
  #browser()
  X <- x
  title <- if (is.null(list(...)$main)){
    attr(X, "title")
  } else {
    list(...)$main
  }
  
  tpt = X$timept
  J <- X$J
  
  # Collect all data for plotting: time-totals
  ydata <- X$tt
  
  # error bars
  y0 = ydata - X$err
  y1 = ydata + X$err
  
  trend.line <- NULL
  conf.band  <- NULL
  
  X$type <- "changept" # Hack for merging overall/changepts
  if (X$type=="normal") {
    # Trend line
    a <- X$coef[[1]][1] # intercept
    b <- X$coef[[1]][2] # slope
    x <- seq(1, J, length.out=100) # continue timepoint 1..J
    ytrend <- exp(a + b*x)
    xtrend <- seq(min(tpt), max(tpt), len=length(ytrend)) # continue year1..yearn
    trendline = cbind(xtrend, ytrend)
    
    # Confidence band
    xconf <- c(xtrend, rev(xtrend))
    alpha <- 0.05
    df <- J - 2
    t <- qt((1-alpha/2), df)
    j = 1:J
    dx2 <- (x-mean(j))^2
    sumdj2 <- sum((j-mean(j))^2)
    dy <- t * sqrt((X$SSR/(J-2))*(1/J + dx2/sumdj2))
    ylo <- exp(a + b*x - dy)
    yhi <- exp(a + b*x + dy)
    yconf <- c(ylo, rev(yhi))
    conf.band <- cbind(xconf, yconf)
  } else if (X$type=="changept") {
    nsegment = nrow(X$slope)
    for (i in 1:nsegment) {
      
      # Trend line
      a <- X$intercept[i,3]
      b <- X$slope[i,3]
      from <- which(tpt==X$slope[i,1]) # convert year -> time
      upto <- which(tpt==X$slope[i,2])
      delta = (upto-from)*10
      x      <- seq(from, upto, length.out=delta) # continue timepoint 1..J
      ytrend <- exp(a + b*x)
      xtrend <- seq(tpt[from], tpt[upto], length.out=length(ytrend))
      if (i==1) {
        trendline = cbind(xtrend, ytrend)
      } else {
        trendline = rbind(trendline, NA)
        trendline = rbind(trendline, cbind(xtrend, ytrend))
      }
      
      # Confidence band
      xconf <- c(xtrend, rev(xtrend))
      alpha <- 0.05 # Confidence level
      ntpt <- upto - from + 1 # Number of time points in segment
      df <- ntpt - 2
      if (df<=0) next # No confidence band for this segment...
      
      t <- qt((1-alpha/2), df)
      j = from : upto
      dx2 <- (x-mean(j))^2
      sumdj2 <- sum((j-mean(j))^2)
      SSR = X$SSR[i] # Get stored SSR as computed by overall()
      dy <- t * sqrt((SSR/df)*(1/ntpt + dx2/sumdj2))
      ylo <- exp(a + b*x - dy)
      yhi <- exp(a + b*x + dy)
      yconf <- c(ylo, rev(yhi))
      
      if (is.null(conf.band)) {
        conf.band <- cbind(xconf, yconf)
      } else {
        conf.band = rbind(conf.band, NA)
        conf.band = rbind(conf.band, cbind(xconf, yconf))
      }
      
    }
    #yrange = c(300,700)
  } else stop("Can't happen")
  
  # Compute the total range of all plot elements (but limit the impact of the confidence band)
  xrange = range(trendline[,1], na.rm=TRUE)
  #yrange1 = range(range(y0), range(y1), range(trendline[,2]), na.rm=TRUE)
  #yrange2 = range(range(conf.band[,2], na.rm=TRUE))
  #yrange = range(yrange1, yrange2, na.rm=TRUE)
  #ylim = 2 * yrange1[2]
  #if (yrange[2] > ylim) yrange[2] = ylim
  yrange = yrange
  
  # Ensure y-axis starts at 0.0
  #yrange <- c(0,100) # Modified by me to put the maximun hds_ci for each species 
  
  # Now plot layer-by-layer (using ColorBrewer colors)
  cbred <- rgb(228,26,28, maxColorValue = 255)
  cbblue <- rgb(55,126,184, maxColorValue = 255)
  
  plot(xrange, yrange, type='n', xlab=" ", ylab= " ", las=1, main = s_good[xxx], cex.lab = 1.2, cex.axis = 1, axes = FALSE, ...)
  
  axis(side = 2, cex = 1.2)
  #axis(side = 1, cex = 1.2)
  
  # For individual graphs:
  #mtext("Number of individuals", side = 2, line = 2, cex = 0.8) 
  
  polygon(conf.band, col= adjustcolor("red",alpha.f = 0.2), lty=0) # Polygon ci trim
  
  polygon( x = c(yrs2, rev(yrs2)),                                # Polygon ci hds
           y = c(lci.exp, rev(uci.exp)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  
  # Points and lines trim
  points(tpt, ydata, col= adjustcolor("red",alpha.f = 0.4), type='l', pch=16, lwd = 1) 
  points(tpt, ydata, col= "white", type='p', pch=16, lwd = 1, cex = 1.5)
  points(tpt, ydata, col= adjustcolor("red",alpha.f = 0.4), type='p', pch=16, lwd = 1, cex = 1.5)
  
  lines(trendline, col= "red", lwd = 2) # trendline
  #segments(tpt,y0, tpt,y1, lwd=3, col=gray(0.5))
  
  # Significant deviations trim
  x_dev_trim <- tpt*cont_zero_dev
  x_dev_trim <- x_dev_trim[x_dev_trim != 0]  # To put it on the right side
  y_dev_trim <- ydata*cont_zero_dev
  y_dev_trim <- y_dev_trim[y_dev_trim != 0]
  
  # Points and lines hds
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col= adjustcolor("black",alpha.f = 0.4))
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= "white", cex = 1.5)
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= adjustcolor("black",alpha.f = 0.4), cex = 1.5)
  
  points(mean.pred.exp ~ yrs2, type="l", col= "black", lwd = 2)
  
  # Significant deviations hds
  x_dev_hds <- yrs2*year_dev_hds 
  x_dev_hds <- x_dev_hds[x_dev_hds != 0]  # To put it on the left side
  y_dev_hds <- out$summary[grep("popindex", rownames(out$summary)),1]*year_dev_hds
  y_dev_hds <- y_dev_hds[y_dev_hds != 0]
  
  # Print both significant deviations
  points(x_dev_hds, y_dev_hds, col= adjustcolor("black",alpha.f = 0.8), type='p', pch = 8, cex = 1.7)
  points(x_dev_trim, y_dev_trim, col= adjustcolor("red",alpha.f = 0.8), type='p', pch = 8, cex = 1.7) 
  
  # Print estimates
  par(xpd = TRUE)
  legend("topright", legend = c(paste("HDS:",significance_est_hds), paste("TRIM:", significance_est_ci_trim)), 
         fill = c("black", "red"), bg = "white", box.lty=0, xpd = TRUE, xjust = 0, yjust = 5)
  
}

