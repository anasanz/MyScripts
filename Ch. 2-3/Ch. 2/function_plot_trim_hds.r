

# Creation and modification of trim functions


overall <- function(x, which=c("imputed","fitted"), changepoints=numeric(0), bc=FALSE) {
  stopifnot(class(x)=="trim")
  which = match.arg(which)
  
  # Handle automatic selection of changepoints based on the model
  if (is.character(changepoints) && changepoints=="model") {
    changepoints <- x$changepoints
  }
  
  # Convert year-based changepoints if required
  if (all(changepoints %in% x$time.id)) changepoints <- match(changepoints, x$time.id)
  
  # extract vars from TRIM output
  tt_mod <- x$tt_mod
  tt_imp <- x$tt_imp
  var_tt_mod <- x$var_tt_mod
  var_tt_imp <- x$var_tt_imp
  J <- ntime <- x$ntime
  
  # Set changepoints in case none are given
  if (length(changepoints)==0) changepoints <- 1
  
  # if (base>0) { # use index instead
  #     browser()
  #     tt     = tt_mod
  #     var_tt = var_tt_mod
  #     b = base
  #
  #     tau <- tt / tt[b]
  #     J <- length(tt)
  #     var_tau <- numeric(J)
  #     for (j in 1:J) {
  #       d <- matrix(c(-tt[j] / tt[b]^2, 1/tt[b]))
  #       V <- var_tt[c(b,j), c(b,j)]
  #       var_tau[j] <- t(d) %*% V %*% d
  #     }
  #
  #     tt_mod <- tt_imp <- tau
  #     var_tt_mod <- var_tt_imp <- diag(var_tau)
  # }
  
  .meaning <- function(bhat, berr, df) {
    if (df<=0) return("Unknown (df<=0)")
    alpha = c(0.05, 0.001)
    stopifnot(df>0)
    if (bc) {
      # Backwards compatbility, not recommended
      tval <- qnorm((1-alpha/2))
    } else {
      tval <- qt((1-alpha/2), df)
    }
    blo <- bhat - tval * berr
    bhi <- bhat + tval * berr
    
    # First priority: evidece for a strong trend?
    if (blo[2] > +0.05) return("Strong increase (p<0.01)")
    if (bhi[2] < -0.05) return("Strong decrease (p<0.01)")
    if (blo[1] > +0.05) return("Strong increase (p<0.05)")
    if (bhi[1] < -0.05) return("Strong decrease (p<0.05)")
    # if (blo[2] > 1.05) return("Strong increase (p<0.01)")
    # if (bhi[2] < 0.95) return("Strong decrease (p<0.01)")
    # if (blo[1] > 1.05) return("Strong increase (p<0.05)")
    # if (bhi[1] < 0.95) return("Strong decrease (p<0.05)")
    
    # Second prority: evidence for a moderate trend?
    eps = 1e-7 # required to get a correct interpretation for slope=0.0 (Stable)
    if (blo[2] > +eps) return("Moderate increase (p<0.01)")
    if (bhi[2] < -eps) return("Moderate decrease (p<0.01)")
    if (blo[1] > +eps) return("Moderate increase (p<0.05)")
    if (bhi[1] < -eps) return("Moderate decrease (p<0.05)")
    # if (blo[2] > 1.0+eps) return("Moderate increase (p<0.01)")
    # if (bhi[2] < 1.0-eps) return("Moderate decrease (p<0.01)")
    # if (blo[1] > 1.0+eps) return("Moderate increase (p<0.05)")
    # if (bhi[1] < 1.0-eps) return("Moderate decrease (p<0.05)")
    #
    # Third priority: evidency for stability?
    if (blo[1] > -0.05 && bhi[1] < 0.05) return("Stable")
    # if (blo[1]>0.95 && bhi[1]<1.05) return("Stable")
    
    # Leftover category: uncertain
    return("Uncertain")
  }
  
  # The overall slope is computed for both the modeled and the imputed $\Mu$'s.
  # So we define a function to do the actual work
  .compute.overall.slope <- function(tpt, tt, var_tt, src) {
    # tpt = time points, either 1..J or year1..yearn
    n <- length(tpt)
    stopifnot(length(tt)==n)
    stopifnot(nrow(var_tt)==n && ncol(var_tt)==n)
    
    # handle zero time totals (might happen in imputed TT)
    problem = tt<1e-6
    log_tt = log(tt)
    log_tt[problem] <- 0.0
    alt_tt <- exp(log_tt)
    
    # Use Ordinary Least Squares (OLS) to estimate slope parameter $\beta$
    X <- cbind(1, tpt) # design matrix
    y <- matrix(log_tt)
    #y[tt<1e-6] = 0.0 # Handle zero (or very low) counts
    bhat <- solve(t(X) %*% X) %*% t(X) %*% y # OLS estimate of $b = (\alpha,\beta)^T$
    yhat <- X %*% bhat
    
    # Apply the sandwich method to take heteroskedasticity into account
    dvtt <- 1/alt_tt # derivative of $\log{\Mu}$
    Om <- diag(dvtt) %*% var_tt %*% diag(dvtt) # $\var{log{\Mu}}$
    var_beta <- solve(t(X) %*% X) %*% t(X) %*% Om %*% X %*% solve(t(X) %*% X)
    b_err <- sqrt(diag(var_beta))
    
    # Compute the $p$-value, using the $t$-distribution
    df <- n - 2
    t_val <- bhat / b_err
    if (df>0) p <- 2 * pt(abs(t_val), df, lower.tail=FALSE)
    else      p <- c(NA, NA)
    
    # Also compute effect size as relative change during the monitoring period.
    #effect <- abs(yhat[J] - yhat[1]) / yhat[1]
    
    # Reverse-engineer the SSR (sum of squared residuals) from the standard error
    j <- 1 : n
    D <- sum((j-mean(j))^2)
    SSR <- b_err[2]^2 * D * (n-2)
    
    # Export the results
    z <- data.frame(
      add       = bhat,
      se_add    = b_err,
      mul       = exp(bhat),
      se_mul    = exp(bhat) * b_err,
      p         = p,
      row.names = c("intercept","slope")
    )
    z$meaning   = c("<none>", .meaning(z$add[2], z$se_add[2], n-2))
    
    list(src=src, coef=z, SSR=SSR)
  }
  
  if (which=="imputed") {
    tt     <- tt_imp
    var_tt <- var_tt_imp
    src = "imputed"
  } else if (which=="fitted") {
    tt     <- tt_mod
    var_tt <- var_tt_mod
    src = "fitted"
  }
  
  J = length(tt)
  if (length(changepoints)==0) {
    # Normal overall slope
    out <- .compute.overall.slope(1:J, tt, var_tt, src)
    out$type <- "normal" # mark output as 'normal' overall slope
  } else {
    # overall slope per changepoint. First some checks.
    stopifnot(min(changepoints)>=1)
    stopifnot(max(changepoints)<J)
    stopifnot(all(diff(changepoints)>0))
    ncp <- length(changepoints)
    cpx <- c(changepoints, J) # Extend list of overall changepoints with final year
    int.collector <- data.frame() # Here go the intercepts
    slp.collector <- data.frame() # Here go the slopes
    SSR.collector <- numeric(ncp) # Here go the SSR info
    for (i in 1:ncp) {
      idx <- cpx[i] : cpx[i+1]
      tmp <- .compute.overall.slope(idx, tt[idx], var_tt[idx,idx], src)
      prefix <- data.frame(from=x$time.id[cpx[i]], upto=x$time.id[cpx[i+1]])
      intercept <- tmp$coef[1,] # Intercept is on first row of output dataframe
      int.collector <- rbind(int.collector, cbind(prefix, intercept))
      slope <- tmp$coef[2,] # Slope is on second row of output dataframe
      slp.collector <- rbind(slp.collector, cbind(prefix, slope))
      SSR.collector[i] = tmp$SSR
    }
    out <- list(src=src, slope=slp.collector, intercept=int.collector, SSR=SSR.collector)
  }
  out$J = J
  out$tt = tt
  out$err = sqrt(diag(var_tt))
  out$timept <- x$time.id # export time points for proper plotting
  structure(out, class="trim.overall")
}


# ------------------------------------------------------------------- Print ----

#' Print an object of class trim.overall
#'
#' @param x An object of class \code{trim.overall}
#'
#' @export
#' @keywords internal
print.trim.overall <- function(x,...) {
  print(x$slope, row.names=FALSE)
}

#--------------------------------------------------------------------- Plot ----

#' Plot overall slope
#'
#' Creates a plot of the overall slope, its 95\% confidence band, the
#' total population per time and their 95\% confidence intervals.
#'
#' @param x An object of class \code{trim.overall} (returned by \code{\link{overall}})
#' @param imputed \code{[logical]} Toggle to show imputed counts
#' @param ... Further options passed to \code{\link[graphics]{plot}}
#'
#' @family analyses
#'
#' @examples
#' data(skylark)
#' m <- trim(count ~ site + time, data=skylark, model=2)
#' plot(overall(m))
#'
#' @export

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
  
  # Points and lines hds
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col= adjustcolor("black",alpha.f = 0.4))
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= "white", cex = 1.5)
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= adjustcolor("black",alpha.f = 0.4), cex = 1.5)
  
  points(mean.pred.exp ~ yrs2, type="l", col= "black", lwd = 2)
  
  # Print estimates
  legend("bottomleft", legend = c(paste("HDS:",significance_est_hds), paste("TRIM:", significance_est_ci_trim)), fill = c("red", "black"), bty = "n")
  
}


# Function 2: For last graph, contains x-axes
plot.trim.hds.overall_yo2 <- function(x, yrange = c(0, 100), imputed=TRUE, ...) {
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
  axis(side = 1, cex = 1.2)
  
  #mtext("Number of individuals", side = 2, line = 2, cex = 0.8) # For individual graphs
  #mtext("Year", side = 1, line = 3, cex = 0.8)
  
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
  
  # Points and lines hds
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col= adjustcolor("black",alpha.f = 0.4))
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= "white", cex = 1.5)
  points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= adjustcolor("black",alpha.f = 0.4), cex = 1.5)
  
  points(mean.pred.exp ~ yrs2, type="l", col= "black", lwd = 2)
  
  legend("bottomleft", legend = c(paste("HDS:",significance_est_hds), paste("TRIM:", significance_est_ci_trim)), fill = c("red", "black"), bty = "n")
}

