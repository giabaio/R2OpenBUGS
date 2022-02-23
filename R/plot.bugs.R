#' Plotting a bugs object
#' 
#' Plotting a \code{bugs} object
#' 
#' 
#' @param x an object of class `bugs', see \code{\link{bugs}} for details
#' @param display.parallel display parallel intervals in both halves of the
#' summary plots; this is a convergence-monitoring tool and is not necessary
#' once you have approximate convergence (default is \code{FALSE})
#' @param ... further arguments to \code{\link{plot}}
#' @seealso \code{\link{bugs}}
#' @keywords hplot
plot.bugs <- function (x, display.parallel = FALSE, ...){
    mar.old <- par("mar")
    pty.old <- par(pty = "m")
    mfrow.old <- par("mfrow")
        layout(matrix(c(1,2),1,2))
        
    bugs.plot.summary (x, ...)
    bugs.plot.inferences (x, display.parallel, ...)
    header <- ""
    if(!is.null(x$model.file))
        header <- paste(header, "Bugs model at \"", x$model.file, "\", ", sep="")
    if(!is.null(x$program))
        header <- paste(header, "fit using ", x$program, ", ", sep="")
    header <- paste(header, x$n.chains, " chains, each with ",
        x$n.iter, " iterations (first ", x$n.burnin, " discarded)", sep = "")
    mtext(header, outer = TRUE, line = -1, cex = 0.7)
    par(pty = pty.old[[1]], mar = mar.old, mfrow = mfrow.old)
}


#' Tidyverse based function to do traceplots
#' 
#' Traceplot for a \code{bugs} object
#' 
#' 
#' @param x an object of class `bugs', see \code{\link{bugs}} for details
#' @param parameter a string with the name of the parameter for which to show
#' the traceplot
#' @param ... further arguments to \code{\link{traceplot}}
#' @seealso \code{\link{bugs}}
traceplot=function(x,parameter,...) {
  x$sims.array[,,parameter] %>% 
    as_tibble(.name_repair = ~paste("Chain",1:x$n.chains)) %>% mutate(iteration=row_number()) %>% 
    gather(chain,value,contains("Chain"),-iteration) %>% ggplot(aes(x=iteration,y=value,color=chain))+geom_line() +
    theme_bw() + labs(title=paste("Traceplot for",parameter))
}


#' Density plots for the posteriors in a \code{bugs} object
#' 
#' 
#' @param x an object of class `bugs', see \code{\link{bugs}} for details
#' @param parameter a string with the name of the parameter for which to show
#' the density plot
#' @param ... further arguments to \code{\link{densityplot}}
#' @seealso \code{\link{bugs}}
densityplot=function(x,parameter=NULL,...) { 
  if(is.null(parameter)) {
    x$sims.matrix %>% as_tibble() %>% gather(variable,value,1:ncol(.)) %>% 
      ggplot(aes(value))+geom_density()+facet_wrap(~variable,scales="free_y") + 
      theme_bw()
  # This would do a barplot of all the variables
  # x$sims.matrix %>% as_tibble() %>% gather(variable,value,1:ncol(.)) %>% 
  #  ggplot(aes(value))+geom_density()+facet_wrap(~variable,scales="free_y") + 
  #    theme_bw()
  } else {
    x$sims.matrix[,parameter] %>% as_tibble() %>% ggplot(aes(value))+geom_density() +
      labs(x=parameter,title=paste("Density plot for",parameter)) + theme_bw()
  }
}
