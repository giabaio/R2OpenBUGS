fround <- function(x, digits)
    format(round(x, digits), nsmall=digits)



#' Printing a bugs object
#' 
#' Printing a \code{bugs} object
#' 
#' 
#' @param x an object of class `bugs', see \code{\link{bugs}} for details
#' @param digits.summary rounding for tabular output on the console (default is
#' to round to 1 decimal place)
#' @param interval [added by GB] the quantiles for the posterior distribution to 
#' be displayed in the summary statistics table (defaults to 0.025 and 0.975, 
#' which will show the 2.5-th and 97.5-th quantiles of the posterior, so as to 
#' give an approximate central 95% interval). Other choices are 0.25, 0.5 and 
#' 0.75 to show the 25-th, median and 75-th quantiles of the posterior
#' @param ... further arguments to \code{\link{print}}
#' @seealso \code{\link{bugs}}
#' @keywords print
print.bugs <- function(x, digits.summary = 3, interval = c(.025, .975), ...)
{
    if(!is.null(x$model.file))
        cat("Inference for Bugs model at \"", x$model.file, "\", ", sep="")
    if(!is.null(x$program))
        cat("fit using ", x$program, ",", sep="")
    cat("\nCurrent: ", x$n.chains, " chains, each with ", x$n.iter,
        " iterations (first ", x$n.burnin, " discarded)", sep = "")
    if(x$n.thin > 1) cat(", n.thin =", x$n.thin)
    cat("\nCumulative: n.sims =", x$n.sims, "iterations saved\n")
    ####
    # Change by GB: allows to select different intervals
    toshow=c(
      pmatch("mean",x$summary |> colnames()),
      pmatch("sd",x$summary |> colnames()), 
      pmatch(interval*100, x$summary |> colnames())
    )
    # If they exist also add "Rhat" and "n.eff"
    pos=pmatch(c("Rhat","n.eff"),x$summary|>colnames())
    if(!(pos |> is.na() |> any())) {
      toshow=c(
        toshow,
        pmatch("Rhat",x$summary |> colnames()),
        pmatch("n.eff",x$summary |> colnames())
      )
    }
    print(round(x$summary[,toshow], digits.summary), ...)
    ####

    if(x$n.chains > 1) {
      cat("\nFor each parameter, n.eff is a crude measure of effective sample size,")
      cat("\nand Rhat is the potential scale reduction factor (at convergence, Rhat=1).\n")
    }

    if(x$isDIC) {
      msgDICRule <- ifelse(x$DICbyR,
                           "(using the rule, pD = var(deviance)/2)", ## Gelman tweak
                           "(using the rule, pD = Dbar-Dhat)")       ## BUGS
      cat(paste("\nDIC info ", msgDICRule, "\n", sep=""))
      if(length(x$DIC) == 1) {
        cat("pD =", fround(x$pD, digits.summary), "and DIC =", fround(x$DIC, digits.summary))
      } else if(length(x$DIC)>1) {
        print(round(x$DIC, digits.summary))
      }
      cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
    }
    invisible(x)
}
