

#' Plotting summary information - intended for internal use only
#' 
#' Plotting summary information - intended for internal use
#' 
#' \code{bugs.plot.summary} (left hand side of plot) and
#' \code{bugs.plot.inferences} (right hand side of plot).
#' 
#' @aliases bugs.plot.summary bugs.plot.inferences
#' @param sims an object of class `bugs', see \code{\link{bugs}} for details
#' @param display.parallel display parallel intervals in both halves of the
#' summary plots; this is a convergence-monitoring tool and is not necessary
#' once you have approximate convergence (default is \code{FALSE})
#' @param ... further arguments to be passed to low-level plot functions
#' @return Does not return anything, but prints and plots as side-effects.
#' @seealso The main function to be called by the user is \code{plot}, see
#' \code{\link{plot.bugs}} for details.
#' @keywords internal
NULL





#' Running OpenBUGS from R
#' 
#' 
#' \pkg{R2OpenBUGS} Call a \pkg{BUGS} model, summarize inferences and
#' convergence in a table and graph, and save the simulations in arrays for
#' easy access in .  The main command is \code{\link{bugs}}.
#' 
#' 
#' The following are sources of information on \pkg{R2OpenBUGS} package:
#' \tabular{ll}{ DESCRIPTION file\tab \code{library(help="R2OpenBUGS")}\cr \tab
#' \cr This file\tab \code{package?R2OpenBUGS}\cr \tab \cr Vignette\tab
#' \code{vignette("R2OpenBUGS")}\cr \tab \cr Some help files\tab
#' \code{\link{bugs}}\cr \tab \code{\link{write.model}}\cr \tab
#' \code{\link{print.bugs}}\cr \tab \code{\link{plot.bugs}}\cr \tab \cr
#' News\tab \code{file.show(system.file("NEWS", package="R2OpenBUGS"))}\cr }
#' 
#' @name R2OpenBUGS-package
#' @aliases R2OpenBUGS-package R2OpenBUGS
#' @docType package
#' @keywords package
NULL





#' 8 schools analysis
#' 
#' 8 schools analysis
#' 
#' 
#' @name schools
#' @docType data
#' @format A data frame with 8 observations on the following 3 variables.
#' \describe{ \item{school}{See Source.} \item{estimate}{See Source.}
#' \item{sd}{See Source.} }
#' @source Rubin, D.B. (1981): Estimation in Parallel Randomized Experiments.
#' \emph{Journal of Educational Statistics} 6(4), 377-400.
#' 
#' Section 5.5 of Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
#' \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
#' @keywords datasets
NULL



