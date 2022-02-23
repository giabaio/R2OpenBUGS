#' Writing input for OpenBUGS
#' 
#' Write files \file{inits1.txt}, \file{inits2.txt}, etc., in the working
#' directory for \pkg{OpenBUGS} to read
#' 
#' 
#' @param inits a list with \code{n.chains} elements; each element of the list
#' is itself a list of starting values for the \pkg{OpenBUGS} model, \emph{or}
#' a function creating (possibly random) initial values
#' @param n.chains number of Markov chains
#' @param digits number of significant digits used for \pkg{OpenBUGS} input,
#' see \code{\link{formatC}}
#' @param inits.files name for the inits files R write the inits into.
#' @return Vector of names of \code{inits.files}; as a side effect, the inits
#' files \file{inits*.txt} are written
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords IO file
"bugs.inits" <- 
function (inits, n.chains, digits,
          inits.files = paste("inits", 1:n.chains, ".txt", sep = "")){
    if(!is.null(inits)) {
        for(i in 1:n.chains) {
            if(is.function(inits))
                    write.datafile(lapply(inits(), formatC, digits = digits, format = "E"),
                        inits.files[i])
            else 
                write.datafile(lapply(inits[[i]], formatC, digits = digits, format = "E"), 
                    inits.files[i])
        }
    }
    return(inits.files)
}
