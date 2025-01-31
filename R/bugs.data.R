#' Writing input for OpenBUGS
#' 
#' Write file for \pkg{OpenBUGS} to read.
#' 
#' 
#' @param data either a named list (names corresponding to variable names in
#' the \code{model.file}) of the data for the \pkg{OpenBUGS} model, \emph{or} a
#' vector or list of the names of the data objects used by the model
#' @param dir the directory to write the file \file{data.txt} to
#' @param digits number of significant digits used for \pkg{OpenBUGS} input,
#' see \code{\link{formatC}}
#' @param data.file name for the file R writes the data into.
#' @return The name of \code{data.file} is returned and as a side effect, the
#' data file is written
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords file IO
"bugs.data" <- 
function(data, dir = getwd(), digits = 5, data.file = "data.txt"){
  if(is.numeric(unlist(data)))
            write.datafile(lapply(data, formatC, digits = digits, format = "E"), 
                file.path(dir, data.file))
  else {
            data.list <- lapply(as.list(data), get, pos = parent.frame(2))
            names(data.list) <- as.list(data)
            write.datafile(lapply(data.list, formatC, digits = digits, format = "E"), 
                file.path(dir, data.file))
  }
  return(data.file)
}
