#' Calling OpenBUGS
#' 
#' Calls \pkg{OpenBUGS} and runs it with \file{script.txt} - intended for
#' internal use
#' 
#' 
#' @param n.burnin length of burn in
#' @param OpenBUGS.pgm Full path to the \pkg{OpenBUGS} executable or shell
#' script
#' @param debug When debug=T, the OpenBUGS gui is displayed during execution.
#' there is no effect with linux execution
#' @param useWINE as in \code{\link{bugs}} meta function
#' @param WINE as in \code{\link{bugs}} meta function
#' @param newWINE as in \code{\link{bugs}} meta function
#' @param WINEPATH as in \code{\link{bugs}} meta function
#' @return Nothing, but has side effects as documented in
#' \code{\link{bugs.update.settings}} and calls \pkg{OpenBUGS}.
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords internal
"bugs.run" <-
    function(n.burnin, OpenBUGS.pgm, debug=FALSE,
             useWINE=FALSE, WINE=NULL,
             newWINE=TRUE, WINEPATH=NULL)
{

## Update the lengths of the adaptive phases in the Bugs updaters
#  try(bugs.update.settings(n.burnin, bugs.directory))
#
#  ## Return the lengths of the adaptive phases to their original settings
#    .fileCopy <- file.copy
#  on.exit(try(.fileCopy(file.path(bugs.directory, "System/Rsrc/Registry_Rsave.odc"),
#                        file.path(bugs.directory, "System/Rsrc/Registry.odc"),
#                        overwrite=TRUE)))


  ## Call Bugs and have it run with script.txt
  if(.Platform$OS.type == "windows" || useWINE){
      bugsCall <- paste("\"", OpenBUGS.pgm, "\" /PAR \"",
                        native2win(file.path(getwd(), "script.txt"),
                                   useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH),
                        "\" /", sep="")
      if(!debug)bugsCall<-paste(bugsCall,"HEADLESS",sep="")
      if(useWINE) bugsCall <- paste(WINE, bugsCall)
  }else{
      bugsCall <- paste(OpenBUGS.pgm, "<", "script.txt", ">", file.path(getwd(), "log.txt"))
  }
  if((.Platform$OS.type == "windows" || useWINE) && debug){
    temp <- system(bugsCall,invisible=FALSE)
  }else temp <- system(bugsCall)

  if(temp == -1)
    stop("Error in bugs.run().")

  ##   !!!! need to check these conditions
  ## Stop and print an error message if Bugs did not run correctly
    tmp <- scan("CODAchain1.txt", character(), quiet=TRUE, sep="\n")
  tmp <- tmp[1:min(100,length(tmp))]
  if(length(grep("OpenBUGS did not run correctly", tmp)) > 0) stop(paste("Look at the log file in ",getwd(),
      " and\ntry again with 'debug=TRUE' to figure out what went wrong within OpenBUGS."))
}
