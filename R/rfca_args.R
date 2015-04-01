#'Get \code{rfca()} arguments
#'
#'\code{rfca_args} creates an empty list of arguments taken 
#'by \code{rfca()} 
#'
#'@usage rfca_args()
#'
#'@return Returns a named list of arguments taken by \code{rfca()}
#'
#'@examples
#'
#'a<-rfca_args()
#'
rfca_args<-function(){
  # Get list of arguments that eqmcc can take
  rfca.args<-names(as.list(args(stcm::rfca)))
  rfca.args<-rfca.args[!rfca.args %in% c("data", "...", "")]

  # Get named list of current eqmcc arguments
  rfca.options.list<-as.list(rep(x = NA, times=length(rfca.args)))
  names(rfca.options.list)<-rfca.args
  
  return(rfca.options.list)
}