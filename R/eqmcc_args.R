#'Get \code{eqmcc()} arguments
#'
#'\code{eqmcc_args} creates an empty list of arguments taken 
#'by \code{eqmcc()} 
#'
#'@usage eqmcc_args()
#'
#'@return Returns a named list of arguments taken by \code{eqmcc()}
#'
#'@examples
#'
#'a<-eqmcc_args()
#'
eqmcc_args<-function(){
  # Get list of arguments that eqmcc can take
  eqmcc.args<-names(as.list(args(QCA::eqmcc)))
  eqmcc.args<-eqmcc.args[!eqmcc.args %in% c("data", "...", "")]
  
  # Get named list of current eqmcc arguments
  eqmcc.options.list<-as.list(rep(x = NA, times=length(eqmcc.args)))
  names(eqmcc.options.list)<-eqmcc.args
  
  return(eqmcc.options.list)
}