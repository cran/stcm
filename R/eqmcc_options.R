#'Simulations using \code{eqmcc()}
#'
#'\code{eqmcc_options} repeatedly executes the \code{eqmcc()} function
#'over a number of different argument values and returns its solutions  
#'
#'@usage eqmcc_options(data, eqmcc.options.list, verbose)
#'
#'@param data a data frame
#'@param eqmcc.options.list a named list containing \code{eqmcc()} arguments
#'and the values with which they should be simulated; list should be initially
#'produced by a call to \code{eqmcc_args()}
#'@param verbose logical; if TRUE, will print extra execution informaton
#'
#'@return Returns a named list containing the following elements:
#'
#'\item{\code{opts}}{a data frame containing the parameter values used for each execution}
#'\item{\code{results}}{a list of \code{eqmcc()} solutions}
#'
#'
#'@examples
#'
#'# Load data
#'data(hicks_20)
#'
#'# Generate list of eqmcc arguments
#'arglist<-eqmcc_args()
#'
#'# Specify values for execution
#'arglist$outcome<-"CON"
#'arglist$n.cut<-1:5
#'arglist$include<-c("1", "?", paste("1", "?", sep=","))
#'
#'# Run execution
#'a<-eqmcc_options(data = hicks_20[,-1], eqmcc.options.list = arglist)
#'
#'# Get data frame of parameter values
#'a$opts
#'
#'# Get the solutions for parameter specifications in row 6
#'a$opts[6,]
#'a$results[[6]]
#'
#'# Load data
#'data(hh)
#'
#'# Generate list of eqmcc arguments
#'arglist<-eqmcc_args()
#'
#'# Specify values for execution
#'arglist$outcome<-"success"
#'arglist$incl.cut1<-seq(from = 0.5, to = 1, by = 0.05)
#'arglist$n.cut<-1:5
#'arglist$include<-c("1", "?", paste("1", "?", sep=","))
#'
#'# Run execution
#'a<-eqmcc_options(data = hh[,-1], eqmcc.options.list = arglist)
#'
#'# Get data frame of parameter values
#'a$opts
#'
#'# Get the solutions for parameter specifications in row 27
#'a$opts[27,]
#'a$results[[27]]
#'
eqmcc_options<-function(data, eqmcc.options.list, verbose){
  
  #library(QCA)
  
  ### Alter function argument values, if necessary
  if(missing(data)){
    stop("The 'data' argument has not been specified", call. = F)
  }
  if(missing(eqmcc.options.list)){
    stop("The 'eqmcc.options.list' argument has not been specified", call. = F)
  }
  if(missing(verbose)){
    verbose<-FALSE
  }
  
  # Temporary workaround for eqmcc() bug
  colnames(data)<-toupper(x = colnames(data))
  eqmcc.options.list$outcome<-toupper(eqmcc.options.list$outcome)

  # Create options grid
  opts.grid<-expand.grid(eqmcc.options.list, stringsAsFactors = F)
  opts.grid<-opts.grid[,apply(X = opts.grid, MARGIN = 2, FUN = function(x){any(!is.na(x))})]
  opts.grid$list.id<-1:nrow(opts.grid)

  out<-lapply(X = 1:nrow(opts.grid), FUN = function(x){
    if(verbose){
      print(opts.grid[x,])
    }
    # Create list of options to feed to eqmcc
    opts<-as.list(opts.grid[x,])
    opts<-lapply(opts, function(x){x[[1]]})
      
    # Add dataset to the options list
    opts$data<-data
    
    # Do call to eqmcc
    sol<-tryCatch(expr = {
      do.call("eqmcc", opts)
    }, error=function(e) NULL)
    if(!is.null(sol)){
      sol$solution
    }else{
      NA
    }
  })
  
  return(list(opts=opts.grid,
              results=out))
}