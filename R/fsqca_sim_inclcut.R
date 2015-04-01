#'fsQCA Sufficiency Inclusion Score Simulation
#'
#'\code{fsqca_sim_inclcut} returns QCA results for a range of minimum frequency thresholds across 
#'an arbitrarily large set of sufficiency inclusion scores
#'
#'@usage fsqca_sim_inclcut(data, outcome, conditions, min.incl.cut, 
#'max.incl.cut, n.cut, reps, verbose, ...)
#'  
#'@param data a data frame
#'@param outcome a character string or column index indicating the outcome variable
#'@param conditions optional character vector or vector of column indices indicating explanatory variables
#'@param min.incl.cut numeric lower bound for sampling of sufficiency inclusion scores
#'@param max.incl.cut numeric upper bound for sampling of sufficiency inclusion scores
#'@param n.cut minimum frequency threshold
#'@param reps number of sufficiency inclusion score pairs to be sampled
#'@param verbose logical; if TRUE, prints additional execution information 
#'@param ... optional arguments passed to \code{eqmcc()}
#'
#'@return Returns adata frame containing solutions produced for each set of sampled parameter values 
#'
#'@examples
#'
#'# Load data
#'data(hh)
#'
#'# Remove case indicator
#'hh<-hh[,-which(colnames(hh)=="Country")]
#'
#'# Run function
#'a<-fsqca_sim_inclcut(data = hh, outcome = "success", n.cut=2)
#'
#'# Get table of solutions (note: truncated to first 50 rows)
#'a[1:50,]
#'
#'# Increase the number of replications
#'#'a<-fsqca_sim(data = hh, outcome = "success", reps=1000, plot = TRUE)
fsqca_sim_inclcut<-function(data, outcome, conditions, min.incl.cut, max.incl.cut, n.cut, reps, verbose, ...){

  ### Temporary workaround for bug in QCA::eqmcc()
  colnames(data)[which(colnames(data)==outcome)]<-toupper(x = outcome)
  outcome<-toupper(x = outcome)
  
  ### Alter function argument values, if necessary
  if(missing(data)){
    stop("The 'data' argument has not been specified", call. = F)
  }
  if(missing(outcome)){
    stop("The 'outcome' argument has not been specified", call. = F)
  }
  if(missing(min.incl.cut)){
    min.incl.cut<-0
  }
  if(missing(max.incl.cut)){
    max.incl.cut<-1
  }
  if(missing(reps)){
    reps<-100
  }
  if(missing(verbose)){
    verbose<-TRUE
  }  
  
  # Check argument classes
  if(!is.data.frame(data)){
    stop("'data' object must be of class 'data.frame")
  }
  if(!is.character(outcome) & !is.numeric(outcome)){
    stop("'outcome' argument must be either a character vector or a vector of column indices")
  }
  if(!is.numeric(min.incl.cut) | !is.numeric(max.incl.cut) | !is.numeric(n.cut) | !is.numeric(reps)){
    stop("'min.incl.cut', 'max.incl.cut', 'n.cut', and 'reps' arguments must be of class 'numeric'")
  }
  if(min.incl.cut<0 | min.incl.cut>1 | max.incl.cut<0 | max.incl.cut>1){
    stop("'min.incl.cut' and 'max.incl.cut' arguments must be between 0 and 1")
  }
  if(n.cut<1){
    stop("'n.cut' argument must be greater than or equal to 1")
  }
  if(reps<1){
    stop("'reps' argument must be greater than or equal to 1")
  }
  
  # Create incl.cut value pairs
  incl.cut1.val<-runif(n=3*reps, min=min.incl.cut, max=max.incl.cut)
  incl.cut0.val<-runif(n=3*reps, min=0, max=max.incl.cut)
  n.cut.val<-rep(x=n.cut, times=3*reps)
  vals<-cbind(incl.cut1.val, incl.cut0.val, n.cut.val)
  vals<-vals[incl.cut1.val>=incl.cut0.val,]
  vals<-vals[sample(x=1:nrow(vals), size=reps, replace=FALSE),]
  vals<-cbind(vals, progress=1:reps/reps*100)
  
  # Prepare data object for analysis
  if(is.numeric(outcome)){
    outcome<-colnames(data)[outcome]
  }
  if(!missing(conditions)){
    if(!is.character(conditions) & !is.numeric(conditions)){
      stop("'conditions' argument must be either a character vector or a vector of column indices")
    }
    if(is.numeric(conditions)){
      conditions<-colnames(data)[conditions]
    }
    data<-data[,colnames(data) %in% c(outcome, conditions)]
  }
  
  # Apply the error handling function
  out<-apply(X=vals, MARGIN=1, FUN=function(x){
    if(verbose){
      cat("Percent complete: n.cut = ", x[3], ", ", x[4], "%\n", sep="")
    }
    out<-tryCatch(expr=QCA::eqmcc(data=data,outcome=outcome,incl.cut1=x[1],incl.cut0=x[2],n.cut=x[3], ...)$solution, error=function(e) NA)
    as.data.frame(t(do.call("rbind", lapply(X=out, paste, collapse="+"))))   
  })
  
  # Combine and clean results (removing progress indicator)
  results<-data.frame(vals[, -4], plyr::rbind.fill(out))
  colnames(results)[1:3]<-c("incl.cut1", "incl.cut0", "n.cut")
  colnames(results)[4:length(colnames(results))]<-paste("Solution", 1:length(4:length(colnames(results))))
  
  return(results)
}