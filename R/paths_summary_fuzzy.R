#'Summarize a fuzzy STCM dataset
#'
#'\code{paths_summary_fuzzy} provides descriptions of an STCM dataset related to cases
#'and causal paths.
#'
#'@usage paths_summary_fuzzy(data, case.ids, outcome, conditions, 
#'  inter.func, sig.digits, size.warn) 
#'  
#'@param data a data frame
#'@param case.ids optional character vector containing variable names that identify cases
#'@param outcome a character string containing the name of the outcome variable
#'@param conditions   an optional character vector containing the names of the causal factors
#'@param inter.func a function that combines the constituent values of an interaction
#'@param sig.digits number of significant digits for cutpoints
#'@param size.warn logical; if \code{TRUE}, prompts the user to authorize execution when number of causal paths >=1000
#'
#'@return Returns a named list with the following components: 
#'
#'\item{\code{n.fuzzy.paths.obs}}{a data frame containing the number and percent of possible paths observed for a given binary cutpoint}
#'\item{\code{fuzzy.paths.plot}}{a plot of the percent of possible paths observed across a range of binary cutpoints}

#'@examples
#'# Load data
#'data(hh)
#'
#'# Run function
#'a<-paths_summary_fuzzy(data = hh, case.ids = "Country", outcome = "success", sig.digits = 1)
#'
#'# Get results
#'a$n.fuzzy.paths.obs
#' 
#'# Get plot
#'a$fuzzy.paths.plot
paths_summary_fuzzy<-function(data, case.ids, outcome, conditions, inter.func, sig.digits, size.warn){
  
  ### Alter function argument values, if necessary
  if(missing(data)){
    stop("The 'data' argument has not been specified", call. = F)
  }
  if(missing(case.ids)){
    Case<-1:nrow(data)
    data<-data.frame(Case, data)
    case.ids<-"Case"
  }
  if(missing(outcome)){
    stop("The 'outcome' argument has not been specified", call. = F)
  }
  if(missing(conditions)){
    conditions<-colnames(data)[!colnames(data) %in% c(case.ids, outcome)]
  }
  if(missing(inter.func)){
    inter.func<-"min"
  }
  if(missing(sig.digits)){
    sig.digits<-2
  }
  if(missing(size.warn)){
    size.warn<-TRUE
  }

  # Check to make sure that variable names have not been misspecified
  bad.vars<-c(case.ids, outcome, conditions)[!c(case.ids, outcome, conditions) %in% colnames(data)]
  if(length(bad.vars)>0){
    stop(paste("The following variables do not appear in the dataset: ", paste(bad.vars, collapse=", "), sep=""), call. = F)
  }
  
  ### Check to make sure no variables have been redundantly specified
  vars<-c(case.ids, outcome, conditions)
  if(!length(vars)==length(unique(vars))){
    stop("One or more variables has been redundantly specified.\nPlease check the 'case.id', 'outcome' and 'conditions' arguments.", call. = F)
  }
  
  ### Issue warning if large number of conditions is to be computed
  if(length(conditions)>=10){
    if(size.warn){
      proceed<-readline("\nWarning: there are over 1000 possible paths to the outcome.  Computation may proceed slowly.\n\n Do you wish to proceed?")
      if(!tolower(proceed) %in% c("y", "yes", "t", "true", "1", "yeah", "sure", "ok")){return()}    
    }
  }  
  
  ### Get expanded dataset
  data.expand<-cond_expand(data = data, case.ids = case.ids, outcome = outcome, conditions = conditions, inter.func = inter.func)
  
  ### Remove case.ids and outcome to yield expanded predictors dataset
  pred<-data.expand[,colnames(data.expand)[!colnames(data.expand) %in% c(case.ids, outcome)]]
  
  ### Get number of unique paths observed
  n.paths.obs<-nrow(unique(pred))
  
  n.fuzzy.paths.obs<-lapply(X = seq(from = 0, to = 1, by = 0.1^sig.digits), FUN = function(cut){
    temp.pred<-pred
    temp.pred[pred<cut]<-0
    temp.pred[pred>=cut]<-1
    data.frame(cut, n.fuzzy.paths.obs=nrow(unique(temp.pred)), n.paths.pos=2^(length(conditions)))
  })
  n.fuzzy.paths.obs<-do.call("rbind", n.fuzzy.paths.obs)
  n.fuzzy.paths.obs$pct.fuzzy.paths.obs<-n.fuzzy.paths.obs$n.fuzzy.paths.obs/n.fuzzy.paths.obs$n.paths.pos*100

  # Plot percentage paths v. cut
  fuzzy.paths.plot<-ggplot2::ggplot(data = n.fuzzy.paths.obs, ggplot2::aes(x = cut, y = pct.fuzzy.paths.obs))+ggplot2::geom_line(size=1)+
    ggplot2::xlim(0,1)+ggplot2::ylim(0,100)+ggplot2::xlab(label = "Binary Cutpoint")+ggplot2::ylab(label = "Paths Observed (% Possible Paths)")+
    ggplot2::theme_bw()
  
  return(list(n.fuzzy.paths.obs=n.fuzzy.paths.obs,
              fuzzy.paths.plot=fuzzy.paths.plot))
}