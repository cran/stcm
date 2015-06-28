#'Create interaction expansion
#'
#'\code{cond_expand} expands a dataset to include interaction values for 
#'all possible combinations of its variables.
#'
#'@usage cond_expand(data, case.ids, outcome, conditions, inter.func)
#'  
#'@param data a data frame
#'@param case.ids optional character vector containing variable names that identify cases
#'@param outcome a character string containing the name of the outcome variable
#'@param conditions an optional character vector containing the names of the causal factors
#'@param inter.func a function that combines the constituent values of an interaction
#'
#'@return Returns a data frame 
#'
#'@examples
#'# Load data
#'data(hicks_20)
#'
#'# Combine values via "min" function
#'cond_expand(data = hicks_20, case.ids = "Case", outcome = "CON")
#'
#'# Combine values via "mean" function
#'cond_expand(data = hicks_20, case.ids = "Case", outcome = "CON", inter.func = "mean")
#'
cond_expand<-function(data, case.ids, outcome, conditions, inter.func){
  
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
  
  ### Create data.frame of predictor variables
  pred<-data[,conditions]
  
  ### Expander for non-binary data
  cols<-unlist(x = lapply(X = 2:ncol(pred), FUN = function(m){utils::combn(x = colnames(pred), m = m, simplify = F)}), recursive = F)
  data.exp<-data.frame(
    sapply(X = 1:length(cols), FUN = function(col){
    apply(X = data[,cols[[col]]], MARGIN = 1, FUN = inter.func)
    })
  )
  colnames(data.exp)<-sapply(X = 1:length(cols), FUN = function(col){
    paste(cols[[col]], collapse=".")
  })
  data.exp<-data.frame(data[,c(case.ids, outcome, conditions)], data.exp)
  colnames(data.exp)<-gsub(pattern = "[.]", replacement = "_", colnames(data.exp))
  
  return(data.exp)
}