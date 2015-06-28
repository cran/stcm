#'QCA-style Inference with Predictive Models
#'
#'\code{caret_infer} produces QCA-style solutions using models from the
#' \code{caret} package and methods detailed in Krogslund & Michael (2014)  
#'data, case.ids, outcome, inter.func, type, clust.iter.max, clust.nstart, clust.alg, qca.style
#'@usage caret_infer(data, case.ids, outcome, inter.func, type, clust.iter.max, 
#'clust.nstart, clust.alg, qca.style, ...)
#'
#'@param data a data frame
#'@param outcome a character string giving the name of the outcome
#'@param case.ids a character vector giving the names of case identifiers
#'@param inter.func a function detailing how variable values should be combined 
#'to create interaction values
#'@param type one of either "c" for classification prediction or "r" for regression prediction
#'@param clust.iter.max the maximum number of iterations allowed in \code{kmeans()}
#'@param clust.nstart number of random sets to be chosen in \code{kmeans()}
#'@param clust.alg the clustering algorithm used by \code{kmeans()} 
#'@param qca.style logical; if TRUE, return QCA-style printed solutions 
#'@param ... arguments passed to \code{caret::train()}
#'
#'@return Returns a named list containing the following elements:
#'
#'\item{model}{a \code{caret} object of class "train"}
#'\item{imp.scores}{a vector of unscaled variable importance scores}
#'\item{solutions}{a vector of QCA-style sufficient conditions}
#'
#'@examples
#'
#'### NOTE: Not Run
#'
#'# Load data
#'data(hicks_29)
#'
#'# QCA-style inference using a Linear Support Vector Machine 
#'#a<-caret_infer(data = hicks_20, case.ids = "Case", outcome = "CON", type = "c", method="svmLinear")
#'
#'# Examine solutions
#'#a$solutions
#'
#'# QCA-style inference using Naive Bayes 
#'#a<-caret_infer(data = hicks_20, case.ids = "Case", outcome = "CON", type = "c", method="nb")
#'
#'# Examine solutions
#'#a$solutions
#'
#'# QCA-style inference using Neural Networks 
#'#a<-caret_infer(data = hicks_20, case.ids = "Case", outcome = "CON", type = "c", method="nnet")
#'
#'# Examine solutions
#'#a$solutions
caret_infer<-function(data, case.ids, outcome, inter.func, type, clust.iter.max, clust.nstart, clust.alg, qca.style, ...){
  
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
  if(missing(inter.func)){
    inter.func<-"min"
  }
  if(missing(type)){
    stop("The 'type' argument is not specified", call. = F)
  }
  if(missing(clust.iter.max)){
    clust.iter.max<-10
  }  
  if(missing(clust.nstart)){
    clust.nstart<-1
  }  
  if(missing(clust.alg)){
    clust.alg<-"Hartigan-Wong"
  }
  if(missing(qca.style)){
    qca.style<-FALSE
  }  
  
  # Check to make sure that variable names have not been misspecified
  bad.vars<-c(case.ids, outcome)[!c(case.ids, outcome) %in% colnames(data)]
  if(length(bad.vars)>0){
    stop(paste("The following variables do not appear in the dataset: ", paste(bad.vars, collapse=", "), sep=""), call. = F)
  }
  
  ### Check to make sure no variables have been redundantly specified
  vars<-c(case.ids, outcome)
  if(!length(vars)==length(unique(vars))){
    stop("One or more variables has been redundantly specified.\nPlease check the 'case.id' and 'outcome' arguments.", call. = F)
  }
  
  ### Expand the dataset
  data.expand<-cond_expand(data = data, case.ids = case.ids, outcome = outcome, inter.func = inter.func)

  ### Remove case.ids
  data.expand<-data.expand[,colnames(data.expand)[!colnames(data.expand) %in% case.ids]]
  
  ### Train model
  if(type=="c"){
    form<-stats::as.formula(paste("factor(", outcome, ")~.", sep=""))
  }
  if(type=="r"){
    form<-stats::as.formula(paste(outcome, "~.", sep=""))    
  }
  out<-caret::train(form = form, data = data.expand, ...)

  ### Recover importance scores
  imp.scores<-caret::varImp(object = out, scale = F)$importance
  
  ### Use k-means clustering to recover solutions
  solutions<-tryCatch(expr={
    imp<-imp.scores
    survivors<-rownames(imp)[!is.na(imp)]
    imp<-imp[!is.na(imp)]
    names(imp)<-survivors
    kmeans.sol<-stats::kmeans(x=imp, centers=2)$cluster
    s<-split(x=kmeans.sol, f=kmeans.sol)
    m<-sapply(X=1:length(s), function(x){
      mean(imp[names(imp) %in% names(s[[x]])])
    })
    s[[which(m==min(m))]]<-NULL
    kmeans.sol<-unlist(lapply(X=s, names), use.names=F)
    kmeans.sol<-gsub(pattern = "_", replacement = "*", x = kmeans.sol)
    if(qca.style){
      kmeans.sol.print<-paste(toupper(outcome), " <= ", paste(toupper(kmeans.sol), collapse = " + "))
      writeLines(strwrap(x = kmeans.sol.print, width = 70, exdent = nchar(outcome)+4))  
    }
    kmeans.sol
  }, error=function(e) "No Solution")

  return(list(model=out,
              imp.scores=imp.scores,
              solutions=solutions))  
}
  
  
