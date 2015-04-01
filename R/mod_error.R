#'Add model error to datasets
#'
#'\code{mod_error} creates a list of datasets that contain a 
#'specified type and level of measurement error
#'
#'@usage mod_error(data, case.ids, outcome, conditions, type, 
#'number, reps, analyze, max.cond.plot, ...)
#'  
#'@param data a data frame
#'@param case.ids optional character vector containing variable names that identify cases
#'@param outcome a character string containing the name of the outcome variable
#'@param conditions optional character vector indicating explanatory variables
#'@param type a character string indicating the type of set membership scores in the data; takes either
#'"crisp" for crisp-set data, or "fuzzy" for fuzzy-set data
#'@param number a numeric vector indicating the number of random variables to be added to the data
#'@param reps number of perturbed datasets to create 
#'@param analyze logical; if TRUE, carry out frequency analysis of conditions
#'@param max.cond.plot maximum number of conditions to plot for frequency analysis
#'@param ... optional arguments passed to \code{eqmcc()}
#'
#'@return Returns a named list containing the following components:
#'
#'\item{\code{datasets}}{a list of data frames containing model specification error}
#'\item{\code{result.freq}}{a data frame containing counts of condition appearances}
#'\item{\code{plot}}{a plot of condition frequencies (per solution)}
#'
#'@examples
#'# Load data
#'data(hicks_29)
#'
#'# Run function, get three random variables
#'a<-mod_error(data = hicks_29, case.ids="Case", outcome="CON", type = "crisp", 
#'number = 3, reps = 200, analyze=FALSE)
#'
#'# Load data
#'data(hh)
#'
#'# Run function, get two random variables
#'a<-mod_error(data = hh, case.ids="Country", outcome="success", type = "fuzzy", 
#'number = 2, reps = 200, analyze=FALSE)
#'
mod_error<-function(data, case.ids, outcome, conditions, type, number, reps, analyze, max.cond.plot, ...){
  
  ### Alter function argument values, if necessary
  if(missing(data)){
    stop("The 'data' argument has not been specified", call. = F)
  }
  if(!missing(case.ids)){
    data<-data[,colnames(data)[!colnames(data) %in% case.ids]]
  }
  if(missing(case.ids)){
    case.ids<-NULL
  }
  if(missing(outcome)){
    stop("The 'outcome' argument has not been specified", call. = F)
  }
  if(missing(conditions)){
    conditions<-colnames(data)[!colnames(data) %in% c(case.ids, outcome)]
  }
  if(missing(type)){
    stop("The 'type' argument has not been specified", call. = F)
  }
  if(missing(number)){
    stop("The 'number' argument has not been specified", call. = F)
  }
  if(missing(reps)){
    reps<-100
  }
  if(missing(analyze)){
    analyze<-TRUE
  }
  if(missing(max.cond.plot)){
    max.cond.plot<-50
  }
  
  ### Create list of data frames
  if(type=="crisp"){
    out<-replicate(n = reps, simplify = F, expr = {
      dat<-data.frame(matrix(data = rbinom(n = nrow(data)*number, size = 1, prob = 0.5), nrow = nrow(data), ncol = number))
      colnames(dat)<-paste("random_variable", 1:number, sep = "_")
      data.frame(data, dat)
    })
  } 
  if(type=="fuzzy"){
    out<-replicate(n = reps, simplify = F, expr = {
      dat<-data.frame(matrix(data = runif(n = nrow(data)*number, min = 0, max = 1), nrow = nrow(data), ncol = number))
      colnames(dat)<-paste("random_variable", 1:number, sep = "_")
      data.frame(data, dat)
    })
  } 
  
  if(analyze){
    
    # Get base solutions
    base.sol<-tryCatch(expr = {
      base.sol<-QCA::eqmcc(data = data, outcome = outcome, conditions = conditions, ...)$solution
      unlist(base.sol)
    }, error=function(e) NA)
    
    # Get error solutions
    results<-lapply(X = out, function(x){tryCatch(expr = {
      random<-colnames(x)[grepl(pattern = "random_variable_", x = colnames(x))]
      QCA::eqmcc(data = x, outcome = outcome, conditions = c(conditions, random), ...)$solution  
    }, error=function (e) NA)
    })
    result.freq<-sort(table(unlist(results)))
    result.freq<-data.frame(config=names(result.freq), count=result.freq, stringsAsFactors = FALSE)
    result.freq$base.sol<-ifelse(test = result.freq$config %in% base.sol, yes = "Yes", no = "No")
    rownames(result.freq)<-NULL
    
    # Make plot
    top.conds<-nrow(result.freq)-(0:(max.cond.plot-1))
    if(min(top.conds)<0){top.conds<-1:nrow(result.freq)}
    .e<-environment()
    plot<-ggplot2::ggplot(data = result.freq[top.conds,], ggplot2::aes(x = reorder(config, count), y = count/reps, fill=base.sol), environment=.e)+
      ggplot2::geom_bar(stat="identity")+ggplot2::coord_flip()+ggplot2::theme_bw()+ggplot2::xlab(label = "Condition")+ggplot2::ylab("Frequency (per Solution)")+
      ggplot2::scale_fill_manual(name="Base\nSolution?",values = c("grey50", "black"))
    
  }else{
    result.freq<-NULL
    plot<-NULL
  }
  return(list(datasets=out,
              result.freq=result.freq,
              plot=plot))
}

