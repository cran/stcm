#'Add measurement error to datasets
#'
#'\code{meas_error} creates a list of datasets that contain a 
#'specified type and level of measurement error
#'
#'@usage meas_error(data, case.ids, outcome, conditions, type,
#'  error.level, fuzzy.range, reps, analyze, max.cond.plot, ...)
#'  
#'@param data a data frame
#'@param case.ids optional character vector containing variable names that identify cases
#'@param outcome a character string containing the name of the outcome variable
#'@param conditions optional character vector indicating explanatory variables
#'@param type a character string indicating the type of set membership scores in the data; takes either
#'"crisp" for crisp-set data, or "fuzzy" for fuzzy-set data
#'@param error.level a numeric vector indicating the proportion of data points that should be 
#'perturbed with every iteration
#'@param fuzzy.range bandwidth around a data point used for sampling fuzzy-set membership scores
#'from a uniform distribution
#'@param reps number of perturbed datasets to create 
#'@param analyze logical; if TRUE, carry out frequency analysis of conditions
#'@param max.cond.plot maximum number of conditions to plot for frequency analysis
#'@param ... optional arguments passed to \code{eqmcc()}
#'
#'@return Returns a named list containing the following components:
#'
#'\item{\code{parameter.specs}}{a data frame containing parameter specifications used to generate each subset of the data frames list}
#'\item{\code{datasets}}{a list of data frames containing measurement error}
#'\item{\code{result.freq}}{a data frame containing counts of condition appearances}
#'\item{\code{plot}}{a plot of condition frequencies (per solution)}
#' 
#'@examples
#'# Load data
#'data(hicks_29)
#'
#'# Run function, get measurement error in 1, 5, and 10% of the data points
#'a<-meas_error(data = hicks_29, case.ids = "Case", outcome = "CON", type = "crisp", 
#'error.level = c(0.01, 0.05, 0.1), reps = 200, analyze=FALSE)
#'
#'# Get parameter specifications
#'a$parameter.specs
#'
#'# Isolate list of datasets with 5% measurement error
#'a$datasets[[2]]
#'
#'# Load data
#'data(hh)
#'
#'# Run function, get measurement error in 1, 5, and 10% of the data points
#'a<-meas_error(data = hh, case.ids = "Country", outcome = "success", type = "fuzzy", 
#'error.level = c(0.01, 0.05, 0.1), reps = 200, analyze=FALSE)
#'
#'# Get parameter specifications
#'a$parameter.specs
#'
#'# Isolate list of datasets with 10% measurement error
#'a$datasets[[3]]
meas_error<-function(data, case.ids, outcome, conditions, type, error.level, fuzzy.range, reps, analyze, max.cond.plot, ...){
  
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
  if(missing(error.level)){
    stop("The 'error.level' argument has not been specified", call. = F)
  }
  if(missing(reps)){
    reps<-100
  }
  if(missing(fuzzy.range)){
    fuzzy.range<-0.1
  }
  if(missing(analyze)){
    analyze<-TRUE
  }
  if(missing(max.cond.plot)){
    max.cond.plot<-50
  }
  
  ### Temporary workaround for eqmcc bug
  colnames(data)<-toupper(colnames(data))
  outcome<-toupper(outcome)
  case.ids<-toupper(case.ids)
  conditions<-toupper(conditions)
  
  ### Create options list
  opts<-expand.grid(error.level=error.level, reps=reps, stringsAsFactors = F)
  
  ### Create list of data frames
  if(type=="crisp"){
    out<-lapply(X = 1:nrow(opts), FUN = function(row){  
      out<-replicate(n = reps, simplify = F, expr = {          
        dat<-as.matrix(data)
        switchers<-sample(x = 1:length(dat), size = ceiling(length(dat)*opts$error.level[row]), replace = F)
        dat[switchers]<-abs(dat[switchers]-1) 
        data.frame(dat)
      })
    })
  }
  if(type=="fuzzy"){
    out<-lapply(X = 1:nrow(opts), FUN = function(row){  
      out<-replicate(n = reps, simplify = F, expr = {          
        dat<-as.matrix(data)
        switchers<-sample(x = 1:length(dat), size = ceiling(length(dat)*opts$error.level[row]), replace = F)
        dat[switchers]<-sapply(X = dat[switchers], FUN = function(num){
          stats::runif(n = 1, min = max(0, num-fuzzy.range/2), max = min(1, num+fuzzy.range/2))
        })
        data.frame(dat)
      })
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
      QCA::eqmcc(data = x, outcome = outcome, conditions = conditions, ...)$solution
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
    plot<-ggplot2::ggplot(data = result.freq[top.conds,], ggplot2::aes(x = stats::reorder(config, count), y = count/reps, fill=base.sol), environment=.e)+
      ggplot2::geom_bar(stat="identity")+ggplot2::coord_flip()+ggplot2::theme_bw()+ggplot2::xlab(label = "Condition")+ggplot2::ylab("Frequency (per Solution)")+
      ggplot2::scale_fill_manual(name="Base\nSolution?",values = c("grey50", "black"))
    
  }else{
    result.freq<-NULL
    plot<-NULL
  }
  
  return(list(parameter.specs=opts,
              datasets=out,
              result.freq=result.freq,
              plot=plot))
}
