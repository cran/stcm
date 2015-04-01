#'Summarize an STCM dataset
#'
#'\code{paths_summary} provides descriptions of an STCM dataset related to cases
#'and causal paths.
#'
#'@usage paths_summary(data, case.ids, outcome, conditions, plot, verbose, 
#'size.warn)
#'  
#'@param data a data frame
#'@param case.ids optional character vector containing variable names that identify cases
#'@param outcome a character string containing the name of the outcome variable
#'@param conditions   an optional character vector containing the names of the causal factors
#'@param plot logical; if \code{TRUE}, a plot of causal pathways is returned
#'@param verbose logical; if \code{TRUE}, information on code execution will be printed
#'@param size.warn logical; if \code{TRUE}, prompts the user to authorize execution when number of causal paths >=1000
#'
#'@return Returns a named list with the following components: 
#'
#'\item{\code{n.cases}}{number of cases in the dataset}
#'\item{\code{n.cases.table}}{a data frame containing the number of unique cases for each variable in \code{case.ids}}
#'\item{\code{n.paths.possible}}{number of possible paths to the outcome}
#'\item{\code{n.paths.observed}}{number of observed unique paths to the outcome}
#'\item{\code{n.paths.percent}}{percent of possible paths to the outcome observed}
#'\item{\code{paths.obs}}{a data frame containing the observed unique paths to the outcome}
#'\item{\code{paths.unobs}}{a data frame containing the observed unique paths to the outcome}
#'\item{\code{freq.paths.obs}}{a data frame containing the observed unique paths to the outcome, the number of cases corresponding
#'to those paths, and their \code{case.id} values}
#'\item{\code{var.diversity}}{a data frame containing a measure of diversity in the observed values for each variable;
#'ranges from 0 (observed values are either all 0 or all 1) to 1 (observed values are equally divided between 0s and 1s)}
#'\item{\code{paths.plot}}{if \code{plot==TRUE}, a path diagram; solid lines indicate observed paths, while 
#'dotted lines indicate unobserved paths; the outcome values associated with each path are also given}
#'\item{\code{multicollinearity.df}}{a data frame bivariate correlations and their absolute values for all causal factors}
#'\item{\code{multicollinearity.avg}}{the average absolute bivariate correlation}
#'
#'@examples
#'# Load data
#'data(hicks_20)
#'
#'# Run function
#'a<-paths_summary(data = hicks_20, case.ids = "Case", outcome = "CON", plot = TRUE)
#'
#'# Get number of cases
#'a$n.cases
#'
#'# Get number of paths to the outcome observed
#'a$n.paths.observed
#'
#'# Get percent of possible paths observed
#'a$n.paths.percent
#'
#'# Get multicollinearity
#'a$multicollinearity.df.avg
#'
#'# View paths plot
#'a$paths.plot
paths_summary<-function(data, case.ids, outcome, conditions, plot, verbose, size.warn){
  
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
  if(missing(plot)){
    plot<-FALSE
  }
  if(missing(verbose)){
    verbose<-TRUE
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
  
  if(verbose){
    cat("Creating dataset...\n")
  }
  ### Create dataset to use in analysis
  d<-data[,vars]
  unique.vals<-unique(unlist(d[,colnames(d)[!colnames(d) %in% case.ids]]))
  if(FALSE %in% (unique.vals %in% c(0,1))){
    stop("Data values do not appear to be binary (0,1)", call. = F)
  }
  
  if(verbose){
    cat("Getting number of cases...\n")
  }
  ### Get the total number of cases
  n.cases<-NROW(unique(d[,case.ids]))
  
  if(verbose){
    cat("Getting number of cases by case.id...\n")
  }
  ### Get the total number of cases for each unique case id
  n.cases.table<-data.frame(matrix(data = NA, nrow = length(case.ids), ncol = 2, dimnames = list(NULL, c("case.id", "n.unique"))))
  for(id in 1:length(case.ids)){
    n.cases.table[id,"case.id"]<-case.ids[id]
    n.cases.table[id,"n.unique"]<-NROW(unique(d[,case.ids[id]])) 
  }
  
  if(verbose){
    cat("Getting number of possible paths...\n")
  }
  ### Get the number of possible paths to outcome
  n.paths.possible<-2^length(conditions)
  
  if(verbose){
    cat("Getting number of unique observed paths...\n")
  }
  ### Get the number of observed paths to outcome
  n.paths.observed<-NROW(unique(d[,conditions]))

  
  ### Get percent of possible paths observed
  n.paths.percent<-n.paths.observed/n.paths.possible*100
  
  if(verbose){
    cat("Getting possible paths...\n")
  }
  ### Get possible paths
  paths.pos<-data.frame(unique(t(combn(x = rep(x = c(0,1), times = length(conditions)), m = length(conditions)))))
  paths.pos<-paths.pos[do.call(order,paths.pos),]
  colnames(paths.pos)<-conditions
  rownames(paths.pos)<-NULL

  if(verbose){
    cat("Getting unique observed paths...\n")
  }
  ### Get observed paths
  paths.obs<-unique(d[,conditions])
  paths.obs<-paths.obs[do.call(order,paths.obs),]

  if(verbose){
    cat("Getting unobserved paths...\n")
  }
  ### Get unobserved paths
  obs<-unlist(lapply(X = 1:NROW(paths.obs), FUN = function(y){
    lapply(X = 1:NROW(paths.pos), FUN = function(x){
      if(all.equal(paths.obs[y,], paths.pos[x,], check.attributes = F)[1]==TRUE){x}
    })
  }))
  paths.obs<-paths.pos[c(obs),]
  paths.unobs<-paths.pos[-c(obs),]
  
  ### Get frequencies for the observed paths
  out<-lapply(X = 1:NROW(paths.obs), FUN = function(y){
    out<-unlist(lapply(X = 1:NROW(d[,conditions]), FUN = function(x){
      if(all.equal(target = paths.obs[y,], current = d[x,conditions], check.attributes = F)[1]==TRUE){x}
    }))
    config<-unique(d[out,colnames(d)[!colnames(d) %in% c(case.ids, outcome)]])
    if(is.null(dim(d[out,case.ids]))){
      data.frame(config, n.obs=length(out), cases=paste(d[out,case.ids], collapse = "; "))
    }else{
      data.frame(config, n.obs=length(out), cases=paste(do.call("paste", d[out,case.ids]), collapse="; "))
    }
  })  
  freq.paths.obs<-do.call("rbind", out)
  rownames(freq.paths.obs)<-NULL
  freq.paths.obs<-plyr::arrange(df = freq.paths.obs, plyr::desc(n.obs))  
  
  if(verbose){
    cat("Getting unique outcomes for observed paths...\n")
  }
  ### Get outcome types for each observed path
  paths.obs.unique <-sapply(X = 1:NROW(paths.obs), FUN = function(y){
    out<-unlist(lapply(X = 1:NROW(d[,conditions]), FUN = function(x){
      if(all.equal(target = paths.obs[y,], current = d[x,conditions], check.attributes = F)[1]==TRUE){x}
    }))
    paste(sort(unique(d[out,outcome])), collapse=",")   
  })
  
  if(verbose){
    cat("Getting variable diversity measures...\n")
  }
  ### Get variable diversity measures
  var.diversity<-lapply(X = conditions, FUN = function(x){
    data.frame(condition=x, diversity=1-abs(mean(data[,x])-0.5)/0.5)
  })
  var.diversity<-do.call("rbind", var.diversity)
  var.diversity<-plyr::arrange(var.diversity, plyr::desc(diversity))  
  
  ### Dendrogram
  if(plot){
    if(verbose){
      cat("Creating plot...\n")
    }
    options(warn=-1)
    ### Create dendrogram of paths to outcome
    
    # Make distance matrix from x = 1:(2^k)
    in.mat<-dist(x = 1:n.paths.possible)
    
    # Create the cluster object
    out<-hclust(d = in.mat, method = "average")
    
    # Normalize the height of the tree
    span<-1/length(unique(out$height))
    for(i in 1:length(unique(out$height))){
      out$height[out$height==unique(out$height)[i]]<-0+(span*i)
    }
    
    # Convert data into class of dendrogram
    d.out<-as.dendrogram(out)
    
    # Plot
    plot(d.out)
    
    # Indicate nodes that are observed
    obs<-as.numeric(rownames(paths.obs))
    
    # Set labels to be known outcomes
    #library(dendextend)
    labels(d.out)[as.numeric(rownames(paths.obs))]<-paths.obs.unique
    labels(d.out)[-as.numeric(rownames(paths.obs))]<-"?"
    
    # More plotting
    d.out %>% dendextend::set("by_labels_branches_lty", value = c("0", "1", "0,1"), c(1,3), type="any") %>% 
      dendextend::set("by_labels_branches_lwd", value = c("0", "1", "0,1"), c(3,1), type="any") %>% plot(yaxt = "n", ann=F)

    node.xy<-dendextend::get_nodes_xy(x = d.out)
    for(i in 1:length(unique(node.xy[,2]))){
      if(unique(node.xy[,2])[i]!=0){
        y.val<-unique(node.xy[,2])[i]
        x.vals<-node.xy[,1][node.xy[,2]==y.val]
        text(cex=.75, x = x.vals, y = y.val-(span*.1), paste("0", colnames(paths.pos)[i], "1", sep="-"))
      }
    }
    paths.plot<-recordPlot()
    options(warn=1)
  }else{
    paths.plot<-NULL
  }
  
  if(verbose){
    cat("Getting multicollinearity measurements...\n")
  }
  # Calculate multicollinearity (avg bivariate correlation)
  multivar.com<-combn(x = conditions, m = 2)
  cors<-apply(X = multivar.com, MARGIN = 2, FUN = function(x){
    cor(x = d[,x[1]], y = d[,x[2]])
  })
  multicollinearity.df<-data.frame(condition=t(multivar.com), cor=cors, abs.cors=abs(cors))
  multicollinearity.df<-plyr::arrange(multicollinearity.df, plyr::desc(abs.cors))
  multicollinearity.avg<-mean(abs(cors))
  
  if(verbose){
    cat("Done.\n\n")
  }
  # Return list
  return.list<-list(n.cases=n.cases,
                    n.cases.table=n.cases.table,
                    n.paths.possible=n.paths.possible,
                    n.paths.observed=n.paths.observed,
                    n.paths.percent=n.paths.percent,
                    paths.obs=paths.obs,
                    paths.unobs=paths.unobs,
                    freq.paths.obs=freq.paths.obs,
                    var.diversity=var.diversity,
                    paths.plot=paths.plot,
                    multicollinearity.df=multicollinearity.df,
                    multicollinearity.avg=multicollinearity.avg)
  return(return.list)
}

