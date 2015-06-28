#'fsQCA Sufficiency Inclusion Score & Minimum Frequency Threshold Simulation
#'
#'\code{fsqca_sim} returns QCA results for a range of minimum frequency thresholds across 
#'an arbitrarily large set of sufficiency inclusion scores
#'
#'@usage fsqca_sim(data, outcome, conditions, min.incl.cut, 
#'max.incl.cut, min.n.cut, max.n.cut, reps, plot, 
#'plot.legend, verbose, ...)
#'  
#'@param data a data frame
#'@param outcome a character string or column index indicating the outcome variable
#'@param conditions optional character vector or vector of column indices indicating explanatory variables
#'@param min.incl.cut numeric lower bound for sampling of sufficiency inclusion scores
#'@param max.incl.cut numeric upper bound for sampling of sufficiency inclusion scores
#'@param min.n.cut numeric lower bound for minimum frequency thresholds
#'@param max.n.cut numeric upper bound for minimum frequency thresholds
#'@param reps number of sufficiency inclusion score pairs to be sampled
#'@param plot logical; if TRUE, returns a plot of solutions
#'@param plot.legend a character string indicating the type of legend to plot; 
#'\code{solutions} indicates plot legend should contain actual unique solutions; 
#'\code{ids} indicates plot should contain numeric identifiers for unique solutions; 
#'\code{none} indicates plot should not contain a legend
#'@param verbose logical; if TRUE, prints additional execution information 
#'@param ... optional arguments passed to \code{eqmcc()}
#'
#'@return Returns a named list with the following components: 
#'
#'\item{\code{plot}}{(if \code{PLOT==TRUE}Plot of results across \code{incl.cut} and \code{n.cut} parameter values}
#'\item{\code{results}}{a data frame containing solutions produced for each set of sampled parameter values}
#'\item{\code{legend}}{a data frame containing QCA solutions and their unique identifiers }
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
#'fsqca_sim(data = hh, outcome = "success", reps=25, plot = TRUE)
fsqca_sim<-function(data, outcome, conditions, min.incl.cut, max.incl.cut, min.n.cut, max.n.cut, reps, plot, plot.legend, verbose, ...){
    
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
  if(missing(min.n.cut)){
    min.n.cut<-1
  }
  if(missing(max.n.cut)){
    max.n.cut<-4
  }
  if(missing(reps)){
    reps<-100
  }
  if(missing(verbose)){
    verbose<-TRUE
  }  
  if(!missing(conditions)){
    # Apply fsqca_sim_inclcut across different n.cut values
    out<-lapply(X=min.n.cut:max.n.cut, FUN=function(x){
      as.data.frame(fsqca_sim_inclcut(data=data, outcome=outcome, conditions=conditions, min.incl.cut=min.incl.cut, max.incl.cut=max.incl.cut, n.cut=x, reps=reps, verbose=verbose, ...))
    })
  }else {
    # Apply fsqca_sim_inclcut across different n.cut values
    out<-lapply(X=min.n.cut:max.n.cut, FUN=function(x){
      as.data.frame(fsqca_sim_inclcut(data=data, outcome=outcome, min.incl.cut=min.incl.cut, max.incl.cut=max.incl.cut, n.cut=x, reps=reps, verbose=verbose, ...))
    })
  }
   
  # Bind results
  results<-plyr::rbind.fill(out)

  # Parse result strings
  config<-do.call(paste, as.data.frame(results[,4:ncol(results)], stringsAsFactors=FALSE))
  config<-gsub(pattern=" NA", replacement="", x=config)
  config<-gsub(pattern=" ", replacement=" + \n  ", x=config)
  
  # Clean results
  results<-cbind(results[,1:2], paste("Frequency Threshold = ", results[,3], sep=""), config, stringsAsFactors=F)
  colnames(results)[3]<-"n.cut"
  results<-plyr::arrange(df=results, plyr::desc(incl.cut1), plyr::desc(incl.cut0), n.cut)
  results$config<-ifelse(test=results$config=="NA", yes="No Solution", no=results$config)
  results$config.id<-factor(x=results$config, labels=1:length(unique(results$config)))
  
  # Create a legend of solutions
  legend<-lapply(X=1:length(unique(as.numeric(results$config.id))), function(x){
    list("config.id"=unique(as.numeric(results$config.id))[x], "config"=unique(as.character(results$config))[x])
  })
  
  # Plots
  if(missing(plot)){
    plot<-F
  }
  if(missing(plot.legend)){
    plot.legend<-"ids"
  }
  
  if(plot){
  if(plot.legend=="solutions"){
    plot<-ggplot2::ggplot(data=results, ggplot2::aes(x=incl.cut0, y=incl.cut1))+
      ggplot2::geom_point(ggplot2::aes(color=config))+ggplot2::scale_color_hue(c=150, l=60, name="Configuration")+
      ggplot2::xlim(0,1)+ggplot2::ylim(0,1)+
      ggplot2::xlab(label="Maximum Sufficiency Inclusion Score")+
      ggplot2::ylab(label="Minimum Sufficiency Inclusion Score")+
      ggplot2::guides(col=ggplot2::guide_legend(keyheight=2))+ggplot2::facet_wrap(~n.cut, ncol=2)+ggplot2::theme_bw()
  }else if(plot.legend=="ids"){
    plot<-ggplot2::ggplot(data=results, ggplot2::aes(x=incl.cut0, y=incl.cut1))+
      ggplot2::geom_point(ggplot2::aes(color=config.id))+ggplot2::scale_color_hue(c=150, l=60, name="Configuration ID")+
      ggplot2::xlim(0,1)+ggplot2::ylim(0,1)+
      ggplot2::xlab(label="Maximum Sufficiency Inclusion Score")+
      ggplot2::ylab(label="Minimum Sufficiency Inclusion Score")+
      ggplot2::guides(col=ggplot2::guide_legend(keyheight=2))+ggplot2::facet_wrap(~n.cut, ncol=2)+ggplot2::theme_bw()
  }else if(plot.legend=="none"){
    plot<-ggplot2::ggplot(data=results, ggplot2::aes(x=incl.cut0, y=incl.cut1))+
      ggplot2::geom_point(ggplot2::aes(color=config.id))+ggplot2::scale_color_hue(c=150, l=60, name="Configuration ID")+
      ggplot2::xlim(0,1)+ggplot2::ylim(0,1)+
      ggplot2::xlab(label="Maximum Sufficiency Inclusion Score")+
      ggplot2::ylab(label="Minimum Sufficiency Inclusion Score")+
      ggplot2::guides(col=F)+ggplot2::facet_wrap(~n.cut, ncol=2)+ggplot2::theme_bw()  
  }
  return(list("plot"=plot, "results"=results[,-4], "legend"=legend))
  } else{
    return(list("results"=results[,-4], "legend"=legend))
  }
}