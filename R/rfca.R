#'Carry out RFCA
#'
#'\code{rfca} carries out Random Forest Comparative Analysis (RFCA) as detailed
#'in Krogslund & Michael (2014)  
#'
#'@usage rfca(data, outcome, case.ids, fuzzy, ntree, mtry, inter.func, 
#'clust.iter.max, clust.nstart, clust.alg, qca.style, ...)
#'
#'@param data a data frame
#'@param outcome a character string giving the name of the outcome
#'@param case.ids a character vector giving the names of case identifiers
#'@param fuzzy logical; if TRUE, data will be dichotomized around the median
#'@param ntree the number of trees to be grown
#'@param mtry the number of variables to try at each split
#'@param inter.func a function detailing how variable values should be combined 
#'to create interaction values
#'@param clust.iter.max the maximum number of iterations allowed in \code{stats::kmeans()}
#'@param clust.nstart number of random sets to be chosen in \code{stats::kmeans()}
#'@param clust.alg the clustering algorithm used by \code{stats::kmeans()} 
#'@param qca.style logical; if TRUE, return QCA-style printed solutions 
#'@param ... arguments passed to \code{randomForest()}
#'
#'@return Returns a vector of variable names
#'
#'@examples
#'
#'# Load data
#'data(hicks_29)
#'
#'# Run RFCA
#'a<-rfca(data = hicks_29, outcome = "CON", case.ids = "Case", ntree = 100, mtry = 5)
#'
#'# Increase the number of trees grown and variables tried at each split
#'a<-rfca(data = hicks_29, outcome = "CON", case.ids = "Case", ntree = 100, mtry = 5)
#'
#'# Load data
#'data(hh)
#'
#'# Run RFCA
#'a<-rfca(data = hh, outcome = "success", case.ids = "Country", fuzzy = TRUE, 
#'ntree = 100, mtry = 5)
rfca<-function(data, outcome, case.ids, fuzzy, ntree, mtry, inter.func, clust.iter.max, clust.nstart, clust.alg, qca.style, ...){
  
  ### Alter function argument values, if necessary
  if(missing(data)){
    stop("The 'data' argument has not been specified", call. = F)
  }
  if(missing(outcome)){
    stop("The 'outcome' argument has not been specified", call. = F)
  }
  if(!missing(case.ids)){
    data<-data[,!colnames(data) %in% case.ids]
  }
  if(missing(case.ids)){
    case.ids<-NULL
  }
  if(missing(ntree)){
    ntree<-2000
  }
  if(missing(mtry)){
    mtry<-2^(ncol(data[,!colnames(data) %in% c(case.ids, outcome)]))-1
  }
  if(missing(inter.func)){
    inter.func<-"min"
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
  if(missing(fuzzy)){
    fuzzy<-FALSE
  }
  
  ### Carry out dichotomization, if necessary
  if(fuzzy){
    dat.f<-as.matrix(data[sapply(data,is.numeric)])
    crossover<-stats::median(dat.f, na.rm = T)
    dat.f[dat.f>=crossover]<-1
    dat.f[dat.f<crossover]<-0
    data[sapply(data,is.numeric)]<-dat.f
  }

  ### Expand dataset
  dat<-stcm::cond_expand(data = data, outcome = outcome, inter.func = inter.func)
  dat<-dat[,!colnames(dat)%in% c("Case", outcome, case.ids)]
 
  ### Run RFCA algorithm
  result<-randomForest::randomForest(y = factor(data[,outcome]), x = dat, ntree = ntree, mtry = mtry, ...)
  result<-tryCatch(expr = {
    imp<-result$importance
    kmeans.sol<-stats::kmeans(x=imp, centers=2, iter.max = clust.iter.max, algorithm = clust.alg)$cluster
    s<-split(x=kmeans.sol, f=kmeans.sol)
    m<-sapply(X=1:length(s), function(x){
      mean(imp[rownames(imp) %in% names(s[[x]])])
    })
    s[[which(m==min(m))]]<-NULL
    kmeans.sol<-unlist(lapply(X=s, names), use.names=F) 
    kmeans.sol<-gsub(pattern = "_", replacement = "*", x = kmeans.sol)
    if(qca.style){
      kmeans.sol.print<-paste(toupper(outcome), " <= ", paste(toupper(kmeans.sol), collapse = " + "))
      writeLines(strwrap(x = kmeans.sol.print, width = 70, exdent = nchar(outcome)+4))  
    }
    kmeans.sol
  }, error=function(e) NULL)
  if(!is.null(result)){
    result
  }else{
    "No Solution"
  } 
}