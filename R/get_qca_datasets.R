#'Get QCA datasets
#'
#'\code{get_qca_datasets} downloads a list of QCA datasets from the 
#'COMPASSS.org database
#'
#'@usage get_qca_datasets(csQCA, mvQCA, fsQCA)
#'  
#'@param csQCA logical; if TRUE, download all available csQCA datasets
#'@param mvQCA logical; if TRUE, download all available mvQCA datasets
#'@param fsQCA logical; if TRUE, download all available fsQCA datasets
#'
#'@return Returns a named list of QCA datasets with author identifiters
#'
#'@examples
#'# Get all csQCA datasets (not run)
#'
#'# a<-get_qca_datasets(csQCA = TRUE)
#'
#'# Get all csQCA and fsQCA datasets (not run)
#'
#'# a<-get_qca_datasets(csQCA = TRUE, fsQCA = TRUE)
#'
get_qca_datasets<-function(csQCA, mvQCA, fsQCA){
  
  if(missing(csQCA)){
    csQCA<-F
  }
  if(missing(mvQCA)){
    mvQCA<-F
  }
  if(missing(fsQCA)){
    fsQCA<-F
  }
  
  if(missing(csQCA) & missing(mvQCA) & missing(fsQCA)){
    stop("At least one kind of QCA dataset must be selected")
  }
  if(!csQCA & !mvQCA & !fsQCA){
    stop("At least one kind of QCA dataset must be selected")
  }
  
  if(csQCA){
    url<-"http://www.compasss.org/bibliography/csQCAapp.htm"
    doc<-XML::htmlParse(url)
    stubs<-as.vector(XML::xpathSApply(doc, "//span[@class='data']/a/@href", simplify=T))
    links<-paste("http://www.compasss.org/bibliography/", stubs, sep="")
    cat("Downloading csQCA datasets ")
    csQCA.datasets<-lapply(X=1:length(links), function(x){
      cat(".")
      out.data<-tryCatch(utils::read.csv(file=links[x]), error=function(e) NULL)
      out.link<-stubs[x]
      return(list(out.link, out.data))
    })
  }
  
  if(mvQCA){
    url<-"http://www.compasss.org/bibliography/mvQCAapp.htm"
    doc<-XML::htmlParse(url)
    stubs<-as.vector(XML::xpathSApply(doc, "//span[@class='data']/a/@href", simplify=T))
    links<-paste("http://www.compasss.org/bibliography/", stubs, sep="")
    cat("Downloading mvQCA datasets ")
    mvQCA.datasets<-lapply(X=1:length(links), function(x){
      cat(".")
      out.data<-tryCatch(utils::read.csv(file=links[x]), error=function(e) NULL)
      out.link<-stubs[x]
      return(list(out.link, out.data))
    })
  }
  
  if(fsQCA){
    url<-"http://www.compasss.org/bibliography/fsQCAapp.htm"
    doc<-XML::htmlParse(url)
    stubs<-as.vector(XML::xpathSApply(doc, "//span[@class='data']/a/@href", simplify=T))
    links<-paste("http://www.compasss.org/bibliography/", stubs, sep="")
    cat("Downloading fsQCA datasets ")
    fsQCA.datasets<-lapply(X=1:length(links), function(x){
      cat(".")
      out.data<-tryCatch(utils::read.csv(file=links[x]), error=function(e) NULL)
      out.link<-stubs[x]
      return(list(out.link, out.data))
    })
  }
  
  if(csQCA & !mvQCA & !fsQCA){return(list("csQCA.datasets"=csQCA.datasets))}
  if(!csQCA & mvQCA & !fsQCA){return(list("mvQCA.datasets"=mvQCA.datasets))}
  if(!csQCA & !mvQCA & fsQCA){return(list("fsQCA.datasets"=fsQCA.datasets))}
  if(csQCA & mvQCA & !fsQCA){return(list("csQCA.datasets"=csQCA.datasets, "mvQCA.datasets"=mvQCA.datasets))}
  if(csQCA & !mvQCA & fsQCA){return(list("csQCA.datasets"=csQCA.datasets, "fsQCA.datasets"=fsQCA.datasets))}
  if(!csQCA & mvQCA & fsQCA){return(list("mvQCA.datasets"=mvQCA.datasets, "fsQCA.datasets"=fsQCA.datasets))}
  if(csQCA & mvQCA & fsQCA){return(list("csQCA.datasets"=csQCA.datasets, "mvQCA.datasets"=mvQCA.datasets, "fsQCA.datasets"=fsQCA.datasets))} 
}
