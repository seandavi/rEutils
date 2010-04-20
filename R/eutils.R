### Utility functions
singleValueIfElse <- function(test,yes,no) {
  if(test) return(yes) else return(no)
}

library(RCurl)
library(XML)
setClassUnion('CharOrNULL',c('character','NULL'))
setClass('eUtilsQuery',
         contains='list')
setClass('eSearchQuery',
         contains='eUtilsQuery')
setClass('eSummaryQuery',
         contains='eUtilsQuery')
setClass('eFetchQuery',
         representation(parser='function'),
         prototype(parser=function(x) {return(readLines(x))}),
         contains='eUtilsQuery')
setClass('eLinkQuery',
         contains='eUtilsQuery')
setClass('eInfoQuery',
         contains='eUtilsQuery')
setClass('eUtilsResult',
         representation(queryparams='list',
                        error='CharOrNULL'),
         prototype(queryparams=list(),
                   error=NULL))
setClass('eUtilsXMLResult',
         representation(xmlresult='XMLNode'),
         prototype(xmlresult=xmlNode('empty')),
         contains='eUtilsResult')
setClass('eInfoResultDbSpecific',
         representation(db='character',
                        menuname='character',
                        description='character',
                        count='numeric',
                        lastupdate='character',
                        links='data.frame',
                        fields='data.frame'),
         contains='eUtilsResult')
setClass('eInfoResultOverview',
         representation(dbnames='character'),
         contains='eUtilsResult')
setClass('eSearchResult',
         representation(webenv='CharOrNULL',
                        query_key='CharOrNULL',
                        queryurl='character',
                        idlist='vector',
                        count='numeric',
                        retstart='numeric',
                        retmax='numeric',
                        querytranslation='CharOrNULL'),
         prototype(webenv=NULL,
                   query_key=NULL,
                   queryurl='',
                   idlist=vector('character',0),
                   count=0,
                   retstart=0,
                   retmax=0,
                   querytranslation=NULL),
         contains='eUtilsXMLResult')
setClass('eSummaryResult',
         contains='eUtilsXMLResult')

eUtilsURLbase <- function(program=c('eSearch','eSummary','eFetch',
                            'ePost','eLink','eInfo')) {
  urlbase <- switch(program,
                    eSearch='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?',
                    eSummary='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?',
                    eFetch='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?',
                    ePost='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/epost.fcgi?',
                    eLink='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?',
                    eInfo='http://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi?')
  return(urlbase)
}
 
eUtilsQueryString <- function(params) {
  tmp <- getOption('scipen')
  options('scipen'=15) # to reduce chance of url escaping numbers in scientific notation
  querystring=paste(paste(names(params),curlEscape(as.character(params)),sep="="),collapse="&")
  options('scipen'=tmp)
  return(querystring)
}

.eSearch <- function(object,...) {
  params <- c(object,...)
  db <- params$db
  querystring <- eUtilsQueryString(params)
  urlbase <- eUtilsURLbase('eSearch')
  url <- paste(urlbase,querystring,sep="")
  xmlrawresult <- xmlRoot(xmlTreeParse(getURL(url)))
  if("ERROR" %in% names(xmlrawresult)) {
    errorval <- xmlValue(xmlrawresult[['ERROR']])
    warning(sprintf('Query [%s]\n generated ERROR: %s',querystring,errorval))
    result <- new('eSearchResult',error=errorval,
                  db=object$db,
                  queryparams=params,
                  xmlresult=xmlrawresult)
    return(result)
  }
  result <- new('eSearchResult',
                queryparams=params,
                xmlresult=xmlrawresult,
                webenv=singleValueIfElse(is.null(xmlrawresult[['WebEnv']]),
                  NULL,
                  xmlValue(xmlrawresult[['WebEnv']])),
                query_key=singleValueIfElse(is.null(xmlrawresult[['QueryKey']]),
                  NULL,
                  xmlValue(xmlrawresult[['QueryKey']])),
                idlist=xmlSApply(xmlrawresult[['IdList']],xmlValue),
                queryurl=url,
                retstart=as.integer(xmlValue(xmlrawresult[['RetStart']])),
                retmax=as.integer(xmlValue(xmlrawresult[['RetMax']])),
                count=as.integer(xmlValue(xmlrawresult[['Count']])),
                querytranslation=xmlValue(xmlrawresult[['QueryTranslation']]))
  return(result)
} 

.eSummary <- function(object,...) {
  db=object$db
  params <- c(object,...)
  querystring <- eUtilsQueryString(params)
  urlbase <- eUtilsURLbase('eSummary')
  url <- paste(urlbase,querystring,sep="")
  xmlrawresult <- xmlRoot(xmlTreeParse(url))
  if("ERROR" %in% names(xmlrawresult)) {
    errorval <- xmlValue(xmlrawresult[['ERROR']])
    warning(sprintf('Query [%s]\n generated ERROR: %s',querystring,errorval))
    result <- new('eSummaryResult',error=errorval,
                  queryparams=params,
                  xmlresult=xmlrawresult)
    return(result)
  }
  result <- new('eSummaryResult',
                  queryparams=params,
                  xmlresult=xmlrawresult)
  return(result)
} 

.eFetch <- function(object,...) {
  db=object@dat$db
  params <- c(object@dat,...)
  querystring <- eUtilsQueryString(params)
  urlbase <- eUtilsURLbase('eFetch')
  furl <- paste(urlbase,querystring,sep="")
  result <- object@parser(furl)
  return(result)
} 

#####################################
###
### .eInfo
### Called by 'eInfo' method
###
### Returns polymorphically
### If a "db" parameter is given, returns
### detailed information about the database
### in an eInfoResultDbSpecific class.  If
### not, then, an eInfoResultOverview class
### is returned.
###
#####################################
.eInfo <- function(object,...) {
  params <- c(object,...)
  querystring <- eUtilsQueryString(params)
  urlbase <- eUtilsURLbase('eInfo')
  url <- paste(urlbase,querystring,sep="")
  xmlrawresult <- xmlRoot(xmlTreeParse(url))
  if("ERROR" %in% names(xmlrawresult)) {
    errorval <- xmlValue(xmlrawresult[['ERROR']])
    result <- new('eInfoResult',error=errorval,
                  queryparams=params,
                  xmlresult=xmlrawresult)
  }
  ## db name given, so DB specific information
  if(names(xmlrawresult)[1]=='DbInfo') {
    if('FieldList' %in% names(xmlrawresult[[1]])) {
      fields <- data.frame(t(xmlSApply(xmlrawresult[[1]][['FieldList']],function(x) {xmlSApply(x,xmlValue)})),row.names=1)
      fields <- fields[order(row.names(fields)),]
    } else {
      fields <- data.frame(NULL)
    }
    if('LinkList' %in% names(xmlrawresult[[1]])) {
      links <- data.frame(t(xmlSApply(xmlrawresult[[1]][['LinkList']],function(x) {xmlSApply(x,xmlValue)})),row.names=1)
      links <- links[order(row.names(links)),]
    } else {
      links <- data.frame(NULL)
    }
    result <- new('eInfoResultDbSpecific',
                  queryparams=params,
                  db=xmlValue(xmlrawresult[[1]][['DbName']]),
                  menuname=xmlValue(xmlrawresult[[1]][['MenuName']]),
                  description=xmlValue(xmlrawresult[[1]][['Description']]),
                  count=as.integer(xmlValue(xmlrawresult[[1]][['Count']])),
                  lastupdate=xmlValue(xmlrawresult[[1]][['LastUpdate']]),
                  fields=fields,
                  links=links)
    return(result)
  }
  ## no db name given, so just dbnames come back
  if(names(xmlrawresult)=='DbList') {
    result <- new("eInfoResultOverview",
                  queryparams=params,
                  dbnames=xmlSApply(xmlrawresult[[1]],xmlValue))
    return(result)
  }
} 

.eLink <- function(object,...) {
  db=object$db
  newparams <- list(...)
  for(i in names(newparams)) {
    params[[i]] <- newparams[[i]]
  }
  params <- c(object,...)
  querystring <- eUtilsQueryString(params)
  urlbase <- eUtilsURLbase('eLink')
  url <- paste(urlbase,querystring,sep="")
  xmlrawresult <- xmlRoot(xmlTreeParse(url))
#  if("ERROR" %in% names(xmlrawresult)) {
#    errorval <- xmlValue(xmlrawresult[['ERROR']])
#    warning(sprintf('Query [%s]\n generated ERROR: %s',querystring,errorval))
#    result <- new('eSummaryResult',error=errorval,
#                  queryparams=params,
#                  xmlresult=xmlrawresult)
#    return(result)
#  }
#  result <- new('eResult',
#                  queryparams=params,
#                  xmlresult=xmlrawresult)
  return(xmlrawresult)
} 



setMethod('initialize',signature(.Object='eSearchQuery'),
          function(.Object,
                   db='pubmed',
                   term,...)
          {
            callNextMethod(.Object,.Data=list(db=db,term=term,...))
          })

setMethod('initialize',signature(.Object='eSummaryQuery'),
          function(.Object,
                   esearchquery,
                   db=esearchquery@queryparams$db,
                   WebEnv=esearchquery@webenv,
                   retmax=100,
                   query_key=esearchquery@query_key,...)
          {
            callNextMethod(.Object,.Data=list(db=db,WebEnv=WebEnv,query_key=query_key,retmax=retmax,...))
          })

setMethod('initialize',signature(.Object='eFetchQuery'),
          function(.Object,
                   esearchquery,
                   parser=function(x) return(x),
                   db=esearchquery@queryparams$db,
                   WebEnv=esearchquery@webenv,
                   query_key=esearchquery@query_key,...)
          {
            callNextMethod(.Object,parser=parser,
                           dat=list(db=db,WebEnv=WebEnv,
                             query_key=query_key,...),.Data=list())
          })

setMethod('initialize',signature(.Object='eLinkQuery'),
          function(.Object,
                   esearchquery,
                   db=esearchquery@queryparams$db,
                   dbto='pubmed',
                   WebEnv=esearchquery@webenv,
                   query_key=esearchquery@query_key,...)
          {
            callNextMethod(.Object,.Data=list(WebEnv=WebEnv,
                                     query_key=query_key,dbfrom=db,db=dbto,...))
          })

setMethod('initialize',signature(.Object='eInfoQuery'),
          function(.Object,...)
          {
            callNextMethod(.Object,.Data=list(...))
          })

setGeneric('eSearch',function(object,...) {
  standardGeneric('eSearch')})

setGeneric('eSummary',function(object,...) {
  standardGeneric('eSummary')})

setGeneric('eFetch',function(object,...) {
  standardGeneric('eFetch')})

setGeneric('eLink',function(object,...) {
  standardGeneric('eLink')})

setGeneric('eInfo',function(object,...) {
  standardGeneric('eInfo')})

setMethod('eSearch','eSearchQuery',function(object,...) {
  .eSearch(object,...)
}
)

setMethod('eSummary','eSummaryQuery',function(object,...) {
  .eSummary(object,...)
}
)

setMethod('eSummary','eSearchResult',function(object,...) {
  equery <- new('eSummaryQuery',esearchquery=object,...)
  .eSummary(equery,...)
}
)

setMethod('eFetch','eFetchQuery',function(object,...) {
  .eFetch(object,...)
}
)

setMethod('eFetch','eSearchResult',function(object,...) {
  equery <- new('eFetchQuery',esearchquery=object,...)
  .eFetch(equery)
}
)

setMethod('eLink','eLinkQuery',function(object,...) {
  .eLink(object,...)
}
)

setMethod('eLink','eSearchResult',function(object,...) {
  equery <- new('eLinkQuery',esearchquery=object,...)
  .eLink(equery,...)
}
)

setMethod('eInfo','eInfoQuery',function(object,...) {
  .eInfo(object,...)
}
)

setGeneric('eSearch',function(object,...) {
  standardGeneric('eSearch')})

####################################
### eUtilsResult methods
####################################
setGeneric('params',function(object,...) {
  standardGeneric('params')})
setMethod('params','eUtilsResult',function(object,...) {
  return(object@queryparams)})

####################################
### eInfoResultDbSpecific methods
####################################
setGeneric('links',function(object,...) {
  standardGeneric('links')})
setMethod('links','eInfoResultDbSpecific',function(object,...) {
  return(object@links)})
setGeneric('fields',function(object,...) {
  standardGeneric('fields')})
setMethod('fields','eInfoResultDbSpecific',function(object,...) {
  return(object@fields)})
setGeneric('description',function(object,...) {
  standardGeneric('description')})
setMethod('description','eInfoResultDbSpecific',function(object,...) {
  return(object@description)})
setGeneric('count',function(object,...) {
  standardGeneric('count')})
setMethod('count','eInfoResultDbSpecific',function(object,...) {
  return(object@count)})
setGeneric('db',function(object,...) {
  standardGeneric('db')})
setMethod('db','eInfoResultDbSpecific',function(object,...) {
  return(object@db)})
setGeneric('lastupdate',function(object,...) {
  standardGeneric('lastupdate')})
setMethod('lastupdate','eInfoResultDbSpecific',function(object,...) {
  return(as.POSIXct(object@lastupdate))})
.showeInfoResultDbSpecific <- function(object,...) {
  cat(sprintf('An object of class "%s"\n',class(object)))
  cat(sprintf("db: %s\n",db(object)))
  cat(sprintf("description: %s\n",description(object)))
  cat(sprintf("fields: %d fields available\n",nrow(fields(object))))
  cat(sprintf("links: %d links available\n",nrow(links(object))))
  cat(sprintf("lastupdate: %s\n",as.character(lastUpdate(object))))
}
setMethod('summary','eInfoResultDbSpecific',function(object,...) {
  .showeInfoResultDbSpecific(object,...)
})
setMethod('show','eInfoResultDbSpecific',function(object) {
  .showeInfoResultDbSpecific(object)
})

####################################
### eInfoResultOverview methods
####################################
setGeneric('dbnames',function(object,...) {
  standardGeneric('dbnames')})
setMethod('dbnames','eInfoResultOverview',function(object,...) {
  return(object@dbnames)})

