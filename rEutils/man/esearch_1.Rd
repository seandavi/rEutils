\name{ESearch}
\alias{ESearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ESearch(db = "pubmed", term, usehistory = c("y", "n"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{ ~~Describe \code{db} here~~ }
  \item{term}{ ~~Describe \code{term} here~~ }
  \item{usehistory}{ ~~Describe \code{usehistory} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(db='pubmed',term,usehistory=c('y','n'),...) {
  params <- list(db=db,term=term,usehistory=usehistory,...)
  querystring <- EUtilsQueryString(params)
  urlbase <- EUtilsURLbase('eSearch')
  url <- paste(urlbase,querystring,sep="")
  xmlrawresult <- xmlRoot(xmlTreeParse(getURL(url)))
  if("ERROR" \%in\% names(xmlrawresult)) {
    errorval <- xmlValue(xmlrawresult[['ERROR']])
    warning(sprintf('Query [\%s]\n generated ERROR: \%s',querystring,errorval))
    result <- new('ESearchResult',error=errorval,
                  db=db,
                  queryparams=params,
                  xmlresult=xmlrawresult)
    return(result)
  }
  result <- new('ESearchResult',
                db=db,
                queryparams=params,
                xmlresult=xmlrawresult,
                webenv=ifelse(is.null(xmlrawresult[['WebEnv']]),NULL,xmlValue(xmlrawresult[['WebEnv']])),
                querykey=ifelse(is.null(xmlrawresult[['QueryKey']]),NULL,xmlValue(xmlrawresult[['QueryKey']])),
                queryurl=url,
                retstart=as.integer(xmlValue(xmlrawresult[['RetStart']])),
                retmax=as.integer(xmlValue(xmlrawresult[['RetMax']])),
                count=as.integer(xmlValue(xmlrawresult[['Count']])),
                querytranslation=xmlValue(xmlrawresult[['QueryTranslation']]))
  return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
