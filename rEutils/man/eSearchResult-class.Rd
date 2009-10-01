\name{eSearchResult-class}
\docType{class}
\alias{eSearchResult-class}
\alias{eFetch,eSearchResult-method}
\alias{eLink,eSearchResult-method}
\alias{eSummary,eSearchResult-method}

\title{Class "eSearchResult" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eSearchResult", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{webenv}:}{Object of class \code{"CharOrNULL"} ~~ }
    \item{\code{query_key}:}{Object of class \code{"CharOrNULL"} ~~ }
    \item{\code{queryurl}:}{Object of class \code{"character"} ~~ }
    \item{\code{idlist}:}{Object of class \code{"vector"} ~~ }
    \item{\code{count}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{retstart}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{retmax}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{querytranslation}:}{Object of class \code{"CharOrNULL"} ~~ }
    \item{\code{xmlresult}:}{Object of class \code{"XMLNode"} ~~ }
    \item{\code{queryparams}:}{Object of class \code{"list"} ~~ }
    \item{\code{error}:}{Object of class \code{"CharOrNULL"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{EUtilsXMLResult}"}, directly.
Class \code{"\linkS4class{EUtilsResult}"}, by class "EUtilsXMLResult", distance 2.
}
\section{Methods}{
  \describe{
    \item{eFetch}{\code{signature(object = "eSearchResult")}: ... }
    \item{eLink}{\code{signature(object = "eSearchResult")}: ... }
    \item{eSummary}{\code{signature(object = "eSearchResult")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	or \code{\linkS4class{CLASSNAME}} for links to other classes
}
\examples{
showClass("eSearchResult")
}
\keyword{classes}
