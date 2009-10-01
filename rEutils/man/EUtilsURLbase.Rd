\name{EUtilsURLbase}
\alias{EUtilsURLbase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EUtilsURLbase(program = c("eSearch", "eSummary", "eFetch", "ePost", "eLink", "eInfo"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{program}{ ~~Describe \code{program} here~~ }
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
function(program=c('eSearch','eSummary','eFetch',
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
