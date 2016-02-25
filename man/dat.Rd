\name{dat}
\alias{dat}
\docType{data}
\title{
The Transcriptomic data and t-test results.
}
\description{
t-value and rho are results ouputed by mbttest.
}
\usage{data("dat")}
\format{
  A data frame with 13409 observations on the following 16 variables.
  \describe{
    \item{\code{tagid}}{a numeric vector}
    \item{\code{geneid}}{a numeric vector}
    \item{\code{name}}{a string vector}
    \item{\code{chr}}{a string vector}
    \item{\code{strand}}{a character vector}
    \item{\code{pos}}{a numeric vector}
    \item{\code{anno}}{a string vector}
    \item{\code{Jurk.NS.A}}{a numeric vector}
    \item{\code{Jurk.NS.B}}{a numeric vector}
    \item{\code{Jurk.NS.C}}{a numeric vector}
    \item{\code{Jurk.48h.A}}{a numeric vector}
    \item{\code{Jurk.48h.B}}{a numeric vector}
    \item{\code{Jurk.48h.C}}{a numeric vector}
    \item{\code{beta_t}}{a numeric vector}
    \item{\code{rho}}{a numeric vector}
    \item{\code{symb}}{a character vector}
  }
}
\details{
t-values \code{(beta_t)}and means over all replicate libraries in two conditions are used to make \emph{MA plot}. The count data of DE isoforms are selected by symb =\code{"+"} and W(omega) and used to make heatmap using myheatmap function.
}
\value{
	ID, information, count data of RNA reads,\code{t-value} and \code{rho-value}, symbol.
}
\references{
Yuan-De Tan Anita M. Chandler, Arindam Chaudhury, and Joel R. Neilson(2015) A Powerful Statistical Approach for Large-scale Differential Transcription Analysis.\emph{Plos One}. DOI: 10.1371/journal.pone.0123658. 
}

\examples{
data(dat)
## maybe str(dat) ; plot(dat) ...
}
\keyword{datasets}
