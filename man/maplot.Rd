\name{maplot}
\alias{maplot}
\title{
MA plot of t-values Against Log Mean 
}
\description{
This function is to display MA plot of t-value against log mean.
}
\usage{
maplot(dat, r1, r2, TT, matitle)
}

\arguments{
  \item{dat}{
object outputted by mbetattest containing data ordered by absolution of t-value and rho (\eqn{\rho}).
}
  \item{r1}{
number of replicate libraries in condition 1.
}
  \item{r2}{
number of replicate libraries in condition 2.
}
  \item{TT}{
a numeric parameter that gives truncate value of t-values.
}
  \item{matitle}{
string for MA plot title.
}
}
\details{
 In MA plot, t-value is in y-axis and log mean in x-axis; Black points gathered nearby zero along log mean are genes without differential expressions or differential splicings while red points scattered out of black points are those of being differentially expressed or differentially spliced.
}
\value{
no return value
}
\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}

\examples{
data(dat) 
maplot(dat=dat,r1=3,r2=3,TT=350,matitle="MA plot")
maplot(dat=dat,r1=3,r2=3,TT=50,matitle="MA plot")

}
\keyword{ maplot }
