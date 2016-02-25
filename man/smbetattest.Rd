\name{smbetattest}
\alias{smbetattest}

\title{
Performance of multiple Beta t-test on simulated data
}
\description{
This function is to peform mBeta t-test with rho=1 and omega=1 on simulated data. The result lists differentially expressed genes or isoforms marked by symbol="+" and their rho values. The rho values are used to calculate omega value for performance of mBeta t-tests on the real data.
}
\usage{
smbetattest(X, na, nb, alpha)
}

\arguments{
  \item{X}{
simulated count data with N genes or isoforms.

}
  \item{na}{
number of replicate libraries in condition A.
}
  \item{nb}{
number of replicate libraries in condition B.

}
  \item{alpha}{
statistical probabilistic threshold, default value is 0.05.

}

}
\details{
Before performing mbeta t-test on real data, user needs omega value for the threshold of rho. To determine omega value, user is requred to simulate null data having the same gene or isoform number and the same numbers of replicate libraries in two conditions and then performs mbeta t-test on the simulated null data by setting rho =1 and omega =1. To calculate accurately omega value, user needs such performance on 4-6 simulated null
datasets. Manual provides method for omega calculation. 
}
\value{
Return results from multple beta t-tests on simulated data.

}
\references{
Yuan-De Tan Anita M. Chandler, Arindam Chaudhury, and Joel R. Neilson(2015) A Powerful Statistical Approach for Large-scale Differential Transcription Analysis.\emph{Plos One},10.1371/journal.pone.0123658. 
}
\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}

\seealso{
See Also as \code{\link{mbetattest}}
}
\examples{
data(skjt) 

mysim<-smbetattest(X=skjt[1:500,],na=3,nb=3,alpha=0.05)

}
\keyword{ t-test }
\keyword{ simulation}
