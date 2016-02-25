\name{betaparametw}
\alias{betaparametw}
\title{
Estimation of Weights of Proportions
}
\description{
Function betaparametw is used to calculate weight.
}
\usage{
betaparametw(xn, a, b)
}
\arguments{
  \item{xn}{
vector of m library sizes. Library size is sum of counts over the whole library. 
}
  \item{a}{
parameter alpha in beta distribution derived from output of function Bag_parametab
}
  \item{b}{
parameter beta in beta distribution derived from output of function Bag_parametab
}
}
\details{
alpha and beta are used to calculate weight. Then weight is in turn used to correct bias of estimation of alpha and beta in Bag_parametab function. 
}
\value{
return weight(W)
}
\references{
Baggerly KA, Deng L, Morris JS, Aldaz CM (2003) Differential expression in SAGE: accounting for normal between-library variation. \emph{Bioinformatics}, \bold{19}: 1477-1483.
\\
Yuan-De Tan, Anita M. Chandler, Arindam Chaudhury, and Joel R. Neilson(2015) A Powerful Statistical Approach for Large-scale Differential Transcription Analysis.\emph{Plos One}. 2015 DOI: 10.1371/journal.pone.0123658. 
}
\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}

\seealso{
 \code{\link{betaparametab}},\code{\link{betaparametVP}}.
}
\examples{
XX<-c(2000,2000,2000)
a<-1.1458
b<-6.4932
betaparametw(xn=XX,a=a,b=b)
#[1] 0.3333333 0.3333333 0.3333333
}
\keyword{ weight}
