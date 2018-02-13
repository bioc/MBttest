\name{mtprocedure}
\alias{mtprocedure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple-Test Procedures
}
\description{
Similiar to Benjamini-Hochberg multiple-test procedure, alpha is adjusted to be a set of values.
}
\usage{
mtprocedure(alpha, N, C)
}

\arguments{
  \item{alpha}{
probabilistic threshold and is usually set to be 0.05 or 0.01. Default value is 0.05
}
  \item{N}{
numeric constant, number of genes to be detected in transcriptome.
}
  \item{C}{
numeric constant, it can be taken from 0 to N. C is used to choose multiple-test procedure. Default value is 0.01. This procedure is single test with C=0, Benjamini-Hochberg procedure with C=1.22 and Bonfroni procedure with C=N. 
}
}
\details{
This is a multiple-test procedure family including Benjamini-Hochberg procedure, Bonferroni procedure and single-test procedure. By choosing C-value, it can generat a multiple-test procedure for controling the false discovery rate, the expected proportion of false discoveries amongst the rejected hypotheses. 
}
\value{
return a list of adjusted alpha values.
}
\references{
Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. \emph{Journal of the Royal Statistical Society Series B} \bold{57}, 289-300. 
\cr
Yuan-De Tan and Hongyan Xu A general method for accurate estimation of false discovery rates in identification of differentially expressed genes. \emph{Bioinformatics} (2014) \bold{30} (14): 2018-2025. doi: 10.1093/bioinformatics/btu124.
}
\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}

\seealso{
         \code{\link[stats]{p.adjust}}
}
\examples{
mtprocedure(alpha=0.5,N=200,C=1.22)
#  [1] 0.007501404 0.011906423 0.015914688 0.019682621 0.023284917 0.026763656
#  [7] 0.030145311 0.033447843 0.036684127 0.039863779 0.042994217 0.046081313
# .....
#[175] 0.444073506 0.446322519 0.448570478 0.450817390 0.453063265 0.455308110
#[181] 0.457551933 0.459794741 0.462036542 0.464277343 0.466517153 0.468755977
#[187] 0.470993825 0.473230701 0.475466614 0.477701571 0.479935578 0.482168642
#[193] 0.484400770 0.486631969 0.488862244 0.491091603 0.493320052 0.495547597
#[199] 0.497774244 0.500000000
}

\keyword{ multiple test procedure}
\keyword{alpha adjustment}

