\name{MBttest-package}
\alias{mBetatest-package}
\alias{mBetatest}
\docType{package}
\title{
Multiple Beta t-tests
}
\description{
This package is used to peform multiple beta t-test analyses of real data and gives heatmap of differential expressions of genes or differential splicings and MA plot. The results listing geneid or isoformid, gene name, the other information, t-value, p-value, adjusted p-value, adjusted alpha value, rho, and symb are saved in csv file. 
}
\details{
\tabular{ll}{
Package: \tab MBttest\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-02\cr
License: \tab GPL-3\cr
}

}
\author{
Yuan-De Tan

Maintainer: Yuan-De Tan 
\email{tanyuande@gmail.com}

}
\references{
Baggerly KA, Deng L, Morris JS, Aldaz CM (2003) Differential expression in SAGE: accounting for normal between-library variation. \emph{Bioinformatics}, \bold{19}: 1477-1483.
\\
Yuan-De Tan Anita M. Chandler, Arindam Chaudhury, and Joel R. Neilson(2015) A Powerful Statistical Approach for Large-scale Differential Transcription Analysis.\emph{Plos One},10.1371/journal.pone.0123658. 

}

\keyword{ package }
\seealso{
\code{\link{betaparametab}}, \code{\link{betaparametVP}}, \code{\link{betaparametw}}, \code{\link{betattest}}, \code{\link{mbetattest}}, \code{\link{maplot}}, \code{\link{myheatmap}}, \code{\link{oddratio}},  \code{\link{pratio}}, \code{\link{simulat}}, \code{\link{smbetattest}}, \code{\link{mtprocedure}},  \code{\link{mtpvadjust}}
}
\examples{
data(jkttcell) 	
mbetattest(X=jkttcell[1:500,],na=3,nb=3,W=1,alpha=0.05,file="jurkat_NS_48h_tag_mbetattest.csv")

}
