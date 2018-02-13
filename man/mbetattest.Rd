\name{mbetattest}
\alias{mbetattest}
\title{
Performance of multiple beta t-test on simulated data
}
\description{
This function is to peform multiple beta t-test method on real data. The result lists geneid or isoformid, gene name, the other information, t-value, p-value, adjusted p-value, adjusted alpha value, rho (\eqn{\rho}), and symb. All these lists are ordered by absolution of t-values.
}
\usage{
mbetattest(X, na, nb, W, alpha=0.05, file)
}
\arguments{
  \item{X}{
count data of RNA reads with na replicates in condition A ans nb replicates in condition B.
}
  \item{na}{
number of replicate libraries in condition A.
}
  \item{nb}{
number of replicate libraries in condition B.
}
  \item{W}{
numeric parameter, called omega (\eqn{\omega}) that is a constant determined by null simulation.
}
  \item{alpha}{
the probabilistic threshold. User can set alpha (\eqn{\alpha})= 0.05 or 0.01 or the other values. Defalt value is 0.05
}
  \item{file}{
a csv file. User needs to give file name and specify direction path. But if user uses setwd function, drive is not necessarily specified in file.
}
}
\details{
 t-statistic is defined as t-statistic multiplied by (rho/omega), that is, 
 \deqn{T=t\times \frac{\rho}{\omega}}
 where 
\deqn{t=\frac{(P_A-P_B)}{\sqrt(V_A+V_B)}}
\deqn{\rho=\sqrt{\psi \zeta}}
 where
\deqn{\psi =max(\frac{min(X_A)}{max(X_B)+1},\frac{min(X_B)}{max(X_A)+1})}
\deqn{\zeta=log(1+\frac{\bar{X}\sigma^2+1}{\bar{X}_A\sigma^2_A+\bar{X}_B\sigma^2_B+1})}
 \eqn{\omega} is a constant as threshold estimated from null data. 
}
\value{
return a dat list: the data ordered by abs(t) contain information cloumns, data columns, t-values, rho and symb that are used to make heatmap and \emph{MAplot}. 
}
\references{
Yuan-De Tan Anita M. Chandler, Arindam Chaudhury, and Joel R. Neilson(2015) A Powerful Statistical Approach for Large-scale Differential Transcription Analysis. \emph{Plos One}, 10.1371/journal.pone.0123658.  
}
\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}
\seealso{
 \code{\link{smbetattest}}.
}
\examples{
data(jkttcell) 

dat<-mbetattest(X=jkttcell[1:1000,],na=3,nb=3,W=1,alpha=0.05,file="jurkat_NS_48h_tag_mbetattest.csv")
}
\keyword{ multiple }
\keyword{ t-tests }