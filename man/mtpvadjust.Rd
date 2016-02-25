\name{mtpvadjust}
\alias{mtpvadjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
P-value Adjustment for Multiple Comparisons
}
\description{
Given a set of N p-values, it returns a set of N p-values adjusted by choosing C-value
}
\usage{
mtpvadjust(pv, C)
}

\arguments{
  \item{pv}{
numeric vector of p-values. 
}
  \item{C}{
numeric constant, the value can be taken from any number > 0 or equal to 0. C is used to choose multiple-test procedure.
}
}
\details{
This is a multiple-test procedure family including Benjamini-Hochberg procedure, Bonferroni procedure and single-test procedure. By choosing C-value, it can generat a multiple-test procedure for controling the false discovery rate,the expected proportion of false discoveries amongst the rejected hypotheses. Benjamini-Hochberg procedure is given with C=1.22, Bonferroni procedure is given with C = N and single-test procedure can be given with C=0. 
}
\value{
return a list of adjusted p-values.
}
\references{
Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. \emph{Journal of the Royal Statistical Society Series B} \bold{57}, 289-300. 
\\
Yuan-De Tan and Hongyan Xu A general method for accurate estimation of false discovery rates in identification of differentially expressed genes. \emph{Bioinformatics} (2014) \bold{30} (14): 2018-2025. doi: 10.1093/bioinformatics/btu124.
}

\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}
\note{
p-value must be ordered from the largest value to the smallest value before executing tan_pvadjust.
}

\seealso{
   \code{\link[stats]{p.adjust}}
}
\examples{
set.seed(123)
x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)))
p <- 2*pnorm(sort(-abs(x)))
round(mtpvadjust(pv=p, C=1.22),4)
# [1] 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#[11] 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 0.6875 0.6174 0.4588
#[21] 0.4115 0.3644 0.2216 0.1554 0.1443 0.1249 0.1027 0.0964 0.0763 0.0319
#[31] 0.0166 0.0135 0.0123 0.0096 0.0091 0.0068 0.0045 0.0041 0.0020 0.0007
#[41] 0.0004 0.0003 0.0002 0.0001 0.0001 0.0001 0.0001 0.0000 0.0000 0.0000

}

\keyword{ multple test procedure }
\keyword{ p-value adjustment}
