\name{myheatmap}
\alias{myheatmap}
\title{
Heatmap 
}
\description{
This function is used to display heatmap of differential expressions of genes or isoforms or differential splicings of genes detected by the multiple beta t-test method in the real data.
}
\usage{
myheatmap(dat, r1, r2, W, colrs, tree, method, rwangle, clangle, maptitle)
}

\arguments{
  \item{dat}{
data outputted by mbeta_ttest, includes information columns, data columns, t-value, rho and symbol columns;
}
  \item{r1}{
numeric argument: number of replicate libraries in condition 1.
}
  \item{r2}{
numeric argument: number of replicate libraries in condition 2
}
  \item{W}{
numeric argument: threshold for choosing genes or isoforms for heatmap. W value can be set to be 0 to any large number. If you set W =0, then the function will select all differentially expressed genes with symb="+". To choose a appropriate W, user needs to refere to rho values in the result file. Default W=1.
}
  \item{colrs}{
heatmap colors. User has 5 options: "redgreen", "greenred", "redblue", "bluered" and
"heat.colors". Default colrs="redgreen".
}
  \item{tree}{
object of heatmap. User has four options: "both" for row and column trees,"row" for only row tree,"column" for only column tree, and "none" for no tree specified. Default tree="both".
}
\item{method}{
method to be chosen to calculate distance between columns or rows. It has four options: "euclidean", "pearson","spearman" and "kendall". The latter three are d=1-cc where cc is correlation coefficients. Default="euclidean". 	
}
  \item{rwangle}{
angle of xlab under heatmap. Default value is 30. 
}
  \item{clangle}{
angle of ylab. Default value is 30
}
  \item{maptitle}{
string for heatmap title.
}
}
\details{
This function uses W (omega) and symb to choose genes or isoforms in the data ordered by t-values and then to normalize the selected data by using z-scale. This function has multiple options to select map color, distance, cluster and x- and y-lab angles. The heatmap was designed for publication and presentation, that is, zoom of the figure can be reduced without impacting solution.  
}                                              
\value{
no return value but create a heatmap.
}

\author{
Yuan-De Tan
\email{tanyuande@gmail.com}
}
\note{
myheatmap requres gplots
}

\seealso{
       \code{\link[gplots]{heatmap.2}}
}
\examples{
require(gplots)
data(dat) 

#dat<-mbetattest(X=jkttcell,na=3,nb=3,W=1,alpha=0.05,
#file="C:/mBeta_ttest/R_package/jurkat_NS_48h_tag_mbetattest.csv")

# data(mtcars)
#x  <-as.matrix(mtcars)
#myheatmap(dat=x,r1=3,r2=3, maptitle="mtcars_heatmap")
 
myheatmap(dat=dat,r1=3,r2=3,maptitle="Jurkat T-cell heatmap2")

myheatmap(dat=dat,r1=3,r2=3,tree="none",maptitle="Jurkat T-cell heatmap")

}
\keyword{heatmap}
