# MBttest

MBttest, also called mBetat test, uses new beta t-test method to identify differential expression for each gene or RNA isoform. This approach introduces a gene- or isoform-specific variable, called rho, into t-statistic based on beta distribution. It outperforms the existing statistical methods for identifying differential expressions of genes or isoforms either by inflating t-values with rho > omega (a null rho) or by shrinking those with rho < omega  when number of replicate libraries in each condition is smaller.


## Installation
You can install MBttest from GitHub using devtools in R Console or Rstudio:

`library(devtools)`

`install_github("yuande/MBttest")`

or

`source("http://bioconductor.org/biocLite.R")`

`biocLite("MBttest", dependencies = TRUE)l`

`library(MBttest)`

## Data preparation
When RNA-seq data have been produced from RNA sequecing experiments, user should first perform pipeline analysis of the RNA sequence read data and map the short RNA sequences to a reference genome.  Currently many pipeline tools such as BWA, Bowtie2, tophat2, star and galaxy can be used to map and annotate RNA sequences on a reference genome. The pipeline analysis generates count matrix.  The count matrix contains two parts:  Annotation information and count data. Information may contain  tagid, geneid, gene name, chromosome, DNA strand, etc columns, depending on a pipeline tool that user used. Information columns are in the  left side of the matrix. It has at least one column for geneid or tagid (isoformid). The count data contain two conditions each having several replicate libraries and must be in the right side.  Here is an example:

`data(jkttcell)`

`jkttcell[1:10,]`

or using head to display the data jkttcell:

`head(jkttcell)`

jkttcell is matrix count data generated from RNA sequences due to differential polyadenylation in  Jurkat T-cell betweem resting and stimulating statuses using BWA.  Data jkttcell contains 7 columns for information of poly(A) sites in the left side and 10 columns for count data.

## Simulation for calculation of omega
Before performing *mbetattest* on the real data, user needs simulation to determine $$\omega$$ value. There are three steps for doing so:

Step1: Use the following function to generate null simulation data: 

`sjknull<-simulat(yy=jkttcell, nci=7, r1=3, r2=3, q=0.2)`

where yy is real data. r1 and r2 are replicate numbers in conditions 1 and 2. q is proportion of genes artificial noise.  nci = column number of information of data.

Step2: Perform multiple beta t-tests on simulated null data sjknull:

`mysim<-smbetattest(X=sjknull, na=3, nb=3, alpha=0.05)`

Figure1 in vignettes fold shows the results. In symbol column, *mbetattest* gives test result:  "-" means that a gene or a tag or isoform is not chosen, while "+"  indicates that the gene or isoform is found to be differentially expressed between two statuses (see Figure2 in vignettes folder. In this example, 12 genes would be detected to be falsely positive. 

Step3:  Calculate omega:
Here is a demo for calculating omega (since we can not use greek letter omega in R function, we use W to represent omega. In Figure 3 in vignettes, red highlighted column is rho column. We copied the rho values of these 12 genes into another empty column and sorted them from the smallest to the largest. Then we gave sequence number (k) from 1 to 12 corresponding to rho-values  and calculate q=k/12 for each ordered rho value. Then we chose rho with q >=
 0.85. Repeat this process for about 5 times (depends on number of genes or isofroms in the real data that user uses) and give average the rho values as W.


## Normalize the count data
As a second processing step, we need to estimate the effective library size. This step is also called "normalization of data", even though it may not make the count data be of normal distribution.  If the counts of expressed genes in one condition are, on average, twice as high as in another (because the library was sequenced twice as deeply), the size factor for the first condition should be twice higher than the second one, then differential analysis would give error results. For this reason, we must make all libraries have the same size before performing any statistical method. For doing so, user can use the following simple method to normalize the the count data: In excel sheet, use function sum to calculate sizes of all libraries, and then use excel function average to calculate averaged library size. The last step is to use the following equation to convert the original count data to new count data with the same library size: 
   
xij = yij*bar(N)/Nj

where i = 2, ..., n(number of genes or isofoms) in rows in a sheet, j = 1, ..., c where c=na+nb; Nj is size of library j  and  bar(N) is mean of sizes over all libraries; yij is the original count of **RNA** reads in row i and column j.

## Perform multiple beta t-tests on the real data

Suppose the data have been normalized so that all libraries have the same size.  After obtaining W value, user can use the function and the real data to perform MBttest:

`res<-mbetattest(X=jkttcell, na=3, nb=3, W=1, alpha=0.05, file= "jurkat_result.csv")`

Function *mbetattest* has two output results: one is saved in  .csv file and the other is res  for making heatmap. Function *myheatmap* has multiple options: both-side, row and column cluster trees with distance methods: "euclidean", "pearson", "spearman", and "kendall" correlation coefficients and color label with "redgreen", "greenred", "redblue", "bluered" or "heat.colors" and angles for genes or isoforms in row and cases (conditions) in column. User can use default without any choice, like

`myheatmap(dat=res, r1=3,r2=3,maptitle="Jurkat T-cell heatmap")`

## Author
Yuande Tan
tanyuande@gmail.com
