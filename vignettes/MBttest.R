### R code from vignette source 'MBttest.Rnw'

###################################################
### code chunk number 1: <style-Sweave
###################################################
BiocStyle::latex()


###################################################
### code chunk number 2: MBttest.Rnw:44-46
###################################################
set.seed(102)
options(width = 90)


###################################################
### code chunk number 3: MBttest.Rnw:49-50
###################################################
library(MBttest)


###################################################
### code chunk number 4: MBttest.Rnw:55-57
###################################################
data(jkttcell)
jkttcell[1:10,]


###################################################
### code chunk number 5: MBttest.Rnw:62-63
###################################################
head(jkttcell)


###################################################
### code chunk number 6: MBttest.Rnw:86-87
###################################################
sjknull1<-simulat(yy=jkttcell[1:500,],nci=7,r1=3,r2=3,q=0.2)


###################################################
### code chunk number 7: MBttest.Rnw:100-101
###################################################
mysim1<-smbetattest(X=sjknull1,na=3,nb=3,alpha=0.05)


###################################################
### code chunk number 8: MBttest.Rnw:160-161
###################################################
res<-mbetattest(X=jkttcell[1:1000,],na=3,nb=3,W=1,alpha=0.05,
                           file="jurkat_NS_48h_tag_mbetattest.csv")


###################################################
### code chunk number 9: MBttest.Rnw:165-167
###################################################
data(dat)
head(dat)


###################################################
### code chunk number 10: MAplot
###################################################
maplot(dat=dat,r1=3,r2=3,TT=350,matitle="MA plot")


###################################################
### code chunk number 11: figMAplot
###################################################
maplot(dat=dat,r1=3,r2=3,TT=350,matitle="MA plot")


###################################################
### code chunk number 12: MAplot50
###################################################
maplot(dat=dat,r1=3,r2=3,TT=50,matitle="MA plot")


###################################################
### code chunk number 13: figMAplot50
###################################################
maplot(dat=dat,r1=3,r2=3,TT=50,matitle="MA plot")


###################################################
### code chunk number 14: heatmapwithtree
###################################################
myheatmap(dat=dat,r1=3,r2=3,maptitle="Jurkat T-cell heatmap2")


###################################################
### code chunk number 15: figheatmapwithtree
###################################################
myheatmap(dat=dat,r1=3,r2=3,maptitle="Jurkat T-cell heatmap2")


###################################################
### code chunk number 16: heatmapwithouttree
###################################################
myheatmap(dat=dat,r1=3,r2=3,tree="none",maptitle="Jurkat T-cell heatmap3")


###################################################
### code chunk number 17: figheatmapwithouttree
###################################################
myheatmap(dat=dat,r1=3,r2=3,tree="none",maptitle="Jurkat T-cell heatmap3")


###################################################
### code chunk number 18: heatmapwithcoltree
###################################################
myheatmap(dat=dat,r1=3,r2=3,colrs="redblue", tree="column",
method="pearson", maptitle="Jurkat T-cell heatmap")


###################################################
### code chunk number 19: figheatmapwithcoltree
###################################################
myheatmap(dat=dat,r1=3,r2=3,colrs="redblue", tree="column",
method="pearson", maptitle="Jurkat T-cell heatmap")


###################################################
### code chunk number 20: MBttest.Rnw:249-250
###################################################
sessionInfo()


