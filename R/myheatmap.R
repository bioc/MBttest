myheatmap <-
function(dat,r1,r2,W=1,colrs="redgreen",tree="both",method="euclidean",rwangle=30,clangle=30,maptitle) {
# import data
#require(gplots)
rho<-dat$rho
symb<-dat$symb
dat1<-subset(dat,rho>=W &symb=="+")
nc<-ncol(dat1)
nr<-nrow(dat1)
dat1<-dat1[order(dat1[,(nc-2)]),]


dat2<-dat1[,(nc-r1-r2-2):(nc-3)]
 
z<-t(apply(dat2,1,scale))

colnames(z)<-colnames(dat2)
	
rc <- redgreen(nrow(z))

if (colrs=="redgreen"){
cc <- redgreen(ncol(z))
pltt<-redgreen
 } else if (colrs=="heat.colors"){
 cc <- heat.colors(ncol(z))
 pltt<-heat.colors
 }else if (colrs=="redblue"){
 cc <- redblue(ncol(z))
 pltt<-redblue
 }else if(colrs=="greenred"){
  cc <- greenred(ncol(z))
  pltt<-greenred
  }else if(colrs=="bluered"){
  cc <- bluered(ncol(z)) 
  pltt<-bluered
   }


if(method=="pearson"){
hr <- hclust(as.dist(1-cor(t(z), method="pearson")), method="complete")
hc <- hclust(as.dist(1-cor(z, method="pearson")), method="complete")
}else if(method=="spearman"){
hr <- hclust(as.dist(1-cor(t(z), method="spearman")), method="complete")
hc <- hclust(as.dist(1-cor(z, method="spearman")), method="complete") 
}else if(method=="kendall"){
hr <- hclust(as.dist(1-cor(t(z), method="kendall")), method="complete")
hc <- hclust(as.dist(1-cor(z, method="kendall")), method="complete") 
}else if(method=="euclidean"){
hr <- hclust(dist(t(z), method="euclidean"))
hc <- hclust(dist(z, method="euclidean"))
}
if(method=="euclidean"){
Rowv=TRUE
Colv=TRUE	
}else{
Rowv=as.dendrogram(hr)
Colv=as.dendrogram(hc)	
	}

#par(mar=c(7,4,4,2)+0.1) 
#png(filename='test.png', width=800, height=750)	
#par(oma=c(1,1,1,1))
heatmap.2(z,Rowv=Rowv, Colv=Colv, breaks=16, col=pltt, trace="none",distfun=function(z){if(method=="euclidean"){
dist(z,method="euclidean")}else if(method=="pearson"){
as.dist(1-cor(t(z),method="pearson"))}else if(method=="spearman"){
as.dist(1-cor(z,method="spearman"))}else if(method=="kendall"){
as.dist(1-cor(z,method="kendall"))}},scale ="none", dendrogram=tree,keysize=2, ColSideColors = cc,ylab="DE genes",main=maptitle, srtRow=rwangle, srtCol=clangle, offsetRow=0, offsetCol=0)

#heatmap.2(dat, srtCol=NULL) 
}
