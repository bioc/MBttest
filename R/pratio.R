pratio <-
function(xx,na,nb){

xx1<-abs(xx[,1:na])
xx2<-abs(xx[,(na+1):(na+nb)])	

XM<-apply(xx1,1,max)
Xm<-apply(xx1,1,min)
YM<-apply(xx2,1,max)
Ym<-apply(xx2,1,min)	

Ng<-nrow(xx)

pratio<-rep(0,Ng)	

for(i in 1:Ng){
	pratio[i]<-max(Ym[i]/(XM[i]+1),Xm[i]/(YM[i]+1))
	
}	
	
return(pratio)	
	
}
