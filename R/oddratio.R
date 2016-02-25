oddratio <-
function(XX, na,nb){
	
	
   N=nrow(XX)
	
	n<-na+nb
	XA<-XX[,1:na]
	XB<-XX[,(na+1):n]	
	
	VarA<-apply(XA,1,var)
	VarB<-apply(XB,1,var)
	VarAB<-apply(XX,1,var)
	MeanA<-apply(XA,1,mean)
	MeanB<-apply(XB,1,mean)
	MeanAB<-apply(XX,1,mean)
	oddrat<-rep(0,N)
	for(i in 1:N){
		if((VarA[i]*MeanA[i]+VarB[i]*MeanB[i])>1){
		
			oddrat[i]<-log(1+(MeanAB[i]*VarAB[i]/(VarA[i]*MeanA[i]+VarB[i]*MeanB[i])))
			}
	}
	return(oddrat)
	
	
}
