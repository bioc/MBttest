oddratio <-
function(XX, na,nb){
	
	
   N=nrow(XX)
	
	n<-na+nb
	XA<-XX[,1:na]
	XB<-XX[,(na+1):n]	
	
	VarA<-apply(XA,1,var)
	VarB<-apply(XB,1,var)
	
	MeanA<-apply(XA,1,mean)
	MeanB<-apply(XB,1,mean)
	
	MeanAB<-(MeanA+MeanB)/2
	#print(MeanAB)
	YY<-(XX-MeanAB)^2
	
	VarAB<-apply(YY, 1,sum)/(na+nb-1)
	
	oddrat<-rep(0,N)
	i<-seq(N)
	#for(i in 1:N){
#		if((VarA[i]*MeanA[i]+VarB[i]*MeanB[i])>1){
		
#			oddrat[i]<-log(1+(MeanAB[i]*VarAB[i]/(VarA[i]*MeanA[i]+VarB[i]*MeanB[i])))
#        }else{
        
        oddrat[i]<-log(1+(1+MeanAB[i]*VarAB[i])/(1+VarA[i]*MeanA[i]+VarB[i]*MeanB[i]))
#        }
#	}

	return(oddrat)
	
	
}
