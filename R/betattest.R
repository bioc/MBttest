betattest <-
function(X,na,nb){
	N=length(X[,1])
	n<-na+nb
	XA<-X[,1:na]
	XB<-X[,(na+1):n]
	
	SA<-apply(XA,2,sum)
	SB<-apply(XB,2,sum)
	# dim=1 for row sum, dim=2 for column sum
	PA<-betaparametVP(XA,SA)
	Pa<-PA[,1]
	Va<-PA[,2]
	PB<-betaparametVP(XB,SB)
	Pb<-PB[,1]
	Vb<-PB[,2]
	t<-rep(0,N)
	df<-rep(0,N)
	for(i in 1:N){
		if(Va[i]+Vb[i]>0){
		t[i]<-(Pa[i]-Pb[i])/(Va[i]+Vb[i])^0.5	
			}
		 df[i]<-(Va[i]+Vb[i])^2/(Va[i]^2/(sum(SA)-1)+Vb[i]^2/(sum(SB)-1))
		}
	#print(t)
	ttest<-cbind(t,df)
	return(ttest)
	
	}
