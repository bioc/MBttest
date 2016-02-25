smbetattest <-
function(X,na,nb,alpha=0.05){
	cn<-length(X[1,])
	rn<-length(X[,1])
	XC<-X[,1:(cn-na-nb)]
	XX<-X[,(cn-na-nb+1):cn]
	pvalue<-rep(1,rn)
	betattest<-betattest(XX,na,nb)
	prat<-pratio(xx=XX,na=na,nb=nb)
	odrat<-oddratio(XX=XX,na=na,nb=nb)
	t_value<-betattest[,1]
    C=0.01
	rho<-(prat*odrat)^0.5
	beta_t<-t_value#*rho
	
	#print(cbind(newt,polarratio))
	df<-betattest[,2]
	for(i in 1:rn){
		if(!(is.na(beta_t[i]))){
			if(abs(beta_t[i])>0){
			pvalue[i]<-2*(1-pt(abs(beta_t[i]),df=df[i]))
			}
			}

		}

#	log2Pt<-log2(Pt)	
	XD<-cbind(XC,beta_t,pvalue,rho)
	
	XD<-XD[order(abs(beta_t)),]
	XD<-XD[,1:(cn-na-nb+3)]
	
	p_value<-XD$pvalue
   # print(p_value)
	adjalpha<-mtprocedure(alpha,rn,C)	
	paj<-mtpvadjust(p_value,C)
	
	symb<-rep("+",rn)
	
	for(i in 1:rn){
		if(p_value[i]>adjalpha[i]){
			symb[i]<-"-"
			}
		}
		
	XD<-cbind(XD,paj,adjalpha,symb)
	return(XD)

	
	}
