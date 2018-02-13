mbetattest <-
function(X,na,nb,W,alpha=0.05,file){
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
    #beta_t<-rho<-rep(0,rn)
  #  for(i in 1:rn){
  #  rho[i]<-sqrt(prat[i]*odrat[i])	
  #  beta_t[i]<-t_value[i]*sqrt(prat[i]*odrat[i])/W	
  #  }
	rho<-sqrt(prat*odrat)/W
	beta_t<-t_value*rho
	#print(cbind(prat,odrat,rho))
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
  dat<-cbind(X, beta_t,rho)
	
	XD<-XD[order(abs(beta_t)),]
	XD<-XD[,1:(cn-na-nb+3)]
  dat<-dat[order(abs(beta_t)),]
 	
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
  dat<-cbind(dat,symb)
  if(!(is.null(file))){
	write.csv(XD,file)
	}
   return(dat)
	
	}
