betaparametVP <-
function(X,NX){
	
#	X<-XX[,4:9]
	#X<-as.numeric(X)
	# row number
	rn=length(X[,1])
	# column number
	cn=length(X[1,])
	# dim=1 for sum of each row
	MX=apply(X,1,sum)
	# dim=2 for sum of each column
		
	# weight
    #	w<-rep(0,cn)
	# wight sequare
    #  W2<-rep(0,cn)
	P<-rep(0,rn) 
	# proportions of sites or genes in cn columns
	
	V<-rep(0,rn)
	VX2<-rep(0,rn)
	# nonbiased variance
	VN<-rep(0,rn)
	# alternative variance
	VA<-rep(0,rn)
	# alpha and beta matrix
	#ab<-matrix(0,nrow=rn,2)
    #AB<-array(NA,c(rn,100,cn))
	# alpha in k<=100 iterations
	AA<-matrix(0,nrow=rn,ncol=100)
	# beta in k<=100 iterations
	BB<-matrix(0,nrow=rn,ncol=100)
	VVX2<-matrix(0,nrow=rn,ncol=100)
	
	
	 # weight in k<=100 iterations
	WW<-array(0,c(rn,100,cn))
 	WW2<-array(0,c(rn,100,cn))	
 	VV2<-array(0,c(rn,100,cn))	# sum of weight square accross cn columns (replicates) 
	SWW2<-matrix(0,nrow=rn,ncol=100)
	SW<-rep(0,rn)
	#Poportion of sites or genes in k<=100 iteration
	PP<-matrix(0,nrow=rn,ncol=100)
	PW<-array(0,c(rn,100,cn))
	VV<-matrix(0,nrow=rn,ncol=100)
	D<-matrix(NA,nrow=rn,ncol=100)
	Q<-matrix(0,nrow=rn,ncol=cn)
	
      # for(j in 1:cn){
           w<-NX[1:cn]/sum(NX)
     #   	}
		
       # 	for(j in 1:cn){
            W2<-w[1:cn]^2
        #    	}
	
	for(i in 1:rn){
        #	print(i)
		
		if(MX[i]>0){
	#	 if(MX[i]==0){
	#	 	P[i]<-0
	#	 	V[i]<-0
	#	} 	
	#	else{
		 	
			for(j in 1:cn){
				
				Q[i,j]<-X[i,j]/NX[j]
            }
			  for(j in 1:cn){
				P[i]<-P[i]+w[j]*Q[i,j]
				}	 
              #  print(Q[i,1:cn])
			 	for(j in 1:cn){
			 		VX2[i]<-VX2[i]+(w[j]*Q[i,j])^2
			 		}
			 	
				V[i]<-(V[i]-sum(W2)*P[i])^2/(1-sum(W2))
            
			
			#	if(is.na(v[i])){v[i]<-0}
            #   print(c(P[i],V[i],VX2[i]))
	    		ab<-betaparametab(NX,w,P[i],V[i])
				# initialized alpha		
				AA[i,1]<-ab[1]
				# initialized beta
				BB[i,1]<-ab[2]
			
				# initialized 					
		    	VVX2[i,1]<-VX2[i]
		    	# initialized weight square
				WW2[i,1,1:cn]<-W2[1:cn]
				# initialized weight
				WW[i,1,1:cn]<-w[1:cn]
				# initialized 
				PP[i,1]<-P[i]
			
				VV[i,1]<-V[i]
			#	AB[i,1,]<-ab[i,]
			    D[i,1]<-10
				# Use while roop to find estimation of weight, Proportion, and variance
			    k=1
				while(D[i,k]>0.01){
				 k<-k+1
			#	print(k)	
				 AB<-betaparametab(xn=NX,w=WW[i,(k-1),],P=PP[i,(k-1)],V=VV[i,(k-1)])
				 AA[i,k]<-AB[1]
				 BB[i,k]<-AB[2]
				
				 EW<-betaparametw(xn=NX,a=AA[i,k],b=BB[i,k])
		
             # for (j in 1:cn){
             WW[i,k,1:cn]<-EW
				
                #	 }
#				 print(c(WW[i,k,1],WW[i,k,2],WW[i,k,3]))				 
				 
                 for(j in 1:cn){
                   PP[i,k]<-PP[i,k]+WW[i,k,j]*Q[i,j]
                 SWW2[i,k]<-SWW2[i,k]+WW[i,k,j]^2
                 VVX2[i,k]<-VVX2[i,k]+(WW[i,k,j]*Q[i,j])^2
			    	
                     	 }
		 			     
				for(j in 1:cn){
					VV[i,k]<-VV[i,k]+(WW[i,k,j]*Q[i,j]-PP[i,k]/cn)^2/(1-SWW2[i,k])
					}
						
					if(k<100){
						D[i,k]<-abs(BB[i,k-1]-BB[i,k])
						}else{
						D[i,k]<-0
                         }
					}
					# unbiased variance
					VN[i]<-VV[i,k]
					SW[i]<-SWW2[i,k]				
					P[i]<-PP[i,k]	
					# alternative variance
					meanNX<-mean(NX)
					VA[i]<-((1+MX[i])/meanNX)*(1-(1+MX[i])/meanNX)/meanNX
                    #VA[i]<-(MX[i]/sum(NX))*(1-MX[i]/sum(NX))/sum(NX)
					V[i]<-max(VN[i],VA[i])
	    			
				
                
	    	     	}
	    						
				}
      
	  paramet<-cbind(P,V)

	return(paramet)
	}
