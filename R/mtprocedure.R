mtprocedure <-
function(alpha=0.05,N,C){
	q<-rep(1,N)
	for(i in 1:N){
		q[i]<-i/N
		}
	S<-1	
	SS<-rep(1,N)
	for(k in 2:N){
		if(q[k]==0&q[k-1]==0){
		S<-S+1	
			}else{
				S<-S+(q[k]-q[k-1])/(q[k]+q[k-1])
				
				}
				for(i in 2:k){
					if(q[i]==0&q[i-1]==0){
						SS[k]<-SS[k-1]+1
						}else{
							SS[k]<-SS[k-1]+(q[i]-q[i-1])/(q[i]+q[i-1])
							}
					}
		       
		}
	 R<-rep(1,N)	
	 adjalpha<-rep(1,N)
		for(k in 1:N){
			R[k]<-(SS[k]/S)^C
			
			}
	for(k in 1:N){
		adjalpha[k]<-alpha*(N)^(R[k])/N
		}	
		
	return(adjalpha)	
		
	}
