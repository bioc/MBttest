betaparametw <-
function(xn,a,b){
	
	n=length(xn)
	w<-rep(0,n)
	W<-rep(0,n)
	for(i in 1:n){
		w[i]<-(a+b)*xn[i]/(a+b+xn[i])
		}
 for(i in 1:n){
 
 	W[i]<-w[i]/sum(w)
 
 	}
 
# print(W)	
return(W)	
	
	}
