betaparametab <-
function(xn,w,P,V){

n<-length(xn)
W2<-0
for(i in 1:n){
	W2<-W2+w[i]^2
	
}
W2X<-0
for(i in 1:n){
 	W2X<-W2X+w[i]^2/xn
	
}

#get beta(b) and alpha(a)

#b<-(P*(1-P)*W2-V)/((V/(1-P))-P*W2X)
#a<-b*P/(1-P)
#print(c(P,V))

if(P==0||V==0){
	b<-0
	}else{
		if((V/(1-P)-P*sum(w^2/xn))==0){
	b<-(P*(1-P)*sum(w^2)-V)/(V/(1-P)-(P*sum(w^2/xn)-0.001))
	  } else{	  	
	  b<-(P*(1-P)*sum(w^2)-V)/(V/(1-P)-P*sum(w^2/xn))
	  	
	  	}
	}
    if(P==1){
   	a<-1
   	}else{
	a<-b*P/(1-P)
	}
#	print(b)
 c<-c(a,b)	
# print(V)
 
 return(c)
	}
