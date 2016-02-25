simulat <-
function(yy,nci,r1,r2,p=0,q=0, A=0){
	# this function is used to create two sets of simulation data
	# yy is real data
	# r is replicate number
	# p is proportion of genes differentially expressed in m genes
	# q is proportion of genes artifying noise
	# A is effect value
	# row number =m,
	# column number =cn
	# nci: culumn number of information of data
m<-nrow(yy)
cn<-ncol(yy)
#xxa<-yy[,(cn+1-6):(cn-3)]
#xxb<-yy[,(cn+1-3):cn]
xxc<-yy[,1:nci]

xxa<-yy[,(nci+1):(nci+r1+r2)]
#xxb<-yy[,(n-2*r+1):n]

r<-max(r1,r2)
colsm1<-rep(0,r)
#colsm2<-rep(0,r)


for(i in 1:r){
	
    for(j in 2:m){
	colsm1[i]<-colsm1[i]+xxa[j,i]
#	colsm2[i]<-colsm2[i]+xxa[j,r+i]	
	}
}



v<-matrix(0,nrow=m,ncol=3)

pp<-rep(1,m)

sv<-rep(0,m)

sm<-rep(0,m)

y1<-matrix(0,nrow=m,ncol=r)
y2<-matrix(0,nrow=m,ncol=r)
sn<-rep(0,m)

# condition effect and label of tag expression
effect<-rep(1,m)
label<-rep("-",m)
Prob<-rep(0,m)

for(i in 1:m){
	if(runif(1)<p){
		effect[i]<-runif(1)*A
		label[i]<-"+"
		
		}
	}

	sm<-apply(xxa,1,mean) # 1 for row, 2 for column

#calculate and choose variances within a condition
for(i in 1:m){
	    
#	    v[i,1]<-(xxa[i,1]-(xxa[i,1]+xxa[i,2])/2)^2+(xxa[i,2]-(xxa[i,1]+xxa[i,2])/2)^2
#	    v[i,2]<-(xxa[i,1]-(xxa[i,1]+xxa[i,3])/2)^2+(xxa[i,3]-(xxa[i,1]+xxa[i,3])/2)^2
#	    v[i,3]<-(xxa[i,2]-(xxa[i,2]+xxa[i,3])/2)^2+(xxa[i,3]-(xxa[i,2]+xxa[i,3])/2)^2
 
#	if((v[i,1]<v[i,2])&(v[i,1]<v[i,3])){
#		p[i]<-1
#			}
#	if((v[i,2]<=v[i,1])&(v[i,2]<=v[i,3])){
#			p[i]<-2
#				}
#	if((v[i,3]<=v[i,1])&(v[i,3]<=v[i,2])){
#			p[i]<-3	
	
#			}		

	
	#q[i]<-which(V[i,1:3]==min(V[i,]))    
   # print(q[i])
      			
	
	Prob[i]<-sm[i]/mean(colsm1)			
		
	
	rn<-runif(1)

	      for(j in 1:r){		
	      	    if(xxa[i,1]==0){
	      	    xxa[i,1]<-1
	      	    }	      	    	
				if(rn<0.5){
				y1[i,j]<-round((rnbinom(1,mu=sm[i],size=xxa[i,1])+effect[i]),digits=0)
		        y2[i,j]<-rnbinom(1,mu=sm[i],size=xxa[i,1])
	        	#y1[i,j]<-round(abs(rnorm(1,sm[i],sv[i]^0.5))*effect[i],digits=0)
	        	#y2[i,j]<-round(abs(rnorm(1,sm[i],sv[i]^0.5)),digit=0)
				} else{		
	           #	y1[i,j]<-round(abs(rnorm(1,sm[i],sv[i]^0.5)),digit=0)
            	#	y2[i,j]<-round(abs(rnorm(1,sm[i],sv[i]^0.5))*effect[i],digit=0)
	            y1[i,j]<-rnbinom(1,mu=sm[i],size=xxa[i,1])
	            y2[i,j]<-round((rnbinom(1,mu=sm[i],size=xxa[i,1])+effect[i]),digits=0)	       
		          }	
		}
	
	
	   # artificial effect
	   art1<-round(runif(1)*r)
	   if (art1==0){
	   	art1=1
	   }
	   
		if(runif(1)<q){
			
		 y1[i,art1]<-y1[i,art1]*2
				
				}
						
		art2<-round(runif(1)*r)	
		if(art2==0){
			art2=1
		}
		if(runif(1)<q){
			
				y2[i,art2]<-y2[i,art2]*2
		
		}
		
 for(j in 1:r){
 	if(is.na(y1[i,j])){
 	y1[i,j]<-1	
 		
 	}
 	if(is.na(y2[i,j])){
 	y2[i,j]<-1	
 	}
 	
 }		
   			
	
	}
	
y1<-y1[,1:r1]
y2<-y2[,1:r2]	
	
y<-cbind(y1,y2)



#SM<-apply(y,1,sum)
#geneid<-rep(NA,m)
#for(i in 1:m){
#	if(SM[i]<10)
#	label[i]<-"-"
#	}
	
geneid<-rep(NA,m)	

for(i in 1:m){
	geneid[i]<-paste(as.character(i),label[i],sep="")
	
	}


xx<-cbind(xxc,y)
colnames(xx)<-colnames(yy)
xx<-cbind(geneid,xx)
return(xx)
}
