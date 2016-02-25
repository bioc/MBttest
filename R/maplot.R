maplot <-
function(dat, r1,r2,TT,matitle){ 

xx<-dat[,(ncol(dat)-r1-r2-2):(ncol(dat)-3)]
tvalue<-dat[,(ncol(dat)-2)]
tv<-tvalue
meanx<-apply(xx,1,mean)

symb<-dat$symb

dat1<-subset(dat,symb=="+")

nc<-ncol(dat1)
nr<-nrow(dat1)
Tvalue<-dat1[,(nc-2)]


yy<-dat1[,(nc-r1-r2-2):(nc-3)]
meany<-apply(yy,1,mean)

for(i in 1:length(tvalue)){
 if(abs(tvalue[i])>=TT){
   tv[i]<-tvalue[i]/10
    }
}

TV<-Tvalue
for(i in 1:nr){
if(abs(Tvalue[i])>=TT){
TV[i]<-Tvalue[i]/10}
}



plot(log(meanx),tv,pch=".", xlab="log(mean)", ylab="t-value", cex.axis=1.8,
cex.lab=1.8, main=matitle,cex.main=2)
points(log(meany),TV,pch=".",col="red")

}
