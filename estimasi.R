#MENGESTIMASI PARAMETER MODEL
data=read.csv("d://TA/Skripsi 2/File Kerja/data.txt")
Y=as.matrix(data[,1])
X1=as.matrix(data[,3])
X2=as.matrix(data[,4])
X3=as.matrix(data[,5])

estimasi<-function(Y, X1, X2, X3, kOptimal)
{
	n<-length(Y)
	p=ncol(data)-2
	k<-kOptimal
	q<-(p*(k+1))+1
	C<-matrix(0,n,q)
	hasil<-matrix(0,k,2)
	error<-rep(0,n)
cat("=============================================================")
	cat("\nx\ty\t\tytopi\t\terror") 
cat("\n=============================================================")
	for(i in 1:n)
	{
		for(r in 1:k)
		{
			C[i,1]=1
			C[i,2]<-X1[i]
			C[i,2+r]=cos(r*X1[i])
			C[i,3+k]<-X2[i]
			C[i,3+k+r]<-cos(r*X2[i])
			C[i,4+(2*k)]<-X3[i]
			C[i,4+(2*k)+r]<-cos(r*X3[i])
		}
	}
	library(pracma)
	I<-diag(1,n,n)
	A<-C%*%pinv(t(C)%*%C)%*%t(C)
	Yhat<-A%*%Y
	alfa<-rep(0,q)
	alfa<-pinv(t(C)%*%C)%*%t(C)%*%Y
	error<-Y-Yhat
	MSE<-sum((error)^2)/n
	for(i in 1:n)
	{
		cat("\n","\t",Y[i],"\t",Yhat[i],"\t",error[i],"\n")
	}
cat("\n=========================================================\n")
	cat("MSE=",MSE,"\n")
	S<-0
		for(i in 1:n)
		{
			s<-(Yhat[i]-mean(Y))^2
			S<-S+s
		}
		F<-0
		for(i in 1:n)
		{
			f<-(Y[i]-mean(Y))^2
			F<-F+f
		}
	R<-S/F
	cat("Nilai Koefisien Determinasi =")
	print(R)
	print(alfa)
}
dx<-estimasi(Y, X1, X2, X3, kOptimal = 3)