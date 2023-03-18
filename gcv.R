#PENENTUAN PARAMETER OSILASI OPTIMAL
data=read.csv("d://TA/Skripsi 2/File Kerja/data.txt")
data
Y=as.matrix(data[,2])
X1=as.matrix(data[,3])
X2=as.matrix(data[,4])
X3=as.matrix(data[,5])

deretfourier<-function(Y, X1, X2, X3, K)
{
	n=length(Y)
	p=ncol(data)-1
	q<-(p*(K+1))+1
	C<-matrix(0,n,q)
	hasil<-matrix(0,K,2)
	for (k in 1:K)
	{
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
		A<-C %*% pinv(t(C) %*% C) %*% t(C)
		Yhat<-A %*% Y
		df<-sum(diag(A))
		W<-(1/n)*I
		atas<-t(Y-Yhat) %*% W %*% (Y-Yhat)
		bawah<-((1-df)/n)^2
		GCV<-atas/bawah
		hasil[k,1]<-k
		hasil[k,2]<-GCV
		print(Yhat)
		print(atas)
		print(bawah)
	}
	print(hasil)

	GCV2<-min(hasil[,2])
	s<-1
	repeat{
		if(hasil[s,2]==GCV2)
		{
			kOpt<-hasil[s,1]
			GCVOpt<-GCV2
			break
		}
		else s<-s+1
	}
	cat("nilai K Optimal adalah \t",kOpt,"\n")
}
deretfourier(Y, X1, X2, X3, k<-c(3))