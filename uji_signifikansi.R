data=read.csv("d://TA/Skripsi 2/File Kerja/data.txt")
el=read.csv("d://TA/Skripsi 2/File Kerja/yhat.txt")
B=read.csv("d://TA/Skripsi 2/File Kerja/beta.txt")
data=as.matrix(data)
yhat=as.matrix(el[,5])
ybar=mean(data[,2])
Bbar=mean(B[,2])
alpha=0.03
p=nrow(data)
q=ncol(data)
cat("=======================================","\n")
cat("Estimasi Parameter","\n")
cat("=======================================","\n")
n1=nrow(B)
res=el[,6]
SSR=sum((yhat-ybar)^2)
SSE=sum((data[,2]-yhat)^2)
SST=SSR+SSE
MSR=SSR/(n1-1)
MSE=SSE/(p-n1)
Rsq=(SSR/SST)*100

#uji F (uji serentak)
Fhit=MSR/MSE
pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail=FALSE)
if (pvalue<=0.05){
cat("------------------------------------","\n")
cat("Kesimpulan hasil uji serentak","\n")
cat("------------------------------------","\n")
cat("Tolak Ho yakni minimal terdapat 1 prediktor yang signifikan","\n")
cat("","\n")}else{
cat("------------------------------------","\n")
cat("Kesimpulan hasil uji serentak","\n")
cat("------------------------------------","\n")
cat("Gagal Tolak Ho yakni semua prediktor tidak berpengaruh signifikan","\n")
cat("","\n")}

#uji t (uji individu)
cat("------------------------------------","\n")
cat("Kesimpulan hasil uji individu","\n")
cat("------------------------------------","\n")
thit=rep(NA,n1)
pval=rep(NA,n1)
for (i in 1:n1)
{
	S=((B[i,1]-Bbar)^2)/(n1-1)
	SE=S/SSE
	thit[i]=B[i,1]/SE
	pval[i]=2*(pt(abs(thit[i]),(p-n1),lower.tail=FALSE))
	if (pval[i]<=0.05){
		cat("Tolak Ho yakni prediktor signifikan dengan pvalue",pval[i],"\n")}else{
		cat("Gagal tolak Ho yakni prediktor tidak signifikan dengan pvalue",pval[i],"\n")}
}
thit=as.matrix(thit)
cat("=======================================","\n")
cat("nilai t hitung","\n")
cat("=======================================","\n")
print(thit)
cat("Analysis of Variance","\n")
cat("======================================","\n")
cat("Sumber df SS MS Fhit","\n")
cat("Regresi ",n1-1," ",SSR," ",MSR,"",Fhit,"\n")
cat("Error ",p-n1," ",SSE,"",MSE,"\n")
cat("Total ",p-1," ",SST,"\n")
cat("======================================","\n")
cat("s=",sqrt(MSE)," Rsq=",Rsq,"\n")
cat("pvalue(F)=",pvalue,"\n")