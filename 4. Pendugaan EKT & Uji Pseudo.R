A<-c(rep(1,18),rep(2,18));A #pengulangan angka 1 dan 2sebanyak 18 kali
A=rep(1:2,each=18);A #New

B<-c(rep(1,6),rep(2,6),rep(3,6),rep(1,6),rep(2,6),rep(3,6));B
B=rep(1:3, each=6 ,time=2);B#new

C<-c(1,1,2,2,3,3,1,1,2,2,3,3,1,1,2,2,3,3,1,1,2,2,3,3,1,1,2,2,3,3,1,1,2,2,3,3);C
C=rep(1:3, each=2 );C#new
C=rep(1:3, each=2 ,time=6);C#new


respon<-c(59,57,54,58,40,44,59,57,54,50,42,45,61,59,59,60,49,50,45,44,45,40,32,36,20,49,49,50,33,39,49,50,51,52,39,42)
data<-data.frame(A=factor(A),B=factor(B), C=factor(C),respon=respon)
#Analisis Varians (untuk melihat MSnya saja)
hasil.aov<-aov(respon~A+B+C+A*B+A*C+B*C+A*B*C, data)
summary(hasil.aov)

 #Kuadrat Tengah
MSA <-1024.0 ;   dfA <- 1
MSB <-139.1 ;   dfB <- 2  
MSAB <-5.3 ;   dfAB <- 2 
MSAC <-60.1 ;   dfAC <- 2  
MSBC <-14.8 ;   dfBC <- 4
MSABC <-36.0 ;   dfABC <- 4
MSC <-434.1 ;   dfC <- 2  
MSE <-27.8 ;   dfE <- 18
#Tentukan EKT
#Untuk Menguji ada tidaknya pengaruh Hari kerja terhadap Hasil kerja (
#lihat EKT pada Sudjana hal 250)
#Fhitung
F.A <- MSA/MSE
F.B <- MSB/MSAB
F.C <- MSC/MSAC
F.AB <-MSAB/MSE
F.AC <- MSAC/MSE
F.BC <-MSBC/MSABC
F.ABC <-MSABC/MSE
fhit= data.frame(F.A,F.B,F.C,F.AB,F.AC,F.BC,F.ABC)
fhit
#p-value
p.A <- 1-pf(F.A, dfA, dfE)
p.B <- 1-pf(F.B,dfB, dfAB)
p.C <- 1-pf(F.C,dfC,dfAC)
p.AB <- 1-pf(F.AB,dfAB,dfE)
p.AC <- 1-pf(F.AC,dfAC,dfE)
p.BC <- 1-pf(F.BC,dfBC,dfABC)
p.ABC <-1-pf(F.ABC,dfABC,dfE)
pvalue<-data.frame(p.A,p.B,p.C,p.AB,p.AC,p.BC,p.ABC)
pvalue



