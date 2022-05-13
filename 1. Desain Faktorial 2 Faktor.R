#-----------------------CONTOH SOAL SUDJANA
Waktu <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
Pupuk <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,
           1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
Respon<- c(28.6,36.8,32.7,32.6,29.1,29.2,30.6,29.1,28.4,27.4,26,29.3,29.2,28.2,27.7,32,
           30.3,32.3,31.6,30.9,32.7,30.8,31.0,33.8,30.3,32.7,33.0,33.9,32.7,31.7,31.8,29.4)
data2<-data.frame(Waktu=factor(Waktu), Pupuk=factor(Pupuk),respon=Respon)
View(data2)
hasil.aov2<-aov(respon~Waktu+Pupuk+Waktu*Pupuk, data2)
summary(hasil.aov2)

#Mencari Fhitung Faktor Acak
MSA <- 32; dfA <- 1
MSB <- 5.47; dfB <- 3
MSAB <- 12.8; dfAB <- 3
MSE <- 3.07; dfE <- 24

FA <- MSA/MSAB; pA <- 1-pf(FA,dfA,dfAB)
FB <- MSB/MSAB; pB <- 1-pf(FB,dfB,dfAB)
FAB <- MSAB/MSE; pAB <- 1-pf(FAB,dfAB,dfE)

cat("F(A)=",FA,"with p =",pA,"\n")
cat("F(B)=",FB,"with p =",pB,"\n")
cat("F(AB)=",FAB,"with p =",pAB,"\n")
