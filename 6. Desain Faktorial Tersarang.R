#---------------------------------------2 FAKTOR------------------------------
#Input data
Gol<-c(1,1,1,1,1,1,1,1,
       2,2,2,2,2,2,2,2,
       3,3,3,3,3,3,3,3)
Tim<-c(1,1,1,1,2,2,2,2,
       3,3,3,3,4,4,4,4,
       5,5,5,5,6,6,6,6)
respon<-c(10,14,13,14,
          12,10,15,16,
          10,9,10,18,
          11,8,9,10,
          10,7,8,9,
          9,9,8,10)
data<-data.frame(Gol=factor(Gol),Tim=factor(Tim),respon=respon)

#Anava
hasil.aov<-aov(respon~Gol+Tim+Gol*Tim,data)
summary(hasil.aov)

#Untuk Model Campuran (Gol. tetap, Tim acak)
MSGol <-36.29; dfGol <-2
MSTim <-3.71 ; dfTim <-3
MSE   <-5.46 ; dfE   <-18

#Menguji pengaruh faktor golongan
F.G   <- MSGol/MSTim
p.G   <- 1-pf(F.G,dfGol,dfTim)
cat("F.G=",F.G,"with p=",p.G,"\n")
#pvalue<alpha, Tolak H0
#Terdapat pengaruh yang signifikan dari faktor Golongan
#terhadap waktu penyelesaian tugas

#Menguji pengaruh faktor tim
F.T   <-MSTim/MSE
p.T   <-1-pf(F.T,dfTim,dfE)
cat("F.T=",F.T,"with p=",p.T,"\n")
#pvalue>alpha, Terima H0
#Tidak terdapat pengaruh yang signifikan dari faktor tim yang tersarang pada golongan
#terhadap waktu penyelesaian tugas


#---------------------------------------3 FAKTOR------------------------------

###=======EKSPERIMEN FAKTORIAL TERSARANG========#####
#Gunakan Contoh X(2) Sudjana hal 263
#Input data
#Metoda, Kelompok Tetap
#Tim Acak
Metode  <- c(1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2,
             1,1,2,2)
Tim     <- c(1,1,1,1,2,2,2,2,3,3,3,3,
             4,4,4,4,5,5,5,5,6,6,6,6,
             7,7,7,7,8,8,8,8,9,9,9,9)
Kelompok<- c(1,1,1,1,1,1,1,1,1,1,1,1,
             2,2,2,2,2,2,2,2,2,2,2,2,
             3,3,3,3,3,3,3,3,3,3,3,3)
respon  <- c(20.2,24.1,14.2,16.2,
             26.2,26.9,18,19.1,
             23.8,24.9,12.5,15.4,
             22,23.5,14.1,16.1,
             22.6,24.6,14,18.1,
             22.9,25,13.7,16,
             23.1,22.9,14.1,16.1,
             22.9,23.7,12.2,13.8,
             21.8,23.5,12.7,15.1)
data    <- data.frame(Metode=factor(Metode),Tim=factor(Tim),Kelompok=factor(Kelompok),respon=respon)

#Anava
hasil.aov<-aov(respon~Metode+Kelompok+Metode*Kelompok+Tim+Metode*Tim,data)
summary(hasil.aov)

MSM <- 652 ; dfM <- 1
MSK <- 8   ; dfK <- 2
MST <- 6.5 ; dfT <- 6
MSMK<- 0.6 ; dfMK<- 2
MSMT<- 1.8 ; dfMT<- 6
MSE <- 2.3 ; dfE <- 18


#Menguji Pengaruh Faktor Metode
F.M <- MSM/MSMT
p.M <- 1-pf(F.M,dfM,dfMT)
cat("F.M=",F.M,"with p=",p.M,"\n")
#pvalue<alpha, Tolak H0
#Terdapat pengaruh yang signifikan dari faktor Metoda
#terhadap waktu penyelesaian pembuatan barang

#Menguji Pengaruh Faktor Kelompok
F.K <- MSK/MST
p.K <- 1-pf(F.K,dfK,dfT)
cat("F.K=",F.K,"with p=",p.K,"\n")
#pvalue>alpha, Terima H0
#Tidak terdapat pengaruh yang signifikan dari faktor Kelompok
#terhadap waktu penyelesaian pembuatan barang

#Menguji Interaksi Metode dan Kelompok
F.MK <- MSMK/MSMT
p.MK <- 1-pf(F.MK,dfMK,dfMT)
cat("F.MK=",F.MK,"with p=",p.MK,"\n")
#pvalue>alpha, Terima H0
#Tidak terdapat pengaruh yang signifikan dari interaksi faktor Metoda dan Kelompok
#terhadap waktu penyelesaian pembuatan barang


