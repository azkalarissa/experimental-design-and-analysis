### KASUS DUA FAKTOR : KUANTITATIF & KUALITIATIF ###
### SUDJANA HAL 187 ###

#INPUT DATA
periode  <- c(rep(1,12),rep(2,12),rep(3,12),rep(4,12))
kelompok <- rep(c(rep(1,4),rep(2,4),rep(3,4)),4)
hasil    <- c(3,6,-1,5,-2,-6,0,3,0,-3,-5,-6,4,3,6,4,-1,3,4,-3,-1,-5,4,5,5,7,4,9,3,4,5,-1,1,0,5,5,8,10,15,13,4,5,4,7,5,4,1,7)
data     <- data.frame(periode=factor(periode),kelompok=factor(kelompok),hasil=hasil)
View(data)

#ANAVA
anava <- aov(hasil~kelompok+periode+kelompok*periode,data=data)
summary(anava)

#PENGUJIAN POLINOM ORTOGONAL PERIODE KE-4
##Manual
linier    <- c(-3,-1,1,3)
kuadratik <- c(1,-1,-1,1)
kubik     <- c(-1,3,-3,1)
polinom   <- data.frame(linier,kuadratik,kubik)
jumlah    <- c(-6,23,47,83)
ksi       <- c(20,4,20)

##Perhitungan JK Polinom Ortogonal
hasil1    <- polinom*jumlah
jum       <- colSums(hasil1);jum
pembilang <- jum^2;pembilang
penyebut  <- 12*ksi;penyebut #nilai 12 adalah replikasi

JK1    <- pembilang/penyebut
RJK1   <- JK1/1     #ouputnya sama seperti hal 191
dbe    <- 36        #dbe dari anava sebelumnya = 36
Fhit   <- RJK1/8.47 #MSE dari anava sebelumnya = 8.47
Ftabel <- qf(0.95,1,dbe)
RJK1
Fhit
Ftabel

#PENGUJIAN INTERAKSI PERIODE-KELOMPOK
rep <- 4
K1  <- c(13,17,25,46)
K2  <- c(-5,3,11,20)
K3  <- c(-14,3,11,17)
K   <- data.frame(K1,K2,K3)

##JK Pl x K
Pl   <- colSums(linier*K)
PlxK <- sum(Pl^2)/(rep*ksi[1])-(sum(Pl))^2/(3*rep*ksi[1])

##JK Pd x K
Pd   <- colSums(kuadratik*K)
PdxK <- sum(Pd^2)/(rep*ksi[2])-(sum(Pd))^2/(3*rep*ksi[2])

##JK Pt x K
Pt   <- colSums(kubik*K)
PtxK <- sum(Pt^2)/(rep*ksi[3])-(sum(Pt))^2/(3*rep*ksi[3])

JK2  <- data.frame(PlxK,PdxK,PtxK)
JK2 #harus sama dengan Sum Sq kelompok:periode pada anava = 29

#UJI KEBERARTIAN
Ftabel2 <- qf(0.95,2,dbe)
RJK2    <- JK2/2         #2 berasal dari df interaksi K:P
RJK2
Fhit2   <- RJK2/8.47     #8.47 berasal dari MSE Anava
Fhit2
Ftabel2

#KOEFISIEN POLINOM ORTOGONAL DALAM U
A0        <- sum(hasil)/length(hasil)
polinom2  <- data.frame(linier,kuadratik,kubik)
ksi2      <- c(20,4,20)
hasil2    <- polinom2*jumlah
penyebut2 <- 12*ksi2
A1        <- colSums(hasil2)/penyebut2
A0
A1

### KASUS DUA FAKTOR : KEDUA FAKTOR KUANTITATIF ###

#INPUT DATA Hal 196
k<-4;r<-3 #k adalah taraf faktor , r adalah replikasi
A<-c(rep(1,r),rep(2,r),rep(3,r),rep(4,r))
A<-rep(c(rep(1,r),rep(2,r),rep(3,r),rep(4,r)),k)
B<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12))
y<-c(20,30,29,26,16,22,17,18,11,31,38,21,
     98,128,67,35,80,29,68,49,61,68,74,47,
     81,44,77,53,93,59,103,59,128,87,116,90,
     100,84,63,90,103,98,80,91,77,113,86,81)
#input respon Y per kolom
data<-data.frame(A=factor(A),B=factor(B),y=y)
data

#-------------------------------------------------------------------

#ANAVA
anava<-aov(y~A+B+A*B,data=data)
summary(anava)

#-------------------------------------------------------------------

#POLINOM ORTOGONAL FAKTOR A #lihat Tabel KOEFISIEN POLINOM ORTOGONAL
linierA<-c(-3,-1,1,3)
kuadratikA<-c(1,-1,-1,1)
kubikA<-c(-1,3,-3,1)
polinomA<-data.frame(linierA,kuadratikA,kubikA)
jumlahA<-c(821,704,761,852)     #input ??Ai
ksiA<-c(20,4,20)                #input ???i


#POLINOM ORTOGONAL FAKTOR B #lihat Tabel KOEFISIEN POLINOM ORTOGONAL
linierB<-c(-3,-1,1,3)
kuadratikB<-c(1,-1,-1,1)
kubikB<-c(-1,3,-3,1)
polinomB<-data.frame(linierB,kuadratikB,kubikB)
jumlahB<-c(279,804,989,1066)    #input ??Bj
ksiB<-c(20,4,20)                #input ???j

#-------------------------------------------------------------------

#Perhitungan JK Efek Faktor Utama A Berpolinom
hasilA<-polinomA*jumlahA  #koef.polinom x ??Ai
jumA<-colSums(hasilA)     #??(koef.polinom x ??Ai)
pembilangA<-jumA^2        #(??(koef.polinom x ??Ai))^2
penyebutA<-12*ksiA        #r x i x ???i
JKA<-pembilangA/penyebutA
KTA<-JKA/1;KTA            #df polinom selalu 1
dbe<-32                   #lihat pada hasil ANALISIS VARIANS
mse= 329.5                #lihat pada hasil ANALISIS VARIANS
#Kriteria Uji: F = (KTA)/(KTE)
FhitA<-KTA/mse ;FhitA
FtabelA<-qf(0.95,1,dbe);FtabelA #apabila alpha 5%



#PerhitungBn JK Efek Faktor Utama B Berpolinom
hasilB<-polinomB*jumlahB    #koef.polinom x ??Bj
jumB<-colSums(hasilB)       #??(koef.polinom x ??Bj)
pembilangB<-jumB^2          #(??(koef.polinom x ??Bj))^2
penyebutB<-12*ksiB          #r x i x ???j
JKB<-pembilangB/penyebutB
KTB<-JKB/1;KTB              #df polinom selalu 1
dbe<-32                     #lihat pada hasil ANALISIS VARIANS
mse= 329.5                  #lihat pada hasil ANALISIS VARIANS
#Kriteria Uji: F = (KTB)/(KTE)
FhitB<-KTB/mse ;FhitB
FtabelB<-qf(0.95,1,dbe);FtabelB #apabila alpha 5%

#-------------------------------------------------------------------

#INTERAKSI POLINOM ORTOGONAL ANTARA FAKTOR A DAN B
#Input Koefisien setiap efek polinom
linierA<-c(-3,-1,1,3)
kuadratikA<-c(1,-1,-1,1)
kubikA<-c(-1,3,-3,1)

linierB<-c(-3,-1,1,3)
kuadratikB<-c(1,-1,-1,1)
kubikB<-c(-1,3,-3,1)

#HAL 197
#Input sel ??ABij (dimasukkan berdasarkan baris)
a1<-c(79,293,202,247)
a2<-c(64,144,205,291)
a3<-c(46,178,289,248)
a4<-c(90,189,293,280)
jum<-matrix(c(a1,a2,a3,a4),k,k)  #dibuat matrix dengan baris = 4 dan kolom = 4
#menyesuaikan efek polinom
jumlah<-t(jum)                   #halaman 200
jumlah


#HAL 201
# A Linier X B Linier            #A sebagai baris dan B sebagai kolom
Alin_Blin<- (linierA)%*%t(linierB)       #perkalian koef.polinom yang bersangkutan
Alin_Blin                                #yang kanan harus dalam bentuk kolom
lin_jum <- Alin_Blin*jumlah              #perkalian koef.polinom yang bersangkutan x ??ABij 
lin_jum 
JK_AlxBl <- (sum(lin_jum))^2/(r*sum(Alin_Blin^2)) 
JK_AlxBl   

# A linier X B Kuadratik
Alin_Bkuad<-linierA%*%t(kuadratikB)
lin_jum<-Alin_Bkuad*jumlah
JK_AlxBt<-(sum(lin_jum))^2/(r*sum(Alin_Bkuad^2))
JK_AlxBt   

# A linier X B Kubik
Alin_Bkub<-linierA%*%t(kubikB)
lin_jum<-Alin_Bkub*jumlah
JK_AlxBt<-(sum(lin_jum))^2/(r*sum(Alin_Bkub^2))
JK_AlxBt   

#A kuadratik x B linier 
Akuad_Blin <- kuadratikA%*%t(linierB)
lin_jum <- Akuad_Blin*jumlah
JK_AkuadxBlin <- (sum(lin_jum))^2/(r*sum(Akuad_Blin^2))
JK_AkuadxBlin  

#A kuadratik x B kuadratik
Akuad_Bkuad <- kuadratikA%*%t(kuadratikB)
lin_jum <- Akuad_Bkuad*jumlah
JK_AkuadxBkuad <- (sum(lin_jum))^2/(r*sum(Akuad_Bkuad^2))
JK_AkuadxBkuad 

#dst dicoba masing masing
#yang di kiri harus selalu taraf yang sama 
#yang di kanan harus ditranspose agar jadi kolom
#jumlah sel setiap taraf faktor A dan B harus berbentuk matriks dengan susunan:
#baris adalah faktor A dan B adalah kolom

#-------------------------------------------------------------------

#Uji Keberartian Efek Faktor Interaksi Berpolinom

#Interaksi A linier dan B linier
KTAlin_Blin<-JK_AlxBl/1;KTAlin_Blin  #df polinom selalu 1
dbe<-32                              #lihat pada hasil ANALISIS VARIANS
mse= 329.5                           #lihat pada hasil ANALISIS VARIANS

#Kriteria Uji: F = (KTPolinom)/(KTE)
FhitAlin_Blin<-KTAlin_Blin/mse ;FhitAlin_Blin
FtabelAlin_Blin<-qf(0.95,1,dbe);FtabelAlin_Blin #apabila alpha 5%

#dst dicoba masing masing
#dengan df semua polinom selalu 1
#nilai dbe dan mse di setiap perhitungan pasti sama