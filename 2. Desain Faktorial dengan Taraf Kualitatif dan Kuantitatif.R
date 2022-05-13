#------Entry data :
Suhu<-c(20,20,20,20,
        25,25,25,25,
        30,30,30,30,
        35,35,35,35,
        40,40,40,40)
PengZ<-c(10,8,9,10,
         18,16,16,15,
         25,20,24,23,
         27,26,25,29,
         23,20,18,20)

#------Anava untuk menguji ada tidaknya pengaruh suhu terhadap pengembangan volume Zat
data<-data.frame(Suhu=factor(Suhu),respon=PengZ)
View(data)
hasil.anava<-aov(respon~Suhu,data)
summary(hasil.anava)

#------Kalau hasil pengujian signifikan, selanjutnya dicari model hubungan fungsional antara faktor sebagai perlakuan dengan variabel respon-nya 
#------Plot diagram pencar dan garis regresinya
plot(Suhu,PengZ)
abline(lm(PengZ~Suhu))
lm(formula = PengZ ~ Suhu)
#Diperoleh persamaan Yx = -0,40 + 0,65Xj


#------Lakukan uji keberartian regresi linier
#H0 : Penyimpangan regresi linier tidak berarti
#H1 : Penyimpangan regresi linier berarti (perlu regresi lengkung)
n=length(Suhu)
lm.res=resid(lm(PengZ~Suhu))
lm.res
b1=(coef(lm(PengZ~Suhu)))[['Suhu']]
s=sqrt(sum(lm.res^2)/(n-2))
SE=s/sqrt(sum((Suhu-mean(Suhu))^2))
t=(b1-(-1))/SE
df=n-2
pt(t,(df),lower.tail=FALSE)
#tolak H0 jika pvalue < alpha
#H0 ditolak sehingga perlu dilakukan uji regresi lengkung
#Kesimpulan: 
#Dengan taraf signifikansi 5%, 
#tampak bahwa penyimpangan dari regresi linier berarti
#Sehingga model non-linier (lengkung) perlu dianalisis. 


#-----------------------------------------------------------#
#REGRESI LENGKUNG
#REGRESI KUADRATIK
gp2<-lm(PengZ~Suhu+I(Suhu^2))
gp2
#Diperoleh persamaan Yx = -73,25 + 5,79Xj - 0,085Xj^2

##Uji Kecocokan Model
#H0 : Model regresi kuadratik tidak cocok digunakan
#H1 :  Model regresi kuadratik cocok digunakan
summary(gp2)
anova(gp2)
#tolak H0 jika pvalue < alpha
#Dengan taraf signifikansi 5%, dari output tabel analisis varians di atas, 
#diperoleh nilai p-value Suhu < ?? yang berarti regresi linier signifikan 
#dan nilai p-value Suhu^2 < ?? maka H0 ditolak, artinya regresi kuadratik signifikan. 

gp3<-lm(PengZ~Suhu+I(Suhu^2)+I(Suhu^3))
gp3
summary(gp3)
anova(gp3)

#untuk cari tahu mana model yang paling tepat harus dilakukan uji model order yang lebih tinggi
#berhenti saat model yang diuji tidak signifikan
#sehingga lebih mudah dilakukan dengan menggunakan POLINOM ORTOGONAL


#-----------------------------------------------------------#

#POLINOM ORTOGONAL

#manual
linear<-c(-2,-1,0,1,2)
kuadratik<-c(2,-1,-2,-1,2)
kubik<-c(-1,2,0,-2,1)
kuartik<-c(1,-4,6,-4,1)
polinom<-data.frame(linear,kuadratik,kubik,kuartik)
polinom
jumlah<-c(37,65,92,107,81)
ksi<-c(10,14,10,70)

#Perhitungan JK Polinom Ortogonal
hasil<-polinom*jumlah;hasil   #hasilnya sama kaya di halaman 180
jum<-colSums(hasil);jum
pembilang<-jum^2;pembilang
penyebut<-4*ksi;penyebut  #nilai 4 adalah replikasi

JKP<-pembilang/penyebut;JKP ## Hal 184
KTP<-JKP/1;KTP
Fhit<-KTP/2.867  #2.867 adalah Mean Sq Residuals dari anava
dke<-15   #15 adalah Residuals dari anava (r*(j-1))
Ftabel1<-qf(0.95,1,dke)
Fhit
Ftabel1