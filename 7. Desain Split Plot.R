#KASUS 1
#Input Data
respon = c(162,201,175,188,233,213,217,170,195,122,138,147,126,185,
           152,158,130,180,160,170,182,229,186,161,167,181,155,201,
           227,172,223,181,156,182,201,199)
R = rep(1:3,each=3,times=4)
T = c(rep(1,9),rep(2,9),rep(3,9),rep(4,9))
B=rep(1:3,times=12)
data=data.frame(R=factor(R),T=factor(T),B=factor(B),respon=respon)

#Uji ANAVA
hasil.aov=aov(respon~R+T+T*R+B+R*B+T*B+R*T*B,data)
summary(hasil.aov)

#Tentukan EKT Desain Eksperimen
#Menentukan nilai F hitung
#Perlakuan Temperatur
MS_T=4165 ; df_T=3
MS_RT=389; df_RT=6
F.T=MS_T/MS_RT
p.T=1-pf(F.T, df_T, df_RT)
cat("F.T=", F.T, "with p=",p.T,"\n")

#Perlakuan Pembakaran
MS_B=283 ; df_B=2
MS_RB=585 ; df_RB=4
F.B=MS_B/MS_RB
p.B=1-pf(F.B, df_B, df_RB)
cat("F.B=", F.B, "with p=",p.B,"\n")

#Interakasi Temperatur dan Pembakaran
MS_TB=433 ; df_TB=6
MS_RTB=642 ; df_RTB=12
F.TB=MS_TB/MS_RTB
p.TB=1-pf(F.TB, df_TB, df_RTB)
cat("F.TB=", F.TB, "with p=",p.TB,"\n")

#KASUS 2
#Input Data
respon=c(30,34,29,28,31,31,31,35,32,35,41,26,32,36,30,37,40,34,37,
         38,33,40,42,32,41,39,39,36,42,36,41,40,40,40,44,45)
R=rep(1:3,each=3,times=4)
S=c(rep(1,9),rep(2,9),rep(3,9),rep(4,9))
M=rep(1:3,times=12)
data=data.frame(R=factor(R),S=factor(S),M=factor(M),respon=respon)

#Uji ANAVA
hasil.aov=aov(respon~R+S+S*R+M+R*M+S*M+R*S*M,data)
summary(hasil.aov)

#Menentukan nilai F hitung
#Perlakuan Suhu
MS_S=144.69 ; df_S=3
MS_RS=3.44; df_RS=6
F.T=MS_S/MS_RS
p.T=1-pf(F.T, df_S, df_RS)
cat("F.T=", F.T, "with p=",p.T,"\n")

#Perlakuan Metode
MS_M=64.19 ; df_M=2
MS_RM=9.07 ; df_RM=4
F.M=MS_M/MS_RM
p.M=1-pf(F.M, df_M, df_RM)
cat("F.M=", F.M, "with p=",p.M,"\n")

#Interaksi Temperatur dan Pembakaran
MS_SM=12.53 ; df_SM=6
MS_RSM=4.24 ; df_RSM=12
F.SM=MS_SM/MS_RSM
p.SM=1-pf(F.SM, df_SM, df_RSM)
cat("F.SM=", F.SM, "with p=",p.SM,"\n")

