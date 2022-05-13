#Desain Blok Acak
#Input Data
O=c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
F=rep(c(rep(1,3),rep(2,3)),4)
L=rep(c(1,2,3),8)
respon=c(90,102,114,
         86,87,93,
         96,106,112,
         84,90,91,
         100,105,108,
         92,97,95,
         92,96,98,
         81,80,83)
data=data.frame(O=factor(O),F=factor(F),L=factor(L),respon=respon)

#Anava
hasil.anava=aov(respon~F+L+F*L+O,data)
summary(hasil.anava)

#Uji Lanjut Duncan
require(laercio)
LDuncan(hasil.anava,"data")


#Anava Lengkap
hasil.anava2=aov(respon~F+L+F*L+O+F*O+L*O+F*L*O,data)
summary(hasil.anava2)

MSF	  <- 1066.7	  ; dfF = 1
MSL	  <- 335.6	  ; dfL = 2
MSO	  <- 402.2	  ; dfO	= 3
MSFL	<- 77.1	    ; dfFL= 2
MSFO	<- 34.3   	; dfFO= 3
MSLO	<- 99.1     ; dfLO= 6
MSE	  <- 32.9	    ; dfE	= 6

F.F	  <- MSF/MSFO
p.F 	<- 1-pf(F.F, dfF, dfFO)
F.L	  <- MSL/MSLO
p.L 	<- 1-pf(F.L, dfL, dfLO)
F.FL	<- MSFL/MSE
p.FL	<- 1-pf(F.FL, dfFL, dfE)
F.O  	<- MSO/MSE
p.O 	<- 1-pf(F.O, dfO, dfE)
F.FO	<- MSFO/MSE
p.FO 	<- 1-pf(F.FO, dfFO, dfE)
F.LO	<- MSLO/MSE
p.LO 	<- 1-pf(F.LO, dfLO, dfE)


cat("F.F =", F.F, "with p =",p.F,"\n")
cat("F.L =", F.L, "with p =",p.L,"\n")
cat("F.FL =", F.FL, "with p =",p.FL,"\n")
cat("F.O =", F.O, "with p =",p.O,"\n")
cat("F.FO =", F.FO, "with p =",p.FO,"\n")
cat("F.LO =", F.LO, "with p =",p.LO,"\n")



#Desain BujurSangkar
O=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6)
F=c(1,1,1,2,2,2,1,1,2,2,1,2,1,1,2,1,2,2,2,2,1,1,2,1,2,2,2,1,1,1,2,2,1,2,1,1)
L=c(1,3,2,2,3,1,2,1,2,1,3,3,3,2,3,1,1,2,1,3,1,2,2,3,3,2,1,3,1,2,2,1,3,3,2,1)
H=c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
respon=c(90,114,102,87,93,86,106,96,90,84,112,91,108,105,95,100,92,97,81,83,92,96,80,98,90,86,85,110,90,100,88,84,104,91,98,92)
data=data.frame(O=factor(O),F=factor(F),L=factor(L),H=factor(H),respon=respon)
hasil.anava=aov(respon~F+L+F*L+O+H,data)
summary(hasil.anava)

