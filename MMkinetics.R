# This script applys moisture function on MM kinetics where substrate and km are both moisture dependent
# Gas and solute diffusivity (delivery rate) calculated based on Bruggeman expression
rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)

mpdata <- read.csv(file = 'datasets.csv')
#==subsetting unique sample ====
rate<-select(filter(mpdata,ra_s1==1), c(id, rs,mvol, mwp, porosity,clay, bd, org, ra))

#===calculating accessible soc an oxy at steady-state====
opt<-function(po,mvol,bd,org){
  soc<-org/12/bd*0.1  #assum ss at rs=0.6, 10% DOC  (mol/cm3)
  oxy<-(po-mvol)*0.21 #(v/v)
  return (list(soc, oxy))}
po<-rate$porosity
mvol<-rate$mvol
org<-rate$org
bd<-rate$bd
ss_opt<-opt(po,mvol,bd,org)
rate$soc_ss<-ss_opt[[1]] ##approximately 100mol/cm3
rate$oxy_ss<-ss_opt[[2]]

#===simulations under steady state====
f_ref<-function(phi,mvol,po,soc,oxy) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- t1*t2*t3* (soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

phi<-rate$mwp
mvol<-rate$mvol
po<-rate$porosity
soc<-rate$soc_ss
oxy<-rate$oxy_ss
outss<-f_ref(phi,mvol, po, soc, oxy)
rate$Vmax<-1/outss #estimate Vmax at ra=1

f_refemp<-function(rs,soc,oxy) {
  femp<-3.11*rs-2.42*rs^2
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- femp*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

rs<-rate$rs
soc<-rate$soc_ss
oxy<-rate$oxy_ss
outss<-f_refemp(rs,soc,oxy)
rate$Vemp<-1/outss #estimate Vmax at ra=1

#===check Vmax calculation====
f_ss<-function(phi,mvol,po,soc,oxy,vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- t1*t2*t3* vmax*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

phi<-rate$mwp
mvol<-rate$mvol
po<-rate$porosity
soc<-rate$soc_ss
oxy<-rate$oxy_ss
vmax<-rate$Vmax
outss<-f_ss(phi,mvol, po, soc, oxy,vmax)
#==
f_refemp<-function(rs,soc,oxy,Vemp) {
  femp<-3.11*rs-2.42*rs^2
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- femp*Vemp*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

rs<-rate$rs
soc<-rate$soc_ss
oxy<-rate$oxy_ss
Vemp<-rate$Vemp
outss<-f_refemp(rs,soc,oxy,Vemp)


#====preparing dataframe for ss and nonss modeling====
userate<-select(rate, c(id, soc_ss, oxy_ss, Vmax,Vemp))
usesoil<-select(mpdata, c(id,rs, mvol, mwp, mwplog, bd, porosity, clay, org, ra))
newmpdata<-merge(usesoil, userate, by="id")
write.csv(newmpdata, "datasets_mmkparams.csv")

#===SS simulation===
#===simulations under steady state====
f_ss<-function(phi,mvol,po,soc,oxy,vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- t1*t2*t3* vmax*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

phi<-newmpdata$mwp
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
vmax<-newmpdata$Vmax
outss<-f_ss(phi,mvol, po, soc, oxy,vmax)
newmpdata$fghe_ss<-outss  #note--need to scale ghe predictions

f_ssemp<-function(rs,soc,oxy,vmax) {
  femp<-3.11*rs-2.42*rs^2
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  F1 <- femp* vmax*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(F1)}

rs<-newmpdata$rs
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
vmax<-newmpdata$Vemp
outssemp<-f_ssemp(rs, soc, oxy,vmax)
newmpdata$femp_ss<-outssemp

write.csv(newmpdata, "datasets_mmkparams.csv")

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonss1<-function(phi,mvol,po,soc, oxy,vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  soc<-soc*t3
  oxy<-oxy*t2
  Vm<-vmax
  F1 <- t1*Vm*(soc) /(kmoc + soc)*(oxy/(oxy+kmo)) 
  return(list(soc, oxy, F1))}

phi<-newmpdata$mwp
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
vmax<-newmpdata$Vmax

ghens1<-f_nonss1(phi,mvol, po, soc, oxy, vmax)

newmpdata$soc_ghens1<-ghens1[[1]]
newmpdata$oxy_ghens1<-ghens1[[2]]
newmpdata$fw_ghens1<-ghens1[[3]]

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonss2<-function(phi,mvol,po,soc, oxy,vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  soc<-soc*t3
  oxy<-oxy*t2
  Vm<-vmax
  F1 <- t1*t2*t3*Vm*(soc) /(kmoc + soc)*(oxy/(oxy+kmo)) 
  return(list(soc, oxy, F1))}

phi<-newmpdata$mwp
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
vmax<-newmpdata$Vmax

ghens2<-f_nonss2(phi,mvol, po, soc, oxy, vmax)

newmpdata$soc_ghens2<-ghens2[[1]]
newmpdata$oxy_ghens2<-ghens2[[2]]
newmpdata$fw_ghens2<-ghens2[[3]]

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonssemp<-function(rs,mvol, po, soc,oxy,vemp) {
  femp<-3.11*rs-2.42*rs^2
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  kmoc<-0.00001  #10uM to mol/cm3
  kmo<-0.121 #v/v ratio
  soc<-soc*t3
  oxy<-oxy*t2
  F1 <- femp* vmax*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(list(soc, oxy, F1))}

rs<-newmpdata$rs
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
vemp<-newmpdata$Vemp

empns<-f_nonssemp(rs,mvol, po, soc, oxy, vemp)

newmpdata$soc_empns<-empns[[1]]
newmpdata$oxy_empns<-empns[[2]]
newmpdata$fw_empns<-empns[[3]]



#=====preparing km values for nonSS simulation (nonSS enzyme-substrate binding (as Km)======================================
Npsite=3000.0 # number of transporter per cell
k2_p=100.0  #transporter specific substrate uptake rate, unit:s-1
rc=1.e-6  #microbial cell radius unit:m
rp=1.e-9  #transporter radius unit:m
Na=6.e23  #Avogadro number
Ratm=50 #atmospheric resistance, 50 s/m

temp=25+273.15#define temperature

#=calculating gaseous and aqueous tortuosity (Original paper based on Morldrup, 2003, replaced with Bruggeman expression for consistency)
tau<-function (mvol, po){
  wpo<-mvol  #water filled porosity
  gpo<-po-mvol #gas filled porosity
  taug<-((po-mvol)/po)^0.5
  tauw<-(mvol/po)^0.5
  return (list(wpo=wpo, gpo=gpo,taug=taug,tauw=tauw))
}
mvol<-newmpdata$mvol
po<-newmpdata$porosity
tortuo<-tau(mvol, po)

#==calculating O2 and OC diffusivity (Equ 3)
Diffu<-function (gpo, wpo, taug, tauw){
  Dw_o2=2.4e-9*temp/298.0  #aqueous tracer diffusivity at 25
  Dg_o2=1.8e-5*(temp/273.0)**1.82 #oxygen diffusivity
  henry_o2=1.3e-3*exp(-1500.*(1/temp-1/298.15))
  bunsen_o2=henry_o2*temp/12.2
  Dwo2=Dw_o2*tauw*wpo+Dg_o2*taug*gpo/bunsen_o2 #bulk aqueous molecular diffusivity as a colume weighted average between aquesous and gaseous phases
  Dw_s=1.4e-9
  Dws=Dw_s*tauw*wpo#bulk substrate diffusivity (between the soil matrix and microsite)
  return(list(Dwo2=Dwo2, Dws=Dws))
}

gpo<-tortuo$gpo
wpo<-tortuo$wpo
taug<-tortuo$taug
tauw<-tortuo$tauw
Dw<-Diffu(gpo, wpo, taug, tauw)

#==calculating Km for OC (mol m-3)========
Kaff<-function(gpo, wpo, taug, tauw, Dws,Dwg,mp){
  Ncell<-100 #number of cells per microsite
  Bdens<-Ncell/Na #free microsite microbial abundance mol/m3
  Rm<-rc*(80*Ncell)^(1/3)  #microsite radius
  vm<-pi*4/3*Rm^3  #microsite volume 
  kw2<-k2_p*Npsite #(Npsite*rp+pi*rc) #cell specific uptake rate for OX, unit s-1
  Dw_s0 = 1.4e-9  #for claculating reference affinity
  Dw_g0 = 2.4e-9
  fin<-Npsite*rp/ (Npsite*rp+pi*rc)#interception probability (number of molecules that 1 mol cell will encounter and be able to intercept)
  ksw1<-4*pi*Dw_s0*rc*fin*Na   #substrate delivery parameter unit m3 mo-1 s-1
  kow1<-4*pi*Dw_g0*rc*fin*Na
  ksw0<-kw2/ksw1#reference affinity (Km used in MM kinetics in a well-mixed solution)
  kow0<-kw2/kow1#reference affinity (Km used in MM kinetics in a well-mixed solution)
  film<-exp(-13.65-0.857*log(mp/1000))    #calculting water film thickness
  ks_con<- (film/(Rm*Dw_s0*(Rm+film)) + 1/(Dws*(Rm+film)))*vm/(4*pi)#conductance coefficient
  ks_aff<-ksw0*(1+ks_con*ksw1*Ncell/Na/vm)
  ko_con<- (film/(Rm*Dw_g0*(Rm+film)) + 1/(Dwg*(Rm+film)))*vm/(4*pi)#conductance coefficient
  ko_aff<-kow0*(1+ko_con*kow1*Ncell/Na/vm)
  return(list(ksw0=ksw0, kow0=kow0,KaffOC=ks_aff, Kaffoxy=ko_aff))
}

mp<-newmpdata$mwp
Dws<-Dw$Dws
Dwg<-Dw$Dwo2
test<-Kaff(gpo, wpo, taug, tauw, Dws,Dwg,mp)

newmpdata$kms_nonss2<-test$KaffOC/100
newmpdata$kmo_nonss2<-test$Kaffoxy*22400
#========end=====================================================

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonss1<-function(phi,mvol,po, soc,oxy,kmoc, kmo, vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  Vm<-vmax
  F1 <- t1*Vm*(soc) /(kmoc + soc)*(oxy/(oxy+kmo)) 
  return(list(soc, oxy, F1))}

phi<-newmpdata$mwp
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
kmoc<-newmpdata$kms_nonss2
kmo <- newmpdata$kms_nonss2
vmax<-newmpdata$Vmax

ghenss1<-f_nonss1(phi,mvol, po, soc, oxy, kmoc, kmo,vmax)

newmpdata$soc_ghenss1<-ghenss1[[1]]
newmpdata$oxy_ghenss1<-ghenss1[[2]]
newmpdata$fw_ghenss1<-ghenss1[[3]]

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonss2<-function(phi,mvol,po,soc, oxy,kmoc, kmo,vmax) {
  t1<- exp(-0.00021*phi)##Microbial activity
  t2<-((po-mvol)/po)^0.5##Gas diffusivity
  t3<-(mvol/po)^0.5##substrate accessibility
  Vm<-vmax
  F1 <- t1*t2*t3*Vm*(soc) /(kmoc + soc)*(oxy/(oxy+kmo)) 
  return(list(soc, oxy, F1))}

phi<-newmpdata$mwp
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
kmoc<-newmpdata$kms_nonss2
kmo <- newmpdata$kms_nonss2
vmax<-newmpdata$Vmax

ghenss2<-f_nonss2(phi,mvol, po, soc, oxy, kmoc, kmo,vmax)

newmpdata$soc_ghenss2<-ghenss2[[1]]
newmpdata$oxy_ghenss2<-ghenss2[[2]]
newmpdata$fw_ghenss2<-ghenss2[[3]]

#===nonSS simulations (diffusion control of SOC and DO)============================
f_nonssemp<-function(rs,mvol, po, soc,oxy,kmoc, kmo,vemp) {
  femp<-3.11*rs-2.42*rs^2
  F1 <- femp* vmax*(soc) / (kmoc + soc)*(oxy/(oxy+kmo)) #0.121 cm3/cm3 from davidson 0.25 g g-1 from MEND
  return(list(soc, oxy, F1))}

rs<-newmpdata$rs
mvol<-newmpdata$mvol
po<-newmpdata$porosity
soc<-newmpdata$soc_ss
oxy<-newmpdata$oxy_ss
kmoc<-newmpdata$kms_nonss2
kmo <- newmpdata$kms_nonss2
vemp<-newmpdata$Vemp

empns<-f_nonssemp(rs,mvol, po, soc, oxy,kmoc, kmo, vemp)

newmpdata$soc_empkm<-empns[[1]]
newmpdata$oxy_empkm<-empns[[2]]
newmpdata$fw_empkm<-empns[[3]]
#=======================================simulations done=================================================
#====residual plots========
predata<-read.csv("datasets_mmk_scaled.csv")
test<-data.frame(rs=predata$rs)
test$mmk<-predata$ghekmns2_scaled-predata$ra

simple.fit = lm(mmk~rs, data=test)
summary(simple.fit)

plot1<-ggplot(test, aes(x = rs,y = mmk)) +
  geom_hline(yintercept=0, linetype="dashed", size=0.3)+
  scale_x_continuous(name = "Observed RA",limits=c(0,1)) +
  scale_y_continuous(name = 'Predicted RA',limits=c(-1,1)) + 
  geom_point(aes( ), color= "#642ba6",size=1,shape=1,stroke=0.5)+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="tomato1", se=TRUE)+
  theme(text = element_text(size=16)) 
plot2<-plot1+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))
print(plot2)

pdf("MMk_ghekmns2.pdf", width=3.5, height=3)
plot2
dev.off() 
#====end====

