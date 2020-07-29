# This script plots moisture function across 6 different soil textures.

rm(list=ls())
library(ggplot2)
library(reshape2)
# set variable values
log_mp<-seq(-2,6,0.1) # soil matric potential at log scale (absolute value taken)
# set van Genuchten parameters for 6 different texture of soil
# 6 sols: sand/sandy loam/silt loam/sandy clay loam/clay loam/clay

mp<-10^log_mp
opt<-log10(33)

f<-function(mp,alfa,n,m) {(1+(mp*alfa)^n)^(-m)} ##Relative saturation
sand<-f(mp,0.035,3.19,0.69)
sndl<-f(mp,0.021,1.61,0.38)
sitl<-f(mp,0.012,1.39,0.28)
sclm<-f(mp,0.033,1.49,0.33)
clyl<-f(mp,0.030,1.37,0.27)
clay<-f(mp,0.021,1.20,0.17)

#data frame with 6 soils

soils<-data.frame(log_mp)
soils$sand <-sand
soils$sndl <-sndl
soils$sitl <-sitl
soils$sclm <-sclm
soils$clyl <-clyl
soils$clay <-clay

datmelt<-reshape2::melt(soils,id.vars=c("log_mp"))
#======Figure--soil retention curves across 6 different soil texture=============
plot1<-ggplot(datmelt, aes(x = log_mp,y = value, group = variable)) +
  scale_x_continuous(name = expression(paste("matric potential"))) +
  scale_y_continuous(name = 'Relative saturation ',limits = c(0,1)) +
  geom_line(aes(color=variable),size=1)+theme_bw() + 
  scale_color_brewer(palette = "PuOr")+ 
  theme(text = element_text(size=20))
plot2<-plot1+coord_flip()
print(plot2)

#============Moyano empirical with soil texutre (Moyano 2012)================
k<-seq(0,1,0.01) # moisture interval

f<-function(k) {-0.67*k+1.08*k^2-0.57*k^3+1.134} ##Relative saturation

test<-f(k)

y<-vector(length=length(test))
for (i in 1:length(y)){
  if (i==1) {y[i]<-test[i]} else {y[i]<-y[i-1]*test[i]}
}
y<-y/max(y)
#testing the math of a single 
#spinnng a range of clay content to reflect soil texture, organic carbon 0.02 g g-1, and bulk density at 1.2 gm-3
#Model 2 clay and SOC
k<-seq(0,1,0.01) # moisture interval
#cy as clay content
f<-function(k,cy) {-0.26*k+0.32*k^2-0.15*k^3+0.08*cy-0.09*k*cy+1.07} ##Relative saturation 0.57*SOC+1.059=

sand<-f(k,0.03) #sandy
sndl<-f(k,0.1) #Sandy loam
sitl<-f(k,0.13)  #Silt loam
sclm<-f(k,0.27)  #Sandy clay loam
clyl<-f(k,0.34)  #Clay loam
clay<-f(k,0.58)  #Clay

y_sand<-vector(length=length(sand))
for (i in 1:length(y)){
  if (i==1) {y_sand[i]<-sand[i]} else {y_sand[i]<-y_sand[i-1]*sand[i]}
}
y_sand<-y_sand/max(y_sand)

#plot(y_sand)

y_sndl<-vector(length=length(sndl))
for (i in 1:length(y_sndl)){
  if (i==1) {y_sndl[i]<-sndl[i]} else {y_sndl[i]<-y_sndl[i-1]*sndl[i]}
}
y_sndl<-y_sndl/max(y_sndl)

y_sitl<-vector(length=length(sitl))
for (i in 1:length(y_sitl)){
  if (i==1) {y_sitl[i]<-sitl[i]} else {y_sitl[i]<-y_sitl[i-1]*sitl[i]}
}
y_sitl<-y_sitl/max(y_sitl)


y_sclm<-vector(length=length(sclm))
for (i in 1:length(y_sclm)){
  if (i==1) {y_sclm[i]<-sclm[i]} else {y_sclm[i]<-y_sclm[i-1]*sclm[i]}
}
y_sclm<-y_sclm/max(y_sclm)

y_clyl<-vector(length=length(clyl))
for (i in 1:length(y_clyl)){
  if (i==1) {y_clyl[i]<-clyl[i]} else {y_clyl[i]<-y_clyl[i-1]*clyl[i]}
}
y_clyl<-y_clyl/max(y_clyl)

y_clay<-vector(length=length(clay))
for (i in 1:length(y_clay)){
  if (i==1) {y_clay[i]<-clay[i]} else {y_clay[i]<-y_clay[i-1]*clay[i]}
}
y_clay<-y_clay/max(y_clay)

rs<-seq(0,1,0.01)
moyano<-data.frame(rs=rs)
moyano$sand <-y_sand
moyano$sndl <-y_sndl
moyano$sitl <-y_sitl
moyano$sclm <-y_sclm
moyano$clyl <-y_clyl
moyano$clay <-y_clay

moyamelt<-reshape2::melt(moyano,id.vars=c("rs"))

plot1<-ggplot(moyamelt, aes(x = rs,y = value, group = variable)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(0,1)) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_line(aes(color=variable),size=1)+theme_bw() + 
  scale_color_brewer(palette = "PuOr")
plot2<-plot1+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))

pdf("Moyano.pdf", width=5.5, height=4)
plot2
dev.off()


#============Manzoni moisture function with water potential (Manzoni 2012)================
#sandy loam soil
f_mzni<-1-((log_mp-0.5)/(4.2-0.5))^1.47

manzoni<-data.frame(f_mzni=f_mzni)
manzoni$sand <-sand
manzoni$sndl <-sndl
manzoni$sitl <-sitl
manzoni$sclm <-sclm
manzoni$clyl <-clyl
manzoni$clay <-clay

manzonimelt<-reshape2::melt(manzoni,id.vars=c("f_mzni"))

plot3<-ggplot(manzonimelt, aes(x = f_mzni,y = value, group = variable)) +
  scale_x_continuous(name = expression(paste("Relative activity")),limits = c(0,1)) +
  scale_y_continuous(name = 'Relative saturation ',limits = c(0,1)) +
  geom_line(aes(color=variable),size=1)+theme_bw() + 
  scale_color_brewer(palette = "PuOr")

plot4<-plot3+coord_flip()+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))

pdf("Manzoni.pdf", width=5.5, height=4)
plot4
dev.off() 


#======================================Ghezzehei 2019===================================
rs<-seq(0,1,0.05) # relative water saturation

f<-function(rs,alfa,n,m) {exp(-0.00021*((rs^(-1/m)-1)^(1/n)/alfa))*(0.2+0.8*(1-rs)^0.5)*rs^0.5} ##Relative saturation
sand<-f(rs,0.035,3.19,0.69) #sandy
sndl<-f(rs,0.021,1.61,0.38) #Sandy loam
sitl<-f(rs,0.012,1.39,0.28)  #Silt loam
sclm<-f(rs,0.033,1.49,0.33)  #Sandy clay loam
clyl<-f(rs,0.030,1.37,0.27)  #Clay loam
clay<-f(rs,0.021,1.20,0.17)  #Clay

ghez<-data.frame(rs=rs)
ghez$sand <-sand
ghez$sndl <-sndl
ghez$sitl <-sitl
ghez$sclm <-sclm
ghez$clyl <-clyl
ghez$clay <-clay

ghezmelt<-reshape2::melt(ghez,id.vars=c("rs"))

plot5<-ggplot(ghezmelt, aes(x = rs,y = value, group = variable)) +
  scale_x_continuous(name = expression(paste("Relative saturation"))) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_line(aes(color=variable),size=1)+theme_bw() + 
  scale_color_brewer(palette = "PuOr")+ 
  theme(text = element_text(size=16)) 
plot6<-plot5+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))

pdf("Ghezzehei.pdf", width=5.5, height=4)
plot6
dev.off() 


#======================================Yan 2018===================================
#porosity=1-1.2/2.65=0.55

fp<-function(clay){
  if (clay>0.37) {a<-1} else { a<-2.8*clay-0.046}
  theta_opt<-0.65*0.55
  rs1<-seq(0,0.64, 0.01)
  rs2<-seq(0.65,1, 0.01)
  theta1<-0.55*rs1
  theta2<-0.55*rs2
  fm1<- (0.1+theta_opt)/(theta1+0.1)*(theta1/theta_opt)^(1+2*a)
  fm2<-(0.55-theta2)^0.75/(0.55*0.35)^0.75
  fm<-c(fm1,fm2)
  fm<-fm/max(fm)
  return(fm)
}

z_sand<-fp(0.03) #sandy
z_sndl<-fp(0.1) #Sandy loam
z_sitl<-fp(0.13)  #Silt loam
z_sclm<-fp(0.27)  #Sandy clay loam
z_clyl<-fp(0.34)  #Clay loam
z_clay<-fp(0.58)  #Clay

rs<-seq(0,1,0.01) # relative water saturation

#dataframe of individual and combined moisture response components
fyan<-data.frame(rs=rs)
fyan$sand<-z_sand
fyan$sndl<-z_sndl
fyan$sitl<-z_sitl
fyan$sclm<-z_sclm
fyan$clyl<-z_clyl
fyan$clay<-z_clay

yanmelt<-reshape2::melt(fyan,id.vars=c("rs"))


plot7<-ggplot(yanmelt, aes(x = rs,y = value, group = variable)) +
  scale_x_continuous(name = expression(paste("Relative saturation"))) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_line(aes(color=variable),size=1)+theme_bw() + 
  scale_color_brewer(palette = "PuOr")

plot8<-plot7+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))

pdf("Yan.pdf", width=5.5, height=4)
plot8
dev.off() 

#================Emperical(Moyano 2013)============================================

rs<-seq(0,1,0.05) # relative water saturation
f_mo13<-3.11*rs-2.42*rs^2
data1_moyano2012<-data.frame(rs=rs)
data1_moyano2012$ra<-f_mo13  #relative activity

plot9<-ggplot(data1_moyano2012, aes(x = rs,y = ra)) +
  scale_x_continuous(name = expression(paste("Relative saturation "))) +
  scale_y_continuous(name = 'Relative activity ') +
  geom_line(aes(),color="gray50",size=1)
plot10<-plot9+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))

pdf("Moyano13review.pdf", width=4.5, height=4)
plot10
dev.off() 


