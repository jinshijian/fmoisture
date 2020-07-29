# This script creates moisture function comparison across clay gradient

rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)
# Create a transparent theme object
transparent_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA))


mpdata <- read.csv(file = 'datasets_mp.csv')

#===constructing the dataset spanning RS range for modeling====
soil<- select(filter(mpdata,ra_s1==1), c(id, rs,mvol, mwp, porosity,clay, bd, org, ra, thaS, thaR, alpha1, n1, m1 )) #unique soil

rs1=0.1
mvol1<-soil$porosity*rs1
soil1<-soil
soil1$rs<-rs1
soil1$mvol<-mvol1

rs2=0.2
mvol2<-soil$porosity*rs2
soil2<-soil
soil2$rs<-rs2
soil2$mvol<-mvol2

rs3=0.3
mvol3<-soil$porosity*rs3
soil3<-soil
soil3$rs<-rs3
soil3$mvol<-mvol3

rs4=0.4
mvol4<-soil$porosity*rs4
soil4<-soil
soil4$rs<-rs4
soil4$mvol<-mvol4

rs5=0.5
mvol5<-soil$porosity*rs5
soil5<-soil
soil5$rs<-rs5
soil5$mvol<-mvol5

rs6=0.6
mvol6<-soil$porosity*rs6
soil6<-soil
soil6$rs<-rs6
soil6$mvol<-mvol6

rs7=0.7
mvol7<-soil$porosity*rs7
soil7<-soil
soil7$rs<-rs7
soil7$mvol<-mvol7

rs8=0.8
mvol8<-soil$porosity*rs8
soil8<-soil
soil8$rs<-rs8
soil8$mvol<-mvol8

rs9=0.9
mvol9<-soil$porosity*rs9
soil9<-soil
soil9$rs<-rs9
soil9$mvol<-mvol9

rs10=1
mvol10<-soil$porosity*rs10
soil10<-soil
soil10$rs<-rs10
soil10$mvol<-mvol10


soilscale<-rbind(soil1, soil2, soil3, soil4, soil5, soil6, soil7, soil8, soil9, soil10)
left<-(soilscale$rs-soilscale$thaR)/(soilscale$thaS-soilscale$thaR)
mp<-left^(1/soilscale$n)/soilscale$a
soilscale$mwp <- mp
write.csv(soilscale, "soil_rs_gradient.csv")
#======================================Yan 2018===================================
soilscale$yan_a<-2.8*soilscale$clay-0.046
which(soilscale$id =="LERB_CL6")  #set param_a to 1 when clay>0.37
soilscale$yan_a[which(soilscale$id =="LERB_CL6")]<-1

sub1<-subset(soilscale, rs<0.65)
sub2<-subset(soilscale, rs>=0.65)

fp1<-function(clay, phi, rs,theta,a){
  for (i in 1:length(clay))
  theta_opt<-0.65*phi 
  fm<-(0.1+theta_opt)/(theta+0.1)*(theta/theta_opt)^(1+2*a) 
  return(fm)}

clay<-sub1$clay
phi<-sub1$porosity
rs<-sub1$rs
theta<-sub1$mvol
a<-sub1$yan_a
pred1<-fp1(clay, phi, rs,theta,a)
sub1$model<-pred1

fp2<-function(clay, phi, rs,theta) {
  for (i in 1:length(clay))
    #if (clay>0.37) {a<-1} else { a<-1.8*clay-0.046}
    theta_opt<-0.65*phi 
  fm<-(phi-theta)^0.75/(phi-theta_opt)^0.75 
  return(fm)
}
clay<-sub2$clay
phi<-sub2$porosity
rs<-sub2$rs
theta<-sub2$mvol
pred2<-fp2(clay, phi, rs,theta)
sub2$model<-pred2

yannew<-rbind(sub1, sub2)
plot1<-ggplot(yannew, aes(x = rs,y = model, group = id)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(0,1)) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_line(aes(color=clay),size=1)+theme_bw()+
  scale_color_gradientn(colors=c("#b35806", "#fee0b6", "#542788"))+theme(text = element_text(size=16)) 
plot2<-plot1+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))#co
print(plot2)

pdf("claygra_yan.pdf", width=4.2, height=3)
plot2
dev.off() 

#=========
#fdb863 light orange
#b2abd2 light purple
#d75c06 dark orange
#642ba6 dark purple

#======Ghezzzehei model==
mpdata <- read.csv(file = 'datasets_mpomit.csv') #omitting dual vg rentention curve for simplicity 

#===constructing the dataset spanning RS range for modeling====
soil<- select(filter(mpdata,ra_s1==1), c(id, rs,mvol, mwp, porosity,clay, bd, org, ra, thaS, thaR, alpha1, n1, m1 )) #unique soil

mp1<-0.5
soil1<-soil
soil1$mp<-mp1

mp2<-1
soil2<-soil
soil2$mp<-mp2

mp3<-5
soil3<-soil
soil3$mp<-mp3

mp4<-10
soil4<-soil
soil4$mp<-mp4

mp5<-50
soil5<-soil
soil5$mp<-mp5

mp6<-100
soil6<-soil
soil6$mp<-mp6

mp7<-500
soil7<-soil
soil7$mp<-mp7

mp8<-1000
soil8<-soil
soil8$mp<-mp8

mp9<-5000
soil9<-soil
soil9$mp<-mp9

mp10<-10000
soil10<-soil
soil10$mp<-mp10

soilscale<-rbind(soil1, soil2, soil3, soil4, soil5, soil6, soil7, soil8,soil9, soil10)

m <- soilscale$m1
n <- soilscale$n1
a <- soilscale$alpha1
mwp <- soilscale$mp
thetaS<-soilscale$thaS
thetaR<-soilscale$thaR
right<- ((a*mwp)^n+1)^(-m)
mvol<- (thetaS-thetaR)*right +thetaR
poro<- soilscale$porosity
rs<-mvol/poro
soilscale$mvol<-mvol
soilscale$rs<-rs

write.csv(soilscale, "soil_mp_gradient.csv")
#===

mpdata<-read.csv("soil_mp_gradient.csv")
f<-function(phi,mvol,po) {exp(-0.00021*phi)*(0.2+0.8*(((po-mvol)/po)^0.5))*(mvol/po)^0.5} ##Relative saturation
phi<-mpdata$mp
mvol<-mpdata$mvol
po<-mpdata$porosity

pred<-f(phi,mvol, po)
pred<-pred/0.5463
mpdata$ghe<-pred
write.csv(mpdata, "ghe_modeled.csv")



plot3<-ggplot(mpdata, aes(x = rs,y = ghe, group = id)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(0,1)) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_line(aes(color=clay),size=1)+theme_bw()+
  scale_color_gradientn(colors=c("#b35806", "#fee0b6", "#542788"))+
 theme(text = element_text(size=16)) 
plot4<-plot3+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))#co

print(plot4)

pdf("claygra_ghe.pdf", width=4.2, height=3)
plot4
dev.off() 

#=====Observational data=====
obv<-read.csv("datasets.csv")

plot5<-ggplot(obv, aes(x = rs,y = ra, group = id)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(0,1)) +
  scale_y_continuous(name = 'Relative activity ',limits = c(0,1)) +
  geom_point(aes(color=clay),size=1.6, shape=1,stroke=1)+theme_bw()+
  scale_color_gradientn(colors=c("#b35806", "#fee0b6", "#542788"))+theme(text = element_text(size=16)) 
plot6<-plot5+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))#co




rs<-seq(0,1,0.05)
femp<-3.11*rs-2.42*rs^2
emp<-data.frame(rs=rs)
emp$res<-femp  #relative activity

bp<-ggplot(emp, aes(x = rs,y = femp)) +geom_line(color="black",size=1,shape=1,stroke=1)+transparent_theme
bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-0
xmin<-0
plot7<-plot6+annotation_custom(grob=bp_grob, xmin =xmin-0.05, xmax=xmin+1.1, ymin=ymin-0.05, ymax=ymin+1.05)
print(plot7)

pdf("claygra_emp.pdf", width=4.2, height=3)
plot7
dev.off() 