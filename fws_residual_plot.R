# This script generates residual plots by comparing 5 moistiure functions (fws) against observational data
rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)
library("PerformanceAnalytics")

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


franz <- read.csv(file = 'datasets.csv')
unique(franz$id)

#=======Moyano 2013 empirical expression=====
rs<-franz$rs
femp<-3.11*rs-2.42*rs^2
emp<-data.frame(rs=rs)
emp$res<-femp  #relative activity


ll<-(emp[,2]-franz[,13])#/franz[,11]
res_emp<-data.frame(rs=rs)
res_emp$res<-ll
#==R^2 coefficient of determination====
ave<-mean(franz$ra)
diff<-emp$res-franz$ra
ssres<-sum(diff^2)
sstot<-sum((franz$ra-ave)^2)  
RR<-1-ssres/sstot

simple.fit = lm(res~rs, data=res_emp)
summary(simple.fit)

#xy-plot, then add confidence interval with polygon function
plot1<-ggplot(res_emp, aes(x = rs,y = res)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(-0.2,1)) +
  scale_y_continuous(name = 'Residual',limits = c(-1,1))+geom_point(col="#642ba6",pch=1,  cex=1.6)+geom_line(y=0, color="gray50")+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="sienna1", se=TRUE)
plot2<-plot1+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))

bp<-ggplot(res_emp, aes(x = rs,y = res)) +geom_boxplot(width =0.2, color="gray50",outlier.shape = 1)+transparent_theme

bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-min(res_emp$res)
ymax<-max(res_emp$res)
xmin<--0.1
xmax<-1
plot3<-plot2+annotation_custom(grob=bp_grob, xmin =xmin-0.1, xmax=xmin+0.1, ymin=ymin, ymax=ymax)
#plot2<-plot1+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=20))
print(plot3)

pdf("Diff_empirical_bp.pdf", width=3.5, height=3)
plot3
dev.off() 

#=======Moyano soil texture model=====

f<-function(k, soc,cy){-0.26*k+0.32*k^2-0.15*k^3+0.08*cy-0.09*k*cy+0.57*soc+1.059}# {-0.29*k+0.37*k^2-0.19*k^3+0.03*bd+0.09*cy-0.08*k*cy+0.8*soc+1.02}

rs<-franz$rs
org<-franz$org
clay<-franz$clay
bd<- franz$bd

pred<-f(rs, org, clay)
ll<-(pred-franz$ra)#/franz[,11]

res_emp<-data.frame(rs=rs)
res_emp$res<-ll


ave<-mean(franz$ra)
diff<-pred-franz$ra
ssres<-sum(diff^2)
sstot<-sum((franz$ra-ave)^2)  
RR<-1-ssres/sstot

simple.fit = lm(res~rs, data=res_emp)
summary(simple.fit)

#xy-plot, then add confidence interval with polygon function
plot4<-ggplot(res_emp, aes(x = rs,y = res)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(-0.2,1)) +
  scale_y_continuous(name = 'Residual',limits = c(-1,1))+geom_point(col="#642ba6", pch=1 , cex=1.6)+geom_line(y=0, color="gray50")+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="tomato1", se=TRUE)
 

plot5<-plot4+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))

bp<-ggplot(res_emp, aes(x = rs,y = res)) +geom_boxplot(width =0.2, color="gray50",outlier.shape = 1)+transparent_theme

bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-min(res_emp$res)
ymax<-max(res_emp$res)
xmin<--0.1
xmax<-1
plot6<-plot5+annotation_custom(grob=bp_grob, xmin =xmin-0.1, xmax=xmin+0.1, ymin=ymin, ymax=ymax)
print(plot6)

pdf("Diff_moyano_bp.pdf", width=3.5, height=3)
plot6
dev.off() 
#======end=

#======Manzoni water potential model==

#set alpha=2 (shape function), represents the commonly used log-linear response curve
#essentially this function is a linear relationship between water potential and microbial activity

f<-function(po) {1-(log(po/33)/log(15800/33))^2}
mwp<-franz$mwp
rs<-franz$rs
pred<-f(mwp)

ll<-(pred-franz$ra)#/franz[,11]

res_emp<-data.frame(rs=rs)
res_emp$res<-ll

ave<-mean(franz$ra)
diff<-pred-franz$ra
ssres<-sum(diff^2)
sstot<-sum((franz$ra-ave)^2)  
RR<-1-ssres/sstot

simple.fit = lm(res~rs, data=res_emp)
summary(simple.fit)

#xy-plot, then add confidence interval with polygon function
plot7<-ggplot(res_emp, aes(x = rs,y = res)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(-0.2,1)) +
  scale_y_continuous(name = 'Residual',limits = c(-1,1))+geom_point(col="#642ba6", pch=1 , cex=1.6)+geom_line(y=0, color="gray50")+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="tomato1", se=TRUE)

plot8<-plot7+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))

bp<-ggplot(res_emp, aes(x = rs,y = res)) +geom_boxplot(width =0.2, color="gray50",outlier.shape = 1)+transparent_theme

bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-min(res_emp$res)
ymax<-max(res_emp$res)
xmin<--0.1
xmax<-1
plot9<-plot8+annotation_custom(grob=bp_grob, xmin =xmin-0.1, xmax=xmin+0.1, ymin=ymin, ymax=ymax)
print(plot9)

pdf("Diff_manzoni_bp.pdf", width=3.5, height=3)
plot9
dev.off() 
#======end=

#======Ghezzzehei model==
f<-function(rs,phi) {exp(-0.00021*phi)*((1-rs)^0.5)*(rs^0.5)} #adjusted Kamin=0
rs<-franz$rs
phi<-franz$mwp
pred<-f(rs, phi)
#found max of pred
franz$gheraw<-pred
write.csv(franz,"ghe_raw.csv")
#scale ghe function predictions using scaling_ghe.R
ghe<-read.csv("ghe_scaled.csv")

ll<-(ghe$scaled_ghe-ghe$ra)#/franz[,11]
res_emp<-data.frame(rs=rs)
res_emp$res<-ll


ave<-mean(ghe$ra)
ssres<-sum(ll^2)
sstot<-sum((ghe$ra-ave)^2)  
RR<-1-ssres/sstot

simple.fit = lm(res~rs, data=res_emp)
summary(simple.fit)

#xy-plot, then add confidence interval with polygon function
plot10<-ggplot(res_emp, aes(x = rs,y = res)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(-0.2,1)) +
  scale_y_continuous(name = 'Residual',limits = c(-1,1))+geom_point(col="#642ba6", pch=1 , cex=1.6)+geom_line(y=0, color="gray50")+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="tomato1", se=TRUE)

plot11<-plot10+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))

bp<-ggplot(res_emp, aes(x = rs,y = res)) +geom_boxplot(width =0.2, color="gray50",outlier.shape = 1)+transparent_theme

bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-min(res_emp$res)
ymax<-max(res_emp$res)
xmin<--0.1
xmax<-1
plot12<-plot11+annotation_custom(grob=bp_grob, xmin =xmin-0.1, xmax=xmin+0.1, ymin=ymin, ymax=ymax)
print(plot12)


pdf("Diff_ghezz_bp.pdf", width=3.5, height=3)
plot12
dev.off() 


#======================================Yan 2018===================================
#porosity=1-1.2/2.65=0.55
sub1<-subset(franz, rs<0.65)
sub2<-subset(franz,rs>=0.65)

fp1<-function(clay, phi, rs,theta){
  for (i in 1:length(clay))
  #if (clay>0.37) {a<-1} else { a<-1.8*clay-0.046}
  a<-0.8*clay-0.046 
  theta_opt<-0.65*phi 
  fm<-(0.1+theta_opt)/(theta+0.1)*(theta/theta_opt)^(1+2*a) 
  return(fm)}

clay<-sub1$clay
phi<-sub1$porosity
rs<-sub1$rs
theta<-sub1$mvol
pred1<-fp1(clay, phi, rs,theta)
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

yan<-rbind(sub1, sub2)
rs<-yan$rs
ll<-(yan$model-yan$ra)#/franz[,11]
res_emp<-data.frame(rs=rs)
res_emp$res<-ll


ave<-mean(yan$ra)
ssres<-sum(ll^2)
sstot<-sum((yan$ra-ave)^2)  
RR<-1-ssres/sstot

simple.fit = lm(res~rs, data=res_emp)
summary(simple.fit)

#xy-plot, then add confidence interval with polygon function
plot13<-ggplot(res_emp, aes(x = rs,y = res)) +
  scale_x_continuous(name = expression(paste("Relative saturation")),limits = c(-0.2,1)) +
  scale_y_continuous(name = 'Residual',limits = c(-1,1))+geom_point(col="#642ba6", pch=1 , cex=1.6)+geom_line(y=0, color="gray50")+
  geom_smooth(method=lm, color="tomato1", size=0.5, fill="tomato1", se=TRUE)

plot14<-plot13+theme_linedraw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size=16))

bp<-ggplot(res_emp, aes(x = rs,y = res)) +geom_boxplot(width =0.2, color="gray50",outlier.shape = 1)+transparent_theme

bp_grob<-ggplotGrob(bp)#create the external graphcal elements (called a grop in Grid terminology)
ymin<-min(res_emp$res)
ymax<-max(res_emp$res)
xmin<--0.1
xmax<-1
plot15<-plot14+annotation_custom(grob=bp_grob, xmin =xmin-0.1, xmax=xmin+0.1, ymin=ymin, ymax=ymax)
print(plot15)

pdf("Diff_yan_bp.pdf", width=3.5, height=3)
plot15
dev.off() 
#====end====

