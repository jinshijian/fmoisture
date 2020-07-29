# clear working space
rm(list=ls())

library(ggplot2)
library(reshape2)

mpdata<-read.csv("ghe_raw.csv")
colnames(mpdata)[16]<-("pred_ghe")
mpdata$scaled_ghe<-0

#check unique id
unique(mpdata$id)

#id1-10 Valentine   wet_mid     dry_top    Int_mid   wet_btm    Int_top     Int_btm   dry_btm====
id_1<-subset(mpdata, id=="Valentine")
id_1$scaled_ghe<-id_1$pred_ghe/max(id_1$pred_ghe)

id_2<-subset(mpdata, id=="wet_mid")
id_2$scaled_ghe<-id_2$pred_ghe/max(id_2$pred_ghe)

id_3<-subset(mpdata, id=="dry_top")
id_3$scaled_ghe<-id_3$pred_ghe/max(id_3$pred_ghe)

id_4<-subset(mpdata, id=="Int_mid")
id_4$scaled_ghe<-id_4$pred_ghe/max(id_4$pred_ghe)

id_6<-subset(mpdata, id=="wet_btm")
id_6$scaled_ghe<-id_6$pred_ghe/max(id_6$pred_ghe)

id_8<-subset(mpdata, id=="Int_top")
id_8$scaled_ghe<-id_8$pred_ghe/max(id_8$pred_ghe)

id_9<-subset(mpdata, id=="Int_btm")
id_9$scaled_ghe<-id_9$pred_ghe/max(id_9$pred_ghe)

id_10<-subset(mpdata, id=="dry_btm")
id_10$scaled_ghe<-id_10$pred_ghe/max(id_10$pred_ghe)

#id11-20 Min1c       Min1   CloMin1c    CloMin1     LERB_NA1    LERB_SI2    Bro1c       Bro1        LERB_SI6    LERB_SI4====
id_11<-subset(mpdata, id=="Min1c")
id_11$scaled_ghe<-id_11$pred_ghe/max(id_11$pred_ghe)

id_12<-subset(mpdata, id=="Min1")
id_12$scaled_ghe<-id_12$pred_ghe/max(id_12$pred_ghe)

id_13<-subset(mpdata, id=="CloMin1c")
id_13$scaled_ghe<-id_13$pred_ghe/max(id_13$pred_ghe)

id_14<-subset(mpdata, id=="CloMin1")
id_14$scaled_ghe<-id_14$pred_ghe/max(id_14$pred_ghe)

id_15<-subset(mpdata, id=="LERB_NA1")
id_15$scaled_ghe<-id_15$pred_ghe/max(id_15$pred_ghe)

id_16<-subset(mpdata, id=="LERB_SI2")
id_16$scaled_ghe<-id_16$pred_ghe/max(id_16$pred_ghe)

id_17<-subset(mpdata, id=="Bro1c")
id_17$scaled_ghe<-id_17$pred_ghe/max(id_17$pred_ghe)

id_18<-subset(mpdata, id=="Bro1")
id_18$scaled_ghe<-id_18$pred_ghe/max(id_18$pred_ghe)

id_19<-subset(mpdata, id=="LERB_SI6")
id_19$scaled_ghe<-id_19$pred_ghe/max(id_19$pred_ghe)

id_20<-subset(mpdata, id=="LERB_SI4")
id_20$scaled_ghe<-id_20$pred_ghe/max(id_20$pred_ghe)

#id21-30 Min2c  Min2  Cecile  Kole   Bro2c    Bro2    LERB_NA2  Walla   CloMin2c  CloMin2==== 
id_21<-subset(mpdata, id=="Min2c")
id_21$scaled_ghe<-id_21$pred_ghe/max(id_21$pred_ghe)

id_22<-subset(mpdata, id=="Min2")
id_22$scaled_ghe<-id_22$pred_ghe/max(id_22$pred_ghe)

id_23<-subset(mpdata, id=="Cecile")
id_23$scaled_ghe<-id_23$pred_ghe/max(id_23$pred_ghe)

id_24<-subset(mpdata, id=="Kole")
id_24$scaled_ghe<-id_24$pred_ghe/max(id_24$pred_ghe)

id_25<-subset(mpdata, id=="Bro2c")
id_25$scaled_ghe<-id_25$pred_ghe/max(id_25$pred_ghe)

id_26<-subset(mpdata, id=="Bro2")
id_26$scaled_ghe<-id_26$pred_ghe/max(id_26$pred_ghe)

id_27<-subset(mpdata, id=="LERB_NA2")
id_27$scaled_ghe<-id_27$pred_ghe/max(id_27$pred_ghe)

id_28<-subset(mpdata, id=="Walla")
id_28$scaled_ghe<-id_28$pred_ghe/max(id_28$pred_ghe)

id_29<-subset(mpdata, id=="CloMin2c")
id_29$scaled_ghe<-id_29$pred_ghe/max(id_29$pred_ghe)

id_30<-subset(mpdata, id=="CloMin2")
id_30$scaled_ghe<-id_30$pred_ghe/max(id_30$pred_ghe)
#id31-40 LERB_CL2 Frederick Min3c   Min3  Clarion Min4c  Min4 Miami LERB_NA3 Bro3c====

id_31<-subset(mpdata, id=="LERB_CL2")
id_31$scaled_ghe<-id_31$pred_ghe/max(id_31$pred_ghe)

id_32<-subset(mpdata, id=="Frederick")
id_32$scaled_ghe<-id_32$pred_ghe/max(id_32$pred_ghe)

id_33<-subset(mpdata, id=="Min3c")
id_33$scaled_ghe<-id_33$pred_ghe/max(id_33$pred_ghe)

id_34<-subset(mpdata, id=="Min3")
id_34$scaled_ghe<-id_34$pred_ghe/max(id_34$pred_ghe)

id_35<-subset(mpdata, id=="Clarion")
id_35$scaled_ghe<-id_35$pred_ghe/max(id_35$pred_ghe)

id_36<-subset(mpdata, id=="Min4c")
id_36$scaled_ghe<-id_36$pred_ghe/max(id_36$pred_ghe)

id_37<-subset(mpdata, id=="Min4")
id_37$scaled_ghe<-id_37$pred_ghe/max(id_37$pred_ghe)

id_38<-subset(mpdata, id=="Miami")
id_38$scaled_ghe<-id_38$pred_ghe/max(id_38$pred_ghe)

id_39<-subset(mpdata, id=="LERB_NA3")
id_39$scaled_ghe<-id_39$pred_ghe/max(id_39$pred_ghe)

id_40<-subset(mpdata, id=="Bro3c")
id_40$scaled_ghe<-id_40$pred_ghe/max(id_40$pred_ghe)
#id41-50 Bro3        CloMin3c    CloMin3     Crider      Mohave      CloMin5c    Bro4c       CloMin5     Bro4        CloMin4c ====
id_41<-subset(mpdata, id=="Bro3")
id_41$scaled_ghe<-id_41$pred_ghe/max(id_41$pred_ghe)

id_42<-subset(mpdata, id=="CloMin3c")
id_42$scaled_ghe<-id_42$pred_ghe/max(id_42$pred_ghe)

id_43<-subset(mpdata, id=="CloMin3")
id_43$scaled_ghe<-id_43$pred_ghe/max(id_43$pred_ghe)

id_44<-subset(mpdata, id=="Crider")
id_44$scaled_ghe<-id_44$pred_ghe/max(id_44$pred_ghe)

id_45<-subset(mpdata, id=="Mohave")
id_45$scaled_ghe<-id_45$pred_ghe/max(id_45$pred_ghe)

id_46<-subset(mpdata, id=="CloMin5c")
id_46$scaled_ghe<-id_46$pred_ghe/max(id_46$pred_ghe)

id_47<-subset(mpdata, id=="Bro4c")
id_47$scaled_ghe<-id_47$pred_ghe/max(id_47$pred_ghe)

id_48<-subset(mpdata, id=="CloMin5")
id_48$scaled_ghe<-id_48$pred_ghe/max(id_48$pred_ghe)

id_49<-subset(mpdata, id=="Bro4")
id_49$scaled_ghe<-id_49$pred_ghe/max(id_49$pred_ghe)

id_50<-subset(mpdata, id=="CloMin4c")
id_50$scaled_ghe<-id_50$pred_ghe/max(id_50$pred_ghe)

#id51-60 CloMin4     LERB_CL4    FortCollins Min5c       Min5        Ah          LERB_NA4    Bro5c       Bro5        LERB_NA5    ====
id_51<-subset(mpdata, id=="CloMin4")
id_51$scaled_ghe<-id_51$pred_ghe/max(id_51$pred_ghe)

id_52<-subset(mpdata, id=="LERB_CL4")
id_52$scaled_ghe<-id_52$pred_ghe/max(id_52$pred_ghe)

id_53<-subset(mpdata, id=="FortCollins")
id_53$scaled_ghe<-id_53$pred_ghe/max(id_53$pred_ghe)

id_54<-subset(mpdata, id=="Min5c")
id_54$scaled_ghe<-id_54$pred_ghe/max(id_54$pred_ghe)

id_55<-subset(mpdata, id=="Min5")
id_55$scaled_ghe<-id_55$pred_ghe/max(id_55$pred_ghe)

id_56<-subset(mpdata, id=="Ah")
id_56$scaled_ghe<-id_56$pred_ghe/max(id_56$pred_ghe)

id_57<-subset(mpdata, id=="LERB_NA4")
id_57$scaled_ghe<-id_57$pred_ghe/max(id_57$pred_ghe)

id_58<-subset(mpdata, id=="Bro5c")
id_58$scaled_ghe<-id_58$pred_ghe/max(id_58$pred_ghe)

id_59<-subset(mpdata, id=="Bro5")
id_59$scaled_ghe<-id_59$pred_ghe/max(id_59$pred_ghe)

id_60<-subset(mpdata, id=="LERB_NA5")
id_60$scaled_ghe<-id_60$pred_ghe/max(id_60$pred_ghe)
#id61-65 LERB_CL6    Ac          LERB_NA6    Houston     Wahiawa      ====
id_61<-subset(mpdata, id=="LERB_CL6")
id_61$scaled_ghe<-id_61$pred_ghe/max(id_61$pred_ghe)

id_62<-subset(mpdata, id=="Ac")
id_62$scaled_ghe<-id_62$pred_ghe/max(id_62$pred_ghe)

id_63<-subset(mpdata, id=="LERB_NA6")
id_63$scaled_ghe<-id_63$pred_ghe/max(id_63$pred_ghe)

id_64<-subset(mpdata, id=="Houston")
id_64$scaled_ghe<-id_64$pred_ghe/max(id_64$pred_ghe)

id_65<-subset(mpdata, id=="Wahiawa")
id_65$scaled_ghe<-id_65$pred_ghe/max(id_65$pred_ghe)



ghe_scaled<-rbind(id_1,id_2,id_3,id_4,id_6,id_8,id_9,id_10,id_11,id_12,id_13,id_14,id_15,id_16,id_17,id_18,id_19,id_20,
                  id_21,id_22,id_23,id_24,id_25,id_26,id_27,id_28,id_29,id_30,id_31,id_32,id_33,id_34,id_35,id_36,id_37,id_38,id_39,id_40,
                  id_41,id_42,id_43,id_44,id_45,id_46,id_47,id_48,id_49,id_50,id_51,id_52,id_53,id_54,id_55,id_56,id_57,id_58,id_59,id_60,
                  id_61,id_62,id_63,id_64,id_65)

write.csv(ghe_scaled, "ghe_scaled.csv")

