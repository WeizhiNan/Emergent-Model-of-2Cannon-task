## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)


##act_eq of each unit through cycles
act_cycle=function(x,limit)
{ 
  x$LineGroup = paste(x$variable)
  p = ggplot(x, aes(y=value, color=variable, x=cycle))
  ##画上点和errorbar，设置点的大小，添加errorbar
  p = p + geom_point(size=4)
  ##画上线条
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  ##添加横纵坐标
  p = p + ylab("Act_eq")
  p = p + xlab("cycle")
  #   ##设置线条对应的颜色属???
  p = p + scale_color_manual(values=c("blue", "red","green"))
  ##添加标题
  #p = p + labs(title = filename)
  ##采用白背景，黑线主题
  p = p + theme_classic()
  # +theme(legend.position = c(.15,.26))+theme(legend.box ="vertical")+geom_vline(xintercept=meanRT,lty=2,col="black")
  
  p
  
}

cycle=c(100)
variable=c(100)
value=c(100)
Subject=c(100)


data.salience_RedTarget_cycle_all=data.frame(cycle,variable,value,Subject)
data.salience_BlueTarget_cycle_all=data.frame(cycle,variable,value,Subject)
data.Nonsalience_RedTarget_cycle_all=data.frame(cycle,variable,value,Subject)
data.Nonsalience_BlueTarget_cycle_all=data.frame(cycle,variable,value,Subject)




for (sub in c(1:30)){
  # sub=17
  ##Set directory 
  pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,'/',sep='')
  pa1=paste(pa0,"test_results/",sep='')
  pa=paste("C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results/subset_0.15_colortarget/",sep='')
  data.raw<-read.table(paste(pa0,'CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  BCcor=read.table(paste(pa1,'B_Orientation_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  RCcor=read.table(paste(pa1,'R_Orientation_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  Tacor=read.table(paste(pa1,'Target_Location_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  BTacor=read.table(paste(pa1,'Blue_Target_Location_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  RTacor=read.table(paste(pa1,'Red_Target_Location_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  BCcor1 <- melt(BCcor, id=c("X"))
  BCcor1=BCcor1[ order(BCcor1[,1], BCcor1[,2]), ]
  a=c(BCcor1$value>0.15)
  BCcor1$value1=0
  BCcor1$value1[a]=BCcor1$value[a]
  
  RCcor1 <- melt(RCcor, id=c("X"))
  RCcor1=RCcor1[ order(RCcor1[,1], RCcor1[,2]), ]
  a=c(RCcor1$value>0.15)
  RCcor1$value1=0
  RCcor1$value1[a]=RCcor1$value[a]
  
  Tacor1 <- melt(Tacor, id=c("X"))
  Tacor1=Tacor1[ order(Tacor1[,1], Tacor1[,2]), ] 
  a=c(Tacor1$value>0.15)
  Tacor1$value1=0
  Tacor1$value1[a]=Tacor1$value[a]
  
  BTacor1 <- melt(BTacor, id=c("X"))
  BTacor1=BTacor1[ order(BTacor1[,1], BTacor1[,2]), ] 
  a=c(BTacor1$value>0.15)
  BTacor1$value1=0
  BTacor1$value1[a]=BTacor1$value[a]
  
  RTacor1 <- melt(RTacor, id=c("X"))
  RTacor1=Tacor1[ order(RTacor1[,1], RTacor1[,2]), ] 
  a=c(RTacor1$value>0.15)
  RTacor1$value1=0
  RTacor1$value1[a]=RTacor1$value[a]
  
  
  data.raw$cycle=data.raw$cycle+1  
  a=c(data.raw$cycle==100)
  b=data.raw$trial[a]
  for(i in (1:length(b)))
  {  data.raw<-data.raw[(data.raw$trial!=b[i]),]}
  
  data.raw=subset(data.raw,cycle<=30)
  for (n in c(0:99))
  {names(data.raw)[names(data.raw)==paste("Hidden_act_eq_",n,sep='')] <- paste('U_',n+1,sep='')}
  
  attach(data.raw)
  data.raw$BC <- (U_1*BCcor1[1,4] + U_2*BCcor1[2,4] + U_3*BCcor1[3,4] + U_4*BCcor1[4,4] + U_5*BCcor1[5,4] + U_6*BCcor1[6,4] + U_7*BCcor1[7,4] + U_8*BCcor1[8,4] + U_9*BCcor1[9,4] + U_10*BCcor1[10,4]
                  + U_11*BCcor1[11,4] + U_12*BCcor1[12,4] + U_13*BCcor1[13,4] + U_14*BCcor1[14,4] + U_15*BCcor1[15,4] + U_16*BCcor1[16,4] + U_17*BCcor1[17,4] + U_18*BCcor1[18,4] + U_19*BCcor1[19,4] + U_20*BCcor1[20,4]
                  + U_21*BCcor1[21,4] + U_22*BCcor1[22,4] + U_23*BCcor1[23,4] + U_24*BCcor1[24,4] + U_25*BCcor1[25,4] + U_26*BCcor1[26,4] + U_27*BCcor1[27,4] + U_28*BCcor1[28,4] + U_29*BCcor1[29,4] + U_30*BCcor1[30,4]
                  + U_31*BCcor1[31,4] + U_32*BCcor1[32,4] + U_33*BCcor1[33,4] + U_34*BCcor1[34,4] + U_35*BCcor1[35,4] + U_36*BCcor1[36,4] + U_37*BCcor1[37,4] + U_38*BCcor1[38,4] + U_39*BCcor1[39,4] + U_40*BCcor1[40,4]
                  + U_41*BCcor1[41,4] + U_42*BCcor1[42,4] + U_43*BCcor1[43,4] + U_44*BCcor1[44,4] + U_45*BCcor1[45,4] + U_46*BCcor1[46,4] + U_47*BCcor1[47,4] + U_48*BCcor1[48,4] + U_49*BCcor1[49,4] + U_50*BCcor1[50,4]
                  + U_51*BCcor1[51,4] + U_52*BCcor1[52,4] + U_53*BCcor1[53,4] + U_54*BCcor1[54,4] + U_55*BCcor1[55,4] + U_56*BCcor1[56,4] + U_57*BCcor1[57,4] + U_58*BCcor1[58,4] + U_59*BCcor1[59,4] + U_60*BCcor1[60,4]
                  + U_61*BCcor1[61,4] + U_62*BCcor1[62,4] + U_63*BCcor1[63,4] + U_64*BCcor1[64,4] + U_65*BCcor1[65,4] + U_66*BCcor1[66,4] + U_67*BCcor1[67,4] + U_68*BCcor1[68,4] + U_69*BCcor1[69,4] + U_70*BCcor1[70,4]
                  + U_71*BCcor1[71,4] + U_72*BCcor1[72,4] + U_73*BCcor1[73,4] + U_74*BCcor1[74,4] + U_75*BCcor1[75,4] + U_76*BCcor1[76,4] + U_77*BCcor1[77,4] + U_78*BCcor1[78,4] + U_79*BCcor1[79,4] + U_80*BCcor1[80,4]
                  + U_81*BCcor1[81,4] + U_82*BCcor1[82,4] + U_83*BCcor1[83,4] + U_84*BCcor1[84,4] + U_85*BCcor1[85,4] + U_86*BCcor1[86,4] + U_87*BCcor1[87,4] + U_88*BCcor1[88,4] + U_89*BCcor1[89,4] + U_90*BCcor1[90,4]
                  + U_91*BCcor1[91,4] + U_92*BCcor1[92,4] + U_93*BCcor1[93,4] + U_94*BCcor1[94,4] + U_95*BCcor1[95,4] + U_96*BCcor1[96,4] + U_97*BCcor1[97,4] + U_98*BCcor1[98,4] + U_99*BCcor1[99,4] + U_100*BCcor1[100,4])/100
  
  data.raw$RC <- (U_1*RCcor1[1,4] + U_2*RCcor1[2,4] + U_3*RCcor1[3,4] + U_4*RCcor1[4,4] + U_5*RCcor1[5,4] + U_6*RCcor1[6,4] + U_7*RCcor1[7,4] + U_8*RCcor1[8,4] + U_9*RCcor1[9,4] + U_10*RCcor1[10,4]
                  + U_11*RCcor1[11,4] + U_12*RCcor1[12,4] + U_13*RCcor1[13,4] + U_14*RCcor1[14,4] + U_15*RCcor1[15,4] + U_16*RCcor1[16,4] + U_17*RCcor1[17,4] + U_18*RCcor1[18,4] + U_19*RCcor1[19,4] + U_20*RCcor1[20,4]
                  + U_21*RCcor1[21,4] + U_22*RCcor1[22,4] + U_23*RCcor1[23,4] + U_24*RCcor1[24,4] + U_25*RCcor1[25,4] + U_26*RCcor1[26,4] + U_27*RCcor1[27,4] + U_28*RCcor1[28,4] + U_29*RCcor1[29,4] + U_30*RCcor1[30,4]
                  + U_31*RCcor1[31,4] + U_32*RCcor1[32,4] + U_33*RCcor1[33,4] + U_34*RCcor1[34,4] + U_35*RCcor1[35,4] + U_36*RCcor1[36,4] + U_37*RCcor1[37,4] + U_38*RCcor1[38,4] + U_39*RCcor1[39,4] + U_40*RCcor1[40,4]
                  + U_41*RCcor1[41,4] + U_42*RCcor1[42,4] + U_43*RCcor1[43,4] + U_44*RCcor1[44,4] + U_45*RCcor1[45,4] + U_46*RCcor1[46,4] + U_47*RCcor1[47,4] + U_48*RCcor1[48,4] + U_49*RCcor1[49,4] + U_50*RCcor1[50,4]
                  + U_51*RCcor1[51,4] + U_52*RCcor1[52,4] + U_53*RCcor1[53,4] + U_54*RCcor1[54,4] + U_55*RCcor1[55,4] + U_56*RCcor1[56,4] + U_57*RCcor1[57,4] + U_58*RCcor1[58,4] + U_59*RCcor1[59,4] + U_60*RCcor1[60,4]
                  + U_61*RCcor1[61,4] + U_62*RCcor1[62,4] + U_63*RCcor1[63,4] + U_64*RCcor1[64,4] + U_65*RCcor1[65,4] + U_66*RCcor1[66,4] + U_67*RCcor1[67,4] + U_68*RCcor1[68,4] + U_69*RCcor1[69,4] + U_70*RCcor1[70,4]
                  + U_71*RCcor1[71,4] + U_72*RCcor1[72,4] + U_73*RCcor1[73,4] + U_74*RCcor1[74,4] + U_75*RCcor1[75,4] + U_76*RCcor1[76,4] + U_77*RCcor1[77,4] + U_78*RCcor1[78,4] + U_79*RCcor1[79,4] + U_80*RCcor1[80,4]
                  + U_81*RCcor1[81,4] + U_82*RCcor1[82,4] + U_83*RCcor1[83,4] + U_84*RCcor1[84,4] + U_85*RCcor1[85,4] + U_86*RCcor1[86,4] + U_87*RCcor1[87,4] + U_88*RCcor1[88,4] + U_89*RCcor1[89,4] + U_90*RCcor1[90,4]
                  + U_91*RCcor1[91,4] + U_92*RCcor1[92,4] + U_93*RCcor1[93,4] + U_94*RCcor1[94,4] + U_95*RCcor1[95,4] + U_96*RCcor1[96,4] + U_97*RCcor1[97,4] + U_98*RCcor1[98,4] + U_99*RCcor1[99,4] + U_100*RCcor1[100,4])/100
  
  data.raw$Ta <- (U_1*Tacor1[1,4] + U_2*Tacor1[2,4] + U_3*Tacor1[3,4] + U_4*Tacor1[4,4] + U_5*Tacor1[5,4] + U_6*Tacor1[6,4] + U_7*Tacor1[7,4] + U_8*Tacor1[8,4] + U_9*Tacor1[9,4] + U_10*Tacor1[10,4]
                  + U_11*Tacor1[11,4] + U_12*Tacor1[12,4] + U_13*Tacor1[13,4] + U_14*Tacor1[14,4] + U_15*Tacor1[15,4] + U_16*Tacor1[16,4] + U_17*Tacor1[17,4] + U_18*Tacor1[18,4] + U_19*Tacor1[19,4] + U_20*Tacor1[20,4]
                  + U_21*Tacor1[21,4] + U_22*Tacor1[22,4] + U_23*Tacor1[23,4] + U_24*Tacor1[24,4] + U_25*Tacor1[25,4] + U_26*Tacor1[26,4] + U_27*Tacor1[27,4] + U_28*Tacor1[28,4] + U_29*Tacor1[29,4] + U_30*Tacor1[30,4]
                  + U_31*Tacor1[31,4] + U_32*Tacor1[32,4] + U_33*Tacor1[33,4] + U_34*Tacor1[34,4] + U_35*Tacor1[35,4] + U_36*Tacor1[36,4] + U_37*Tacor1[37,4] + U_38*Tacor1[38,4] + U_39*Tacor1[39,4] + U_40*Tacor1[40,4]
                  + U_41*Tacor1[41,4] + U_42*Tacor1[42,4] + U_43*Tacor1[43,4] + U_44*Tacor1[44,4] + U_45*Tacor1[45,4] + U_46*Tacor1[46,4] + U_47*Tacor1[47,4] + U_48*Tacor1[48,4] + U_49*Tacor1[49,4] + U_50*Tacor1[50,4]
                  + U_51*Tacor1[51,4] + U_52*Tacor1[52,4] + U_53*Tacor1[53,4] + U_54*Tacor1[54,4] + U_55*Tacor1[55,4] + U_56*Tacor1[56,4] + U_57*Tacor1[57,4] + U_58*Tacor1[58,4] + U_59*Tacor1[59,4] + U_60*Tacor1[60,4]
                  + U_61*Tacor1[61,4] + U_62*Tacor1[62,4] + U_63*Tacor1[63,4] + U_64*Tacor1[64,4] + U_65*Tacor1[65,4] + U_66*Tacor1[66,4] + U_67*Tacor1[67,4] + U_68*Tacor1[68,4] + U_69*Tacor1[69,4] + U_70*Tacor1[70,4]
                  + U_71*Tacor1[71,4] + U_72*Tacor1[72,4] + U_73*Tacor1[73,4] + U_74*Tacor1[74,4] + U_75*Tacor1[75,4] + U_76*Tacor1[76,4] + U_77*Tacor1[77,4] + U_78*Tacor1[78,4] + U_79*Tacor1[79,4] + U_80*Tacor1[80,4]
                  + U_81*Tacor1[81,4] + U_82*Tacor1[82,4] + U_83*Tacor1[83,4] + U_84*Tacor1[84,4] + U_85*Tacor1[85,4] + U_86*Tacor1[86,4] + U_87*Tacor1[87,4] + U_88*Tacor1[88,4] + U_89*Tacor1[89,4] + U_90*Tacor1[90,4]
                  + U_91*Tacor1[91,4] + U_92*Tacor1[92,4] + U_93*Tacor1[93,4] + U_94*Tacor1[94,4] + U_95*Tacor1[95,4] + U_96*Tacor1[96,4] + U_97*Tacor1[97,4] + U_98*Tacor1[98,4] + U_99*Tacor1[99,4] + U_100*Tacor1[100,4])/100
  
  data.raw$BTa <- (U_1*BTacor1[1,4] + U_2*BTacor1[2,4] + U_3*BTacor1[3,4] + U_4*BTacor1[4,4] + U_5*BTacor1[5,4] + U_6*BTacor1[6,4] + U_7*BTacor1[7,4] + U_8*BTacor1[8,4] + U_9*BTacor1[9,4] + U_10*BTacor1[10,4]
                  + U_11*BTacor1[11,4] + U_12*BTacor1[12,4] + U_13*BTacor1[13,4] + U_14*BTacor1[14,4] + U_15*BTacor1[15,4] + U_16*BTacor1[16,4] + U_17*BTacor1[17,4] + U_18*BTacor1[18,4] + U_19*BTacor1[19,4] + U_20*BTacor1[20,4]
                  + U_21*BTacor1[21,4] + U_22*BTacor1[22,4] + U_23*BTacor1[23,4] + U_24*BTacor1[24,4] + U_25*BTacor1[25,4] + U_26*BTacor1[26,4] + U_27*BTacor1[27,4] + U_28*BTacor1[28,4] + U_29*BTacor1[29,4] + U_30*BTacor1[30,4]
                  + U_31*BTacor1[31,4] + U_32*BTacor1[32,4] + U_33*BTacor1[33,4] + U_34*BTacor1[34,4] + U_35*BTacor1[35,4] + U_36*BTacor1[36,4] + U_37*BTacor1[37,4] + U_38*BTacor1[38,4] + U_39*BTacor1[39,4] + U_40*BTacor1[40,4]
                  + U_41*BTacor1[41,4] + U_42*BTacor1[42,4] + U_43*BTacor1[43,4] + U_44*BTacor1[44,4] + U_45*BTacor1[45,4] + U_46*BTacor1[46,4] + U_47*BTacor1[47,4] + U_48*BTacor1[48,4] + U_49*BTacor1[49,4] + U_50*BTacor1[50,4]
                  + U_51*BTacor1[51,4] + U_52*BTacor1[52,4] + U_53*BTacor1[53,4] + U_54*BTacor1[54,4] + U_55*BTacor1[55,4] + U_56*BTacor1[56,4] + U_57*BTacor1[57,4] + U_58*BTacor1[58,4] + U_59*BTacor1[59,4] + U_60*BTacor1[60,4]
                  + U_61*BTacor1[61,4] + U_62*BTacor1[62,4] + U_63*BTacor1[63,4] + U_64*BTacor1[64,4] + U_65*BTacor1[65,4] + U_66*BTacor1[66,4] + U_67*BTacor1[67,4] + U_68*BTacor1[68,4] + U_69*BTacor1[69,4] + U_70*BTacor1[70,4]
                  + U_71*BTacor1[71,4] + U_72*BTacor1[72,4] + U_73*BTacor1[73,4] + U_74*BTacor1[74,4] + U_75*BTacor1[75,4] + U_76*BTacor1[76,4] + U_77*BTacor1[77,4] + U_78*BTacor1[78,4] + U_79*BTacor1[79,4] + U_80*BTacor1[80,4]
                  + U_81*BTacor1[81,4] + U_82*BTacor1[82,4] + U_83*BTacor1[83,4] + U_84*BTacor1[84,4] + U_85*BTacor1[85,4] + U_86*BTacor1[86,4] + U_87*BTacor1[87,4] + U_88*BTacor1[88,4] + U_89*BTacor1[89,4] + U_90*BTacor1[90,4]
                  + U_91*BTacor1[91,4] + U_92*BTacor1[92,4] + U_93*BTacor1[93,4] + U_94*BTacor1[94,4] + U_95*BTacor1[95,4] + U_96*BTacor1[96,4] + U_97*BTacor1[97,4] + U_98*BTacor1[98,4] + U_99*BTacor1[99,4] + U_100*BTacor1[100,4])/100
  
  data.raw$RTa <- (U_1*RTacor1[1,4] + U_2*RTacor1[2,4] + U_3*RTacor1[3,4] + U_4*RTacor1[4,4] + U_5*RTacor1[5,4] + U_6*RTacor1[6,4] + U_7*RTacor1[7,4] + U_8*RTacor1[8,4] + U_9*RTacor1[9,4] + U_10*RTacor1[10,4]
                   + U_11*RTacor1[11,4] + U_12*RTacor1[12,4] + U_13*RTacor1[13,4] + U_14*RTacor1[14,4] + U_15*RTacor1[15,4] + U_16*RTacor1[16,4] + U_17*RTacor1[17,4] + U_18*RTacor1[18,4] + U_19*RTacor1[19,4] + U_20*RTacor1[20,4]
                   + U_21*RTacor1[21,4] + U_22*RTacor1[22,4] + U_23*RTacor1[23,4] + U_24*RTacor1[24,4] + U_25*RTacor1[25,4] + U_26*RTacor1[26,4] + U_27*RTacor1[27,4] + U_28*RTacor1[28,4] + U_29*RTacor1[29,4] + U_30*RTacor1[30,4]
                   + U_31*RTacor1[31,4] + U_32*RTacor1[32,4] + U_33*RTacor1[33,4] + U_34*RTacor1[34,4] + U_35*RTacor1[35,4] + U_36*RTacor1[36,4] + U_37*RTacor1[37,4] + U_38*RTacor1[38,4] + U_39*RTacor1[39,4] + U_40*RTacor1[40,4]
                   + U_41*RTacor1[41,4] + U_42*RTacor1[42,4] + U_43*RTacor1[43,4] + U_44*RTacor1[44,4] + U_45*RTacor1[45,4] + U_46*RTacor1[46,4] + U_47*RTacor1[47,4] + U_48*RTacor1[48,4] + U_49*RTacor1[49,4] + U_50*RTacor1[50,4]
                   + U_51*RTacor1[51,4] + U_52*RTacor1[52,4] + U_53*RTacor1[53,4] + U_54*RTacor1[54,4] + U_55*RTacor1[55,4] + U_56*RTacor1[56,4] + U_57*RTacor1[57,4] + U_58*RTacor1[58,4] + U_59*RTacor1[59,4] + U_60*RTacor1[60,4]
                   + U_61*RTacor1[61,4] + U_62*RTacor1[62,4] + U_63*RTacor1[63,4] + U_64*RTacor1[64,4] + U_65*RTacor1[65,4] + U_66*RTacor1[66,4] + U_67*RTacor1[67,4] + U_68*RTacor1[68,4] + U_69*RTacor1[69,4] + U_70*RTacor1[70,4]
                   + U_71*RTacor1[71,4] + U_72*RTacor1[72,4] + U_73*RTacor1[73,4] + U_74*RTacor1[74,4] + U_75*RTacor1[75,4] + U_76*RTacor1[76,4] + U_77*RTacor1[77,4] + U_78*RTacor1[78,4] + U_79*RTacor1[79,4] + U_80*RTacor1[80,4]
                   + U_81*RTacor1[81,4] + U_82*RTacor1[82,4] + U_83*RTacor1[83,4] + U_84*RTacor1[84,4] + U_85*RTacor1[85,4] + U_86*RTacor1[86,4] + U_87*RTacor1[87,4] + U_88*RTacor1[88,4] + U_89*RTacor1[89,4] + U_90*RTacor1[90,4]
                   + U_91*RTacor1[91,4] + U_92*RTacor1[92,4] + U_93*RTacor1[93,4] + U_94*RTacor1[94,4] + U_95*RTacor1[95,4] + U_96*RTacor1[96,4] + U_97*RTacor1[97,4] + U_98*RTacor1[98,4] + U_99*RTacor1[99,4] + U_100*RTacor1[100,4])/100
  
  
  detach(data.raw)
  
  
  
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red"))
  data.salience_RedTarget=subset(data.raw,a)
  data.salience_RedTarget_cycle=ddply(data.salience_RedTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(RTa))
  data.salience_RedTarget_cycle <- melt(data.salience_RedTarget_cycle, id=c("cycle"))
  data.salience_RedTarget_cycle$Subject=sub
  
  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue") 
  data.salience_BlueTarget=subset(data.raw,a)
  data.salience_BlueTarget_cycle=ddply(data.salience_BlueTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(BTa))
  data.salience_BlueTarget_cycle <- melt(data.salience_BlueTarget_cycle, id=c("cycle"))
  data.salience_BlueTarget_cycle$Subject=sub
  
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue"))
  data.Nonsalience_BlueTarget=subset(data.raw,a) 
  data.Nonsalience_BlueTarget_cycle=ddply(data.Nonsalience_BlueTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(BTa))
  data.Nonsalience_BlueTarget_cycle <- melt(data.Nonsalience_BlueTarget_cycle, id=c("cycle"))
  data.Nonsalience_BlueTarget_cycle$Subject=sub
  
  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")  
  data.Nonsalience_RedTarget=subset(data.raw,a)
  data.Nonsalience_RedTarget_cycle=ddply(data.Nonsalience_RedTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(RTa))
  data.Nonsalience_RedTarget_cycle <- melt(data.Nonsalience_RedTarget_cycle, id=c("cycle"))
  data.Nonsalience_RedTarget_cycle$Subject=sub
  
  
  data.salience_RedTarget_cycle_all=rbind(data.salience_RedTarget_cycle_all,data.salience_RedTarget_cycle)
  data.salience_BlueTarget_cycle_all=rbind(data.salience_BlueTarget_cycle_all,data.salience_BlueTarget_cycle)
  data.Nonsalience_RedTarget_cycle_all=rbind(data.Nonsalience_RedTarget_cycle_all,data.Nonsalience_RedTarget_cycle)
  data.Nonsalience_BlueTarget_cycle_all=rbind(data.Nonsalience_BlueTarget_cycle_all,data.Nonsalience_BlueTarget_cycle)
  
}
data.salience_RedTarget_cycle_all=data.salience_RedTarget_cycle_all[-1,]
data.salience_BlueTarget_cycle_all=data.salience_BlueTarget_cycle_all[-1,]
data.Nonsalience_RedTarget_cycle_all=data.Nonsalience_RedTarget_cycle_all[-1,]
data.Nonsalience_BlueTarget_cycle_all=data.Nonsalience_BlueTarget_cycle_all[-1,]

data.salience_RedTarget_cycle_average=ddply(data.salience_RedTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.salience_RedTarget_cycle_average$Value.SE=data.salience_RedTarget_cycle_average$value.sd/sqrt(sub)
data.salience_BlueTarget_cycle_average=ddply(data.salience_BlueTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.salience_BlueTarget_cycle_average$Value.SE=data.salience_BlueTarget_cycle_average$value.sd/sqrt(sub)
data.Nonsalience_RedTarget_cycle_average=ddply(data.Nonsalience_RedTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.Nonsalience_RedTarget_cycle_average$Value.SE=data.Nonsalience_RedTarget_cycle_average$value.sd/sqrt(sub)
data.Nonsalience_BlueTarget_cycle_average=ddply(data.Nonsalience_BlueTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.Nonsalience_BlueTarget_cycle_average$Value.SE=data.Nonsalience_BlueTarget_cycle_average$value.sd/sqrt(sub)


p=act_cycle(data.salience_RedTarget_cycle_average)
p
ggsave(p, file=paste("Salience_Red_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_RedTarget_cycle_average,paste(pa,'/','data.salience_RedTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")



p=act_cycle(data.salience_BlueTarget_cycle_average)
p
ggsave(p, file=paste("Salience_Blue_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_BlueTarget_cycle_average,paste(pa,'/','data.salience_BlueTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")

p=act_cycle(data.Nonsalience_BlueTarget_cycle_average)
p
ggsave(p, file=paste("nonSalience_Blue_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_BlueTarget_cycle_average,paste(pa,'/','data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")

p=act_cycle(data.Nonsalience_RedTarget_cycle_average)
p
ggsave(p, file=paste("nonSalience_Red_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_RedTarget_cycle_average,paste(pa,'/','data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
