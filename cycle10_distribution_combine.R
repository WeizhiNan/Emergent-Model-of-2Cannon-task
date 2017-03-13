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
  p = p + geom_point(size=4)
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  p = p + ylab("Act_eq")
  p = p + xlab("cycle")
  p = p + scale_color_manual(values=c("blue", "red","green"))
  p = p + theme_classic()
  # +theme(legend.position = c(.15,.26))+theme(legend.box ="vertical")+geom_vline(xintercept=meanRT,lty=2,col="black")
  p
}

##act_eq of each unit through cycles
act_cycle_1=function(x,limit)
{ 
  x$LineGroup = paste(x$variable)
  p = ggplot(x, aes(y=value, color=variable, x=cycle))
  p = p + geom_point(size=4)
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  p = p + ylab("Act_eq")
  p = p + xlab("cycle")
  p = p + scale_color_manual(values=c("blue","yellow", "red","green"))
  #p = p + labs(title = filename)
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
data.RedUp_Cycle_all=data.frame(cycle,variable,value,Subject)
data.RedDown_Cycle_all=data.frame(cycle,variable,value,Subject)
data.BlueUp_Cycle_all=data.frame(cycle,variable,value,Subject)
data.BlueDown_Cycle_all=data.frame(cycle,variable,value,Subject)

for (sub in c(1:2)){
  # sub=1
  ##Set directory 
  pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,'/',sep='')
  pa1=paste(pa0,"test_results/",sep='')
  pa=paste("C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results/cycle10_subset_0.20/",sep='')
  data.raw<-read.table(paste(pa0,'CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  BCcor=read.table(paste(pa1,'Cycle10_B_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  RCcor=read.table(paste(pa1,'Cycle10_R_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  Tacor=read.table(paste(pa1,'Cycle10_Target_Location_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  EFORcor=read.table(paste(pa1,'Cycle10_E_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL)

  BCcor1 <- melt(BCcor, id=c("X"))
  BCcor1=BCcor1[ order(BCcor1[,1], BCcor1[,2]), ]
  a=c(BCcor1$value>0.2)
  BCcor1$value1=0
  BCcor1$value1[a]=1
  NBCcor1=sum(BCcor1$value1)
  BCcor1$value1[a]=BCcor1$value[a]
  
  RCcor1 <- melt(RCcor, id=c("X"))
  RCcor1=RCcor1[ order(RCcor1[,1], RCcor1[,2]), ]
  a=c(RCcor1$value>0.2)
  RCcor1$value1=0
  RCcor1$value1[a]=1
  NRCcor1=sum(RCcor1$value1)
  RCcor1$value1[a]=RCcor1$value[a]
  
  EFORcor1 <- melt(EFORcor, id=c("X"))
  EFORcor1=EFORcor1[ order(EFORcor1[,1], EFORcor1[,2]), ]
  a=c(EFORcor1$value>0.2)
  EFORcor1$value1=0
  EFORcor1$value1[a]=1
  NEFORcor1=sum(EFORcor1$value1)
  EFORcor1$value1[a]=EFORcor1$value[a]
  
  Tacor1 <- melt(Tacor, id=c("X"))
  Tacor1=Tacor1[ order(Tacor1[,1], Tacor1[,2]), ] 
  a=c(Tacor1$value>0.2)
  Tacor1$value1=0
  Tacor1$value1[a]=1
  NTacor1=sum(Tacor1$value1)
  Tacor1$value1[a]=Tacor1$value[a]
  
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
  
  data.raw$EFOR <- (U_1*EFORcor1[1,4] + U_2*EFORcor1[2,4] + U_3*EFORcor1[3,4] + U_4*EFORcor1[4,4] + U_5*EFORcor1[5,4] + U_6*EFORcor1[6,4] + U_7*EFORcor1[7,4] + U_8*EFORcor1[8,4] + U_9*EFORcor1[9,4] + U_10*EFORcor1[10,4]
                    + U_11*EFORcor1[11,4] + U_12*EFORcor1[12,4] + U_13*EFORcor1[13,4] + U_14*EFORcor1[14,4] + U_15*EFORcor1[15,4] + U_16*EFORcor1[16,4] + U_17*EFORcor1[17,4] + U_18*EFORcor1[18,4] + U_19*EFORcor1[19,4] + U_20*EFORcor1[20,4]
                    + U_21*EFORcor1[21,4] + U_22*EFORcor1[22,4] + U_23*EFORcor1[23,4] + U_24*EFORcor1[24,4] + U_25*EFORcor1[25,4] + U_26*EFORcor1[26,4] + U_27*EFORcor1[27,4] + U_28*EFORcor1[28,4] + U_29*EFORcor1[29,4] + U_30*EFORcor1[30,4]
                    + U_31*EFORcor1[31,4] + U_32*EFORcor1[32,4] + U_33*EFORcor1[33,4] + U_34*EFORcor1[34,4] + U_35*EFORcor1[35,4] + U_36*EFORcor1[36,4] + U_37*EFORcor1[37,4] + U_38*EFORcor1[38,4] + U_39*EFORcor1[39,4] + U_40*EFORcor1[40,4]
                    + U_41*EFORcor1[41,4] + U_42*EFORcor1[42,4] + U_43*EFORcor1[43,4] + U_44*EFORcor1[44,4] + U_45*EFORcor1[45,4] + U_46*EFORcor1[46,4] + U_47*EFORcor1[47,4] + U_48*EFORcor1[48,4] + U_49*EFORcor1[49,4] + U_50*EFORcor1[50,4]
                    + U_51*EFORcor1[51,4] + U_52*EFORcor1[52,4] + U_53*EFORcor1[53,4] + U_54*EFORcor1[54,4] + U_55*EFORcor1[55,4] + U_56*EFORcor1[56,4] + U_57*EFORcor1[57,4] + U_58*EFORcor1[58,4] + U_59*EFORcor1[59,4] + U_60*EFORcor1[60,4]
                    + U_61*EFORcor1[61,4] + U_62*EFORcor1[62,4] + U_63*EFORcor1[63,4] + U_64*EFORcor1[64,4] + U_65*EFORcor1[65,4] + U_66*EFORcor1[66,4] + U_67*EFORcor1[67,4] + U_68*EFORcor1[68,4] + U_69*EFORcor1[69,4] + U_70*EFORcor1[70,4]
                    + U_71*EFORcor1[71,4] + U_72*EFORcor1[72,4] + U_73*EFORcor1[73,4] + U_74*EFORcor1[74,4] + U_75*EFORcor1[75,4] + U_76*EFORcor1[76,4] + U_77*EFORcor1[77,4] + U_78*EFORcor1[78,4] + U_79*EFORcor1[79,4] + U_80*EFORcor1[80,4]
                    + U_81*EFORcor1[81,4] + U_82*EFORcor1[82,4] + U_83*EFORcor1[83,4] + U_84*EFORcor1[84,4] + U_85*EFORcor1[85,4] + U_86*EFORcor1[86,4] + U_87*EFORcor1[87,4] + U_88*EFORcor1[88,4] + U_89*EFORcor1[89,4] + U_90*EFORcor1[90,4]
                    + U_91*EFORcor1[91,4] + U_92*EFORcor1[92,4] + U_93*EFORcor1[93,4] + U_94*EFORcor1[94,4] + U_95*EFORcor1[95,4] + U_96*EFORcor1[96,4] + U_97*EFORcor1[97,4] + U_98*EFORcor1[98,4] + U_99*EFORcor1[99,4] + U_100*EFORcor1[100,4])/100
  
  
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
  detach(data.raw)
  print(sub)
}  
  
  
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red"))
  data.salience_RedTarget=subset(data.raw,a)
  data.salience_RedTarget_cycle=ddply(data.salience_RedTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(Ta))
  data.salience_RedTarget_cycle <- melt(data.salience_RedTarget_cycle, id=c("cycle"))
  data.salience_RedTarget_cycle$Subject=sub
  
  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue") 
  data.salience_BlueTarget=subset(data.raw,a)
  data.salience_BlueTarget_cycle=ddply(data.salience_BlueTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(Ta))
  data.salience_BlueTarget_cycle <- melt(data.salience_BlueTarget_cycle, id=c("cycle"))
  data.salience_BlueTarget_cycle$Subject=sub
  
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue"))
  data.Nonsalience_BlueTarget=subset(data.raw,a) 
  data.Nonsalience_BlueTarget_cycle=ddply(data.Nonsalience_BlueTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(Ta))
  data.Nonsalience_BlueTarget_cycle <- melt(data.Nonsalience_BlueTarget_cycle, id=c("cycle"))
  data.Nonsalience_BlueTarget_cycle$Subject=sub
  
  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")  
  data.Nonsalience_RedTarget=subset(data.raw,a)
  data.Nonsalience_RedTarget_cycle=ddply(data.Nonsalience_RedTarget,.(cycle),summarize,BC=mean(BC),RC=mean(RC),Ta=mean(Ta))
  data.Nonsalience_RedTarget_cycle <- melt(data.Nonsalience_RedTarget_cycle, id=c("cycle"))
  data.Nonsalience_RedTarget_cycle$Subject=sub
  
  
  data.salience_RedTarget_cycle_all=rbind(data.salience_RedTarget_cycle_all,data.salience_RedTarget_cycle)
  data.salience_BlueTarget_cycle_all=rbind(data.salience_BlueTarget_cycle_all,data.salience_BlueTarget_cycle)
  data.Nonsalience_RedTarget_cycle_all=rbind(data.Nonsalience_RedTarget_cycle_all,data.Nonsalience_RedTarget_cycle)
  data.Nonsalience_BlueTarget_cycle_all=rbind(data.Nonsalience_BlueTarget_cycle_all,data.Nonsalience_BlueTarget_cycle)
  
  
  a=c(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6")
  data.trial=subset(data.raw,a)
  data.trial=subset(data.trial,CannonAngle==180)
  a=c(data.trial$Orientation=="L"|data.trial$Orientation=="R")
  data.trial=subset(data.trial,!a)
  a=c(data.trial$Orientation=="U"|data.trial$Orientation=="UR"|data.trial$Orientation=="UL")
  data.trial$TCOri[a]="Up"
  a=c(data.trial$Orientation=="D"|data.trial$Orientation=="DR"|data.trial$Orientation=="DL")
  data.trial$TCOri[a]="Down"
  
  data.trial$ColorTCOri=paste(data.trial$TargetColor,data.trial$TCOri,sep='_')
  
  data.RedUp=subset(data.trial,ColorTCOri=="Red_Up")
  data.RedUp_Cycle=ddply(data.RedUp,.(cycle),summarize,BC=mean(BC),RC=mean(RC),EFOR=mean(EFOR),Ta=mean(Ta))
  data.RedUp_Cycle <- melt(data.RedUp_Cycle, id=c("cycle"))
  data.RedUp_Cycle$Subject=sub
  
  
  data.RedDown=subset(data.trial,ColorTCOri=="Red_Down")
  data.RedDown_Cycle=ddply(data.RedDown,.(cycle),summarize,BC=mean(BC),RC=mean(RC),EFOR=mean(EFOR),Ta=mean(Ta))
  data.RedDown_Cycle <- melt(data.RedDown_Cycle, id=c("cycle"))
  data.RedDown_Cycle$Subject=sub
  
  data.BlueUp=subset(data.trial,ColorTCOri=="Blue_Up")
  data.BlueUp_Cycle=ddply(data.BlueUp,.(cycle),summarize,BC=mean(BC),RC=mean(RC),EFOR=mean(EFOR),Ta=mean(Ta))
  data.BlueUp_Cycle <- melt(data.BlueUp_Cycle, id=c("cycle"))
  data.BlueUp_Cycle$Subject=sub
  
  
  data.BlueDown=subset(data.trial,ColorTCOri=="Blue_Down")
  data.BlueDown_Cycle=ddply(data.BlueDown,.(cycle),summarize,BC=mean(BC),RC=mean(RC),EFOR=mean(EFOR),Ta=mean(Ta))
  data.BlueDown_Cycle <- melt(data.BlueDown_Cycle, id=c("cycle"))
  data.BlueDown_Cycle$Subject=sub
  
  
  data.RedUp_Cycle_all=rbind(data.RedUp_Cycle_all,data.RedUp_Cycle)
  data.RedDown_Cycle_all=rbind(data.RedDown_Cycle_all,data.RedDown_Cycle)
  data.BlueUp_Cycle_all=rbind(data.BlueUp_Cycle_all,data.BlueUp_Cycle)
  data.BlueDown_Cycle_all=rbind(data.BlueDown_Cycle_all,data.BlueDown_Cycle)

  print(sub)
}

data.salience_RedTarget_cycle_all=data.salience_RedTarget_cycle_all[-1,]
data.salience_BlueTarget_cycle_all=data.salience_BlueTarget_cycle_all[-1,]
data.Nonsalience_RedTarget_cycle_all=data.Nonsalience_RedTarget_cycle_all[-1,]
data.Nonsalience_BlueTarget_cycle_all=data.Nonsalience_BlueTarget_cycle_all[-1,]
data.RedUp_Cycle_all=data.RedUp_Cycle_all[-1,]
data.RedDown_Cycle_all=data.RedDown_Cycle_all[-1,]
data.BlueUp_Cycle_all=data.BlueUp_Cycle_all[-1,]
data.BlueDown_Cycle_all=data.BlueDown_Cycle_all[-1,]

data.salience_RedTarget_cycle_average=ddply(data.salience_RedTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.salience_RedTarget_cycle_average$Value.SE=data.salience_RedTarget_cycle_average$value.sd/sqrt(sub)
data.salience_BlueTarget_cycle_average=ddply(data.salience_BlueTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.salience_BlueTarget_cycle_average$Value.SE=data.salience_BlueTarget_cycle_average$value.sd/sqrt(sub)
data.Nonsalience_RedTarget_cycle_average=ddply(data.Nonsalience_RedTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.Nonsalience_RedTarget_cycle_average$Value.SE=data.Nonsalience_RedTarget_cycle_average$value.sd/sqrt(sub)
data.Nonsalience_BlueTarget_cycle_average=ddply(data.Nonsalience_BlueTarget_cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.Nonsalience_BlueTarget_cycle_average$Value.SE=data.Nonsalience_BlueTarget_cycle_average$value.sd/sqrt(sub)

data.RedUp_Cycle_average=ddply(data.RedUp_Cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.RedUp_Cycle_average$Value.SE=data.RedUp_Cycle_average$value.sd/sqrt(sub)
data.RedDown_Cycle_average=ddply(data.RedDown_Cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.RedDown_Cycle_average$Value.SE=data.RedDown_Cycle_average$value.sd/sqrt(sub)
data.BlueUp_Cycle_average=ddply(data.BlueUp_Cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.BlueUp_Cycle_average$Value.SE=data.BlueUp_Cycle_average$value.sd/sqrt(sub)
data.BlueDown_Cycle_average=ddply(data.BlueDown_Cycle_all,.(cycle,variable),summarize,value.sd=sd(value),value=mean(value))
data.BlueDown_Cycle_average$Value.SE=data.BlueDown_Cycle_average$value.sd/sqrt(sub)

p=act_cycle(data.salience_RedTarget_cycle_average)
p
ggsave(p, file=paste("Cycle10_Salience_Red_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_RedTarget_cycle_average,paste(pa,'Cycle10_data.salience_RedTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_salience_RedTarget_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.salience_RedTarget_cycle_all$cycle==i&data.salience_RedTarget_cycle_all$variable=="BC")
  data.salience_RedTarget_cycle_BC=subset(data.salience_RedTarget_cycle_all,a)
  b=c(data.salience_RedTarget_cycle_all$cycle==i&data.salience_RedTarget_cycle_all$variable=="RC")
  data.salience_RedTarget_cycle_RC=subset(data.salience_RedTarget_cycle_all,b)
  
  p=t.test(data.salience_RedTarget_cycle_BC$value,data.salience_RedTarget_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()


p=act_cycle(data.salience_BlueTarget_cycle_average)
p
ggsave(p, file=paste("Cycle10_Salience_Blue_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_BlueTarget_cycle_average,paste(pa,'Cycle10_data.salience_BlueTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_salience_BlueTarget_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.salience_BlueTarget_cycle_all$cycle==i&data.salience_BlueTarget_cycle_all$variable=="BC")
  data.salience_BlueTarget_cycle_BC=subset(data.salience_BlueTarget_cycle_all,a)
  b=c(data.salience_BlueTarget_cycle_all$cycle==i&data.salience_BlueTarget_cycle_all$variable=="RC")
  data.salience_BlueTarget_cycle_RC=subset(data.salience_BlueTarget_cycle_all,b)
  
  p=t.test(data.salience_BlueTarget_cycle_BC$value,data.salience_BlueTarget_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()




p=act_cycle(data.Nonsalience_BlueTarget_cycle_average)
p
ggsave(p, file=paste("Cycle10_nonSalience_Blue_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_BlueTarget_cycle_average,paste(pa,'/','Cycle10_data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_Nonsalience_BlueTarget_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.Nonsalience_BlueTarget_cycle_all$cycle==i&data.Nonsalience_BlueTarget_cycle_all$variable=="BC")
  data.Nonsalience_BlueTarget_cycle_BC=subset(data.Nonsalience_BlueTarget_cycle_all,a)
  b=c(data.Nonsalience_BlueTarget_cycle_all$cycle==i&data.Nonsalience_BlueTarget_cycle_all$variable=="RC")
  data.Nonsalience_BlueTarget_cycle_RC=subset(data.Nonsalience_BlueTarget_cycle_all,b)
  
  p=t.test(data.Nonsalience_BlueTarget_cycle_BC$value,data.Nonsalience_BlueTarget_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()



p=act_cycle(data.Nonsalience_RedTarget_cycle_average)
p
ggsave(p, file=paste("Cycle10_nonSalience_Red_combine.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_RedTarget_cycle_average,paste(pa,'/','Cycle10_data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_Nonsalience_RedTarget_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.Nonsalience_RedTarget_cycle_all$cycle==i&data.Nonsalience_RedTarget_cycle_all$variable=="BC")
  data.Nonsalience_RedTarget_cycle_BC=subset(data.Nonsalience_RedTarget_cycle_all,a)
  b=c(data.Nonsalience_RedTarget_cycle_all$cycle==i&data.Nonsalience_RedTarget_cycle_all$variable=="RC")
  data.Nonsalience_RedTarget_cycle_RC=subset(data.Nonsalience_RedTarget_cycle_all,b)
  
  p=t.test(data.Nonsalience_RedTarget_cycle_BC$value,data.Nonsalience_RedTarget_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()


p=act_cycle_1(data.RedUp_Cycle_average)
p
ggsave(p, file=paste("Cycle10_RedUp.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.RedUp_Cycle_average,paste(pa,'/','Cycle10_RedUp_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_RedUp_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.RedUp_Cycle_all$cycle==i&data.RedUp_Cycle_all$variable=="BC")
  data.RedUp_cycle_BC=subset(data.RedUp_Cycle_all,a)
  b=c(data.RedUp_Cycle_all$cycle==i&data.RedUp_Cycle_all$variable=="RC")
  data.RedUp_cycle_RC=subset(data.RedUp_Cycle_all,b)
  
  p=t.test(data.RedUp_cycle_BC$value,data.RedUp_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()



p=act_cycle_1(data.RedDown_Cycle_average)
p
ggsave(p, file=paste("Cycle10_RedDown.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.RedDown_Cycle_average,paste(pa,'/','Cycle10_RedDown_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_RedDown_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.RedDown_Cycle_all$cycle==i&data.RedDown_Cycle_all$variable=="BC")
  data.RedDown_cycle_BC=subset(data.RedDown_Cycle_all,a)
  b=c(data.RedDown_Cycle_all$cycle==i&data.RedDown_Cycle_all$variable=="RC")
  data.RedDown_cycle_RC=subset(data.RedDown_Cycle_all,b)
  
  p=t.test(data.RedDown_cycle_BC$value,data.RedDown_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()


p=act_cycle_1(data.BlueUp_Cycle_average)
p
ggsave(p, file=paste("Cycle10_BlueUp.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.BlueUp_Cycle_average,paste(pa,'/','Cycle10_BlueUp_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_BlueUp_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.BlueUp_Cycle_all$cycle==i&data.BlueUp_Cycle_all$variable=="BC")
  data.BlueUp_cycle_BC=subset(data.BlueUp_Cycle_all,a)
  b=c(data.BlueUp_Cycle_all$cycle==i&data.BlueUp_Cycle_all$variable=="RC")
  data.BlueUp_cycle_RC=subset(data.BlueUp_Cycle_all,b)
  
  p=t.test(data.BlueUp_cycle_BC$value,data.BlueUp_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()


p=act_cycle_1(data.BlueDown_Cycle_average)
p
ggsave(p, file=paste("Cycle10_BlueDown.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.BlueDown_Cycle_average,paste(pa,'/','Cycle10_BlueDown_cycle_average','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
sink(paste(pa,'Cycle10_BlueDown_ttest.txt',sep='/'))
for (i in 1:30){
  
  a=c(data.BlueDown_Cycle_all$cycle==i&data.BlueDown_Cycle_all$variable=="BC")
  data.BlueDown_cycle_BC=subset(data.BlueDown_Cycle_all,a)
  b=c(data.BlueDown_Cycle_all$cycle==i&data.BlueDown_Cycle_all$variable=="RC")
  data.BlueDown_cycle_RC=subset(data.BlueDown_Cycle_all,b)
  
  p=t.test(data.BlueDown_cycle_BC$value,data.BlueDown_cycle_RC$value,parid=TRUE)$p.value
  cat(paste('cycle',i,'\n'))
  cat(p)
  cat('\n')
}
sink()

