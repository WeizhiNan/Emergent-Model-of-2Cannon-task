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
  pa=paste("C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results/combine/",sep='')
  data.raw<-read.table(paste(pa0,'CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  BCcor=read.table(paste(pa1,'B_Orientation_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  RCcor=read.table(paste(pa1,'R_Orientation_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  Tacor=read.table(paste(pa1,'Target_Location_test0.25.csv',sep=''),header=TRUE,sep=",",na.string=NULL)


  BCcor1 <- melt(BCcor, id=c("X"))
  BCcor1=BCcor1[ order(BCcor1[,1], BCcor1[,2]), ]
  
  RCcor1 <- melt(RCcor, id=c("X"))
  RCcor1=RCcor1[ order(RCcor1[,1], RCcor1[,2]), ]
  
  Tacor1 <- melt(Tacor, id=c("X"))
  Tacor1=Tacor1[ order(Tacor1[,1], Tacor1[,2]), ] 
  

  
  data.raw$cycle=data.raw$cycle+1  
  a=c(data.raw$cycle==100)
  b=data.raw$trial[a]
  for(i in (1:length(b)))
  {  data.raw<-data.raw[(data.raw$trial!=b[i]),]}
 
   data.raw=subset(data.raw,cycle<=30)
    for (n in c(0:99))
        {names(data.raw)[names(data.raw)==paste("Hidden_act_eq_",n,sep='')] <- paste('U_',n+1,sep='')}

   attach(data.raw)
   data.raw$BC <- (U_1*BCcor1[1,3] + U_2*BCcor1[2,3] + U_3*BCcor1[3,3] + U_4*BCcor1[4,3] + U_5*BCcor1[5,3] + U_6*BCcor1[6,3] + U_7*BCcor1[7,3] + U_8*BCcor1[8,3] + U_9*BCcor1[9,3] + U_10*BCcor1[10,3]
                   + U_11*BCcor1[11,3] + U_12*BCcor1[12,3] + U_13*BCcor1[13,3] + U_14*BCcor1[14,3] + U_15*BCcor1[15,3] + U_16*BCcor1[16,3] + U_17*BCcor1[17,3] + U_18*BCcor1[18,3] + U_19*BCcor1[19,3] + U_20*BCcor1[20,3]
                   + U_21*BCcor1[21,3] + U_22*BCcor1[22,3] + U_23*BCcor1[23,3] + U_24*BCcor1[24,3] + U_25*BCcor1[25,3] + U_26*BCcor1[26,3] + U_27*BCcor1[27,3] + U_28*BCcor1[28,3] + U_29*BCcor1[29,3] + U_30*BCcor1[30,3]
                   + U_31*BCcor1[31,3] + U_32*BCcor1[32,3] + U_33*BCcor1[33,3] + U_34*BCcor1[34,3] + U_35*BCcor1[35,3] + U_36*BCcor1[36,3] + U_37*BCcor1[37,3] + U_38*BCcor1[38,3] + U_39*BCcor1[39,3] + U_40*BCcor1[40,3]
                   + U_41*BCcor1[41,3] + U_42*BCcor1[42,3] + U_43*BCcor1[43,3] + U_44*BCcor1[44,3] + U_45*BCcor1[45,3] + U_46*BCcor1[46,3] + U_47*BCcor1[47,3] + U_48*BCcor1[48,3] + U_49*BCcor1[49,3] + U_50*BCcor1[50,3]
                   + U_51*BCcor1[51,3] + U_52*BCcor1[52,3] + U_53*BCcor1[53,3] + U_54*BCcor1[54,3] + U_55*BCcor1[55,3] + U_56*BCcor1[56,3] + U_57*BCcor1[57,3] + U_58*BCcor1[58,3] + U_59*BCcor1[59,3] + U_60*BCcor1[60,3]
                   + U_61*BCcor1[61,3] + U_62*BCcor1[62,3] + U_63*BCcor1[63,3] + U_64*BCcor1[64,3] + U_65*BCcor1[65,3] + U_66*BCcor1[66,3] + U_67*BCcor1[67,3] + U_68*BCcor1[68,3] + U_69*BCcor1[69,3] + U_70*BCcor1[70,3]
                   + U_71*BCcor1[71,3] + U_72*BCcor1[72,3] + U_73*BCcor1[73,3] + U_74*BCcor1[74,3] + U_75*BCcor1[75,3] + U_76*BCcor1[76,3] + U_77*BCcor1[77,3] + U_78*BCcor1[78,3] + U_79*BCcor1[79,3] + U_80*BCcor1[80,3]
                   + U_81*BCcor1[81,3] + U_82*BCcor1[82,3] + U_83*BCcor1[83,3] + U_84*BCcor1[84,3] + U_85*BCcor1[85,3] + U_86*BCcor1[86,3] + U_87*BCcor1[87,3] + U_88*BCcor1[88,3] + U_89*BCcor1[89,3] + U_90*BCcor1[90,3]
                   + U_91*BCcor1[91,3] + U_92*BCcor1[92,3] + U_93*BCcor1[93,3] + U_94*BCcor1[94,3] + U_95*BCcor1[95,3] + U_96*BCcor1[96,3] + U_97*BCcor1[97,3] + U_98*BCcor1[98,3] + U_99*BCcor1[99,3] + U_100*BCcor1[100,3])/100
  
   data.raw$RC <- (U_1*RCcor1[1,3] + U_2*RCcor1[2,3] + U_3*RCcor1[3,3] + U_4*RCcor1[4,3] + U_5*RCcor1[5,3] + U_6*RCcor1[6,3] + U_7*RCcor1[7,3] + U_8*RCcor1[8,3] + U_9*RCcor1[9,3] + U_10*RCcor1[10,3]
                   + U_11*RCcor1[11,3] + U_12*RCcor1[12,3] + U_13*RCcor1[13,3] + U_14*RCcor1[14,3] + U_15*RCcor1[15,3] + U_16*RCcor1[16,3] + U_17*RCcor1[17,3] + U_18*RCcor1[18,3] + U_19*RCcor1[19,3] + U_20*RCcor1[20,3]
                   + U_21*RCcor1[21,3] + U_22*RCcor1[22,3] + U_23*RCcor1[23,3] + U_24*RCcor1[24,3] + U_25*RCcor1[25,3] + U_26*RCcor1[26,3] + U_27*RCcor1[27,3] + U_28*RCcor1[28,3] + U_29*RCcor1[29,3] + U_30*RCcor1[30,3]
                   + U_31*RCcor1[31,3] + U_32*RCcor1[32,3] + U_33*RCcor1[33,3] + U_34*RCcor1[34,3] + U_35*RCcor1[35,3] + U_36*RCcor1[36,3] + U_37*RCcor1[37,3] + U_38*RCcor1[38,3] + U_39*RCcor1[39,3] + U_40*RCcor1[40,3]
                   + U_41*RCcor1[41,3] + U_42*RCcor1[42,3] + U_43*RCcor1[43,3] + U_44*RCcor1[44,3] + U_45*RCcor1[45,3] + U_46*RCcor1[46,3] + U_47*RCcor1[47,3] + U_48*RCcor1[48,3] + U_49*RCcor1[49,3] + U_50*RCcor1[50,3]
                   + U_51*RCcor1[51,3] + U_52*RCcor1[52,3] + U_53*RCcor1[53,3] + U_54*RCcor1[54,3] + U_55*RCcor1[55,3] + U_56*RCcor1[56,3] + U_57*RCcor1[57,3] + U_58*RCcor1[58,3] + U_59*RCcor1[59,3] + U_60*RCcor1[60,3]
                   + U_61*RCcor1[61,3] + U_62*RCcor1[62,3] + U_63*RCcor1[63,3] + U_64*RCcor1[64,3] + U_65*RCcor1[65,3] + U_66*RCcor1[66,3] + U_67*RCcor1[67,3] + U_68*RCcor1[68,3] + U_69*RCcor1[69,3] + U_70*RCcor1[70,3]
                   + U_71*RCcor1[71,3] + U_72*RCcor1[72,3] + U_73*RCcor1[73,3] + U_74*RCcor1[74,3] + U_75*RCcor1[75,3] + U_76*RCcor1[76,3] + U_77*RCcor1[77,3] + U_78*RCcor1[78,3] + U_79*RCcor1[79,3] + U_80*RCcor1[80,3]
                   + U_81*RCcor1[81,3] + U_82*RCcor1[82,3] + U_83*RCcor1[83,3] + U_84*RCcor1[84,3] + U_85*RCcor1[85,3] + U_86*RCcor1[86,3] + U_87*RCcor1[87,3] + U_88*RCcor1[88,3] + U_89*RCcor1[89,3] + U_90*RCcor1[90,3]
                   + U_91*RCcor1[91,3] + U_92*RCcor1[92,3] + U_93*RCcor1[93,3] + U_94*RCcor1[94,3] + U_95*RCcor1[95,3] + U_96*RCcor1[96,3] + U_97*RCcor1[97,3] + U_98*RCcor1[98,3] + U_99*RCcor1[99,3] + U_100*RCcor1[100,3])/100
   
   data.raw$Ta <- (U_1*Tacor1[1,3] + U_2*Tacor1[2,3] + U_3*Tacor1[3,3] + U_4*Tacor1[4,3] + U_5*Tacor1[5,3] + U_6*Tacor1[6,3] + U_7*Tacor1[7,3] + U_8*Tacor1[8,3] + U_9*Tacor1[9,3] + U_10*Tacor1[10,3]
                   + U_11*Tacor1[11,3] + U_12*Tacor1[12,3] + U_13*Tacor1[13,3] + U_14*Tacor1[14,3] + U_15*Tacor1[15,3] + U_16*Tacor1[16,3] + U_17*Tacor1[17,3] + U_18*Tacor1[18,3] + U_19*Tacor1[19,3] + U_20*Tacor1[20,3]
                   + U_21*Tacor1[21,3] + U_22*Tacor1[22,3] + U_23*Tacor1[23,3] + U_24*Tacor1[24,3] + U_25*Tacor1[25,3] + U_26*Tacor1[26,3] + U_27*Tacor1[27,3] + U_28*Tacor1[28,3] + U_29*Tacor1[29,3] + U_30*Tacor1[30,3]
                   + U_31*Tacor1[31,3] + U_32*Tacor1[32,3] + U_33*Tacor1[33,3] + U_34*Tacor1[34,3] + U_35*Tacor1[35,3] + U_36*Tacor1[36,3] + U_37*Tacor1[37,3] + U_38*Tacor1[38,3] + U_39*Tacor1[39,3] + U_40*Tacor1[40,3]
                   + U_41*Tacor1[41,3] + U_42*Tacor1[42,3] + U_43*Tacor1[43,3] + U_44*Tacor1[44,3] + U_45*Tacor1[45,3] + U_46*Tacor1[46,3] + U_47*Tacor1[47,3] + U_48*Tacor1[48,3] + U_49*Tacor1[49,3] + U_50*Tacor1[50,3]
                   + U_51*Tacor1[51,3] + U_52*Tacor1[52,3] + U_53*Tacor1[53,3] + U_54*Tacor1[54,3] + U_55*Tacor1[55,3] + U_56*Tacor1[56,3] + U_57*Tacor1[57,3] + U_58*Tacor1[58,3] + U_59*Tacor1[59,3] + U_60*Tacor1[60,3]
                   + U_61*Tacor1[61,3] + U_62*Tacor1[62,3] + U_63*Tacor1[63,3] + U_64*Tacor1[64,3] + U_65*Tacor1[65,3] + U_66*Tacor1[66,3] + U_67*Tacor1[67,3] + U_68*Tacor1[68,3] + U_69*Tacor1[69,3] + U_70*Tacor1[70,3]
                   + U_71*Tacor1[71,3] + U_72*Tacor1[72,3] + U_73*Tacor1[73,3] + U_74*Tacor1[74,3] + U_75*Tacor1[75,3] + U_76*Tacor1[76,3] + U_77*Tacor1[77,3] + U_78*Tacor1[78,3] + U_79*Tacor1[79,3] + U_80*Tacor1[80,3]
                   + U_81*Tacor1[81,3] + U_82*Tacor1[82,3] + U_83*Tacor1[83,3] + U_84*Tacor1[84,3] + U_85*Tacor1[85,3] + U_86*Tacor1[86,3] + U_87*Tacor1[87,3] + U_88*Tacor1[88,3] + U_89*Tacor1[89,3] + U_90*Tacor1[90,3]
                   + U_91*Tacor1[91,3] + U_92*Tacor1[92,3] + U_93*Tacor1[93,3] + U_94*Tacor1[94,3] + U_95*Tacor1[95,3] + U_96*Tacor1[96,3] + U_97*Tacor1[97,3] + U_98*Tacor1[98,3] + U_99*Tacor1[99,3] + U_100*Tacor1[100,3])/100
   
  
                   
   detach(data.raw)
   
   
  
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
  