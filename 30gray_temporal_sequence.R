## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)


##act_eq of each unit through cycles
act_cycle=function(x,meanCycle)
{ 
  x$LineGroup = paste(x$Unit,x$Subject)
  p = ggplot(x, aes(x=cycle,color=Unit,y=value))
  p=p+scale_colour_grey(start = .1, end = .1)
  p = p + geom_point(size=4,alpha=0.1)
  p = p + geom_line(aes(group=LineGroup), size=1.5,alpha=0.1)
  p = p + ylab("Act_eq")
  p = p + xlab("Time (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  p=p + geom_vline(xintercept=meanCycle,linetype = "longdash",size=1.5,col="Red")
  p=p + geom_vline(xintercept=10,linetype = "longdash",size=1.5,col="Red")
  p = p + theme_classic()+theme(legend.position="none")
  # p=p+ annotate(geom="text", x=10, y=0.1, label="Target",color="red",size=10)

  p
  
}

##act_eq of each unit through cycles
act_cycle_1=function(x,meanCycle)
{ 
  x$LineGroup = paste(x$Unit,x$Subject)
  p = ggplot(x, aes(x=cycle,color=Unit,y=value))
  p=p+scale_colour_grey(start = .1, end = .1)
  p = p + geom_point(size=4,alpha=0.1)
  p = p + geom_line(aes(group=LineGroup), size=1.5,alpha=0.1)
  p = p + ylab("Act_eq")
  p = p + xlab("Time (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  # p=p + geom_vline(xintercept=meanCycle,linetype = "longdash",size=1.5,col="Red")
  p=p + geom_vline(xintercept=10,linetype = "longdash",size=1.5,col="Red")
  p = p + theme_classic()+theme(legend.position="none")
  # p=p+ annotate(geom="text", x=10, y=0.1, label="Target",color="red",size=10)
  
  p
  
}

Salience_RedTarget_meanCycle=23.27
Salience_BlueTarget_meanCycle=23.20
NonSalience_RedTarget_meanCycle=24.47
NonSalience_BlueTarget_meanCycle=25.16



cycle=c(100)
Unit=c(100)
value=c(100)
Subject=c(100)

data.salience_RedTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.salience_BlueTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Nonsalience_RedTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Nonsalience_BlueTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.salience_cycle_all=data.frame(cycle,Unit,value,Subject)
data.nonSalience_cycle_all=data.frame(cycle,Unit,value,Subject)

data.0_salience_RedTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_salience_BlueTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_Nonsalience_RedTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_Nonsalience_BlueTarget_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_salience_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_nonSalience_cycle_all=data.frame(cycle,Unit,value,Subject)

data.180_Red_cycle_all=data.frame(cycle,Unit,value,Subject)
data.180_Blue_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_Red_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_Blue_cycle_all=data.frame(cycle,Unit,value,Subject)
data.180_cycle_all=data.frame(cycle,Unit,value,Subject)
data.0_cycle_all=data.frame(cycle,Unit,value,Subject)


data.Up_Red_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Up_Blue_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Down_Red_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Down_Blue_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Up_cycle_all=data.frame(cycle,Unit,value,Subject)
data.Down_cycle_all=data.frame(cycle,Unit,value,Subject)


for (sub in c(1:30)){
  # sub=30
  ##Set directory 
  pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,'/',sep='')
  pa=paste(pa0,"test_results",sep='')
  data.raw<-read.table(paste(pa0,'CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  data.raw$cycle=data.raw$cycle+1  
  a=c(data.raw$cycle==100)
  b=data.raw$trial[a]
  for(i in (1:length(b)))
  {data.raw<-data.raw[(data.raw$trial!=b[i]),]}
  data.raw=subset(data.raw,cycle<=26)
  for (n in c(0:99))
  {names(data.raw)[names(data.raw)==paste("Hidden_act_eq_",n,sep='')] <- paste('U_',n,sep='')}
  
  
  ###Salience_180
  a=c(data.raw$CannonAngle==180&
        ((data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue")|(data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red")) ) 
  data.salience=subset(data.raw,a)
  data.salience_cycle=ddply(data.salience,.(cycle),summarize,
                            U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                            U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                            U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                            U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                            U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                            U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                            U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                            U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                            U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                            U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.salience_cycle)[names(data.salience_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.salience_cycle)[names(data.salience_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.salience_cycle <- melt(data.salience_cycle, id=c("cycle"))
  colnames(data.salience_cycle)[2] <- "Unit"
  data.salience_cycle$Subject=sub
  ## 
  
  a=c(data.raw$CannonAngle==180&
        ((data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")|(data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue")) ) 
  data.nonSalience=subset(data.raw,a)
  data.nonSalience_cycle=ddply(data.nonSalience,.(cycle),summarize,
                               U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                               U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                               U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                               U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                               U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                               U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                               U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                               U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                               U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                               U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.nonSalience_cycle)[names(data.nonSalience_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.nonSalience_cycle)[names(data.nonSalience_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.nonSalience_cycle <- melt(data.nonSalience_cycle, id=c("cycle"))
  colnames(data.nonSalience_cycle)[2] <- "Unit"
  data.nonSalience_cycle$Subject=sub
  ##
  
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red"))
  data.salience_RedTarget=subset(data.raw,a)
  data.salience_RedTarget_cycle=ddply(data.salience_RedTarget,.(cycle),summarize,
                                      U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                      U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                      U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                      U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                      U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                      U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                      U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                      U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                      U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                      U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.salience_RedTarget_cycle)[names(data.salience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.salience_RedTarget_cycle)[names(data.salience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.salience_RedTarget_cycle <- melt(data.salience_RedTarget_cycle, id=c("cycle"))
  colnames(data.salience_RedTarget_cycle)[2] <- "Unit"
  data.salience_RedTarget_cycle$Subject=sub
  ##

  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue") 
  data.salience_BlueTarget=subset(data.raw,a)
  data.salience_BlueTarget_cycle=ddply(data.salience_BlueTarget,.(cycle),summarize,
                                       U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                       U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                       U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                       U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                       U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                       U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                       U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                       U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                       U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                       U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.salience_BlueTarget_cycle)[names(data.salience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.salience_BlueTarget_cycle)[names(data.salience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.salience_BlueTarget_cycle <- melt(data.salience_BlueTarget_cycle, id=c("cycle"))
  colnames(data.salience_BlueTarget_cycle)[2] <- "Unit"
  data.salience_BlueTarget_cycle$Subject=sub
  ##
 
  a=c((data.raw$CannonAngle==180&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue"))
  data.Nonsalience_BlueTarget=subset(data.raw,a)
  data.Nonsalience_BlueTarget_cycle=ddply(data.Nonsalience_BlueTarget,.(cycle),summarize,
                                          U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                          U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                          U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                          U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                          U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                          U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                          U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                          U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                          U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                          U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Nonsalience_BlueTarget_cycle)[names(data.Nonsalience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Nonsalience_BlueTarget_cycle)[names(data.Nonsalience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Nonsalience_BlueTarget_cycle <- melt(data.Nonsalience_BlueTarget_cycle, id=c("cycle"))
  colnames(data.Nonsalience_BlueTarget_cycle)[2] <- "Unit"
  data.Nonsalience_BlueTarget_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==180&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")  
  data.Nonsalience_RedTarget=subset(data.raw,a)
  data.Nonsalience_RedTarget_cycle=ddply(data.Nonsalience_RedTarget,.(cycle),summarize,
                                         U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                         U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                         U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                         U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                         U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                         U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                         U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                         U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                         U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                         U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Nonsalience_RedTarget_cycle)[names(data.Nonsalience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Nonsalience_RedTarget_cycle)[names(data.Nonsalience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Nonsalience_RedTarget_cycle <- melt(data.Nonsalience_RedTarget_cycle, id=c("cycle"))
  colnames(data.Nonsalience_RedTarget_cycle)[2] <- "Unit"
  data.Nonsalience_RedTarget_cycle$Subject=sub
  ##
 
  
  
  
  
  
  
  
  ###Salience_0
  a=c(data.raw$CannonAngle==0&
        ((data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue")|(data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red")) ) 
  data.0_salience=subset(data.raw,a)
  data.0_salience_cycle=ddply(data.0_salience,.(cycle),summarize,
                            U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                            U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                            U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                            U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                            U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                            U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                            U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                            U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                            U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                            U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_salience_cycle)[names(data.0_salience_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_salience_cycle)[names(data.0_salience_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_salience_cycle <- melt(data.0_salience_cycle, id=c("cycle"))
  colnames(data.0_salience_cycle)[2] <- "Unit"
  data.0_salience_cycle$Subject=sub
  ## 
  
  a=c(data.raw$CannonAngle==0&
        ((data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")|(data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue")) ) 
  data.0_nonSalience=subset(data.raw,a)
  data.0_nonSalience_cycle=ddply(data.0_nonSalience,.(cycle),summarize,
                               U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                               U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                               U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                               U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                               U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                               U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                               U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                               U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                               U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                               U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_nonSalience_cycle)[names(data.0_nonSalience_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_nonSalience_cycle)[names(data.0_nonSalience_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_nonSalience_cycle <- melt(data.0_nonSalience_cycle, id=c("cycle"))
  colnames(data.0_nonSalience_cycle)[2] <- "Unit"
  data.0_nonSalience_cycle$Subject=sub
  ##
  
  a=c((data.raw$CannonAngle==0&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Red"))
  data.0_salience_RedTarget=subset(data.raw,a)
  data.0_salience_RedTarget_cycle=ddply(data.0_salience_RedTarget,.(cycle),summarize,
                                      U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                      U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                      U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                      U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                      U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                      U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                      U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                      U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                      U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                      U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_salience_RedTarget_cycle)[names(data.0_salience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_salience_RedTarget_cycle)[names(data.0_salience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_salience_RedTarget_cycle <- melt(data.0_salience_RedTarget_cycle, id=c("cycle"))
  colnames(data.0_salience_RedTarget_cycle)[2] <- "Unit"
  data.0_salience_RedTarget_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==0&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Blue") 
  data.0_salience_BlueTarget=subset(data.raw,a)
  data.0_salience_BlueTarget_cycle=ddply(data.0_salience_BlueTarget,.(cycle),summarize,
                                       U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                       U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                       U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                       U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                       U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                       U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                       U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                       U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                       U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                       U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_salience_BlueTarget_cycle)[names(data.0_salience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_salience_BlueTarget_cycle)[names(data.0_salience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_salience_BlueTarget_cycle <- melt(data.0_salience_BlueTarget_cycle, id=c("cycle"))
  colnames(data.0_salience_BlueTarget_cycle)[2] <- "Unit"
  data.0_salience_BlueTarget_cycle$Subject=sub
  ##
  
  a=c((data.raw$CannonAngle==0&data.raw$Ratio=="2:6"&data.raw$TargetColor=="Blue"))
  data.0_Nonsalience_BlueTarget=subset(data.raw,a)
  data.0_Nonsalience_BlueTarget_cycle=ddply(data.0_Nonsalience_BlueTarget,.(cycle),summarize,
                                          U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                          U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                          U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                          U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                          U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                          U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                          U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                          U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                          U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                          U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_Nonsalience_BlueTarget_cycle)[names(data.0_Nonsalience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_Nonsalience_BlueTarget_cycle)[names(data.0_Nonsalience_BlueTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_Nonsalience_BlueTarget_cycle <- melt(data.0_Nonsalience_BlueTarget_cycle, id=c("cycle"))
  colnames(data.0_Nonsalience_BlueTarget_cycle)[2] <- "Unit"
  data.0_Nonsalience_BlueTarget_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==0&data.raw$Ratio=="6:2"&data.raw$TargetColor=="Red")  
  data.0_Nonsalience_RedTarget=subset(data.raw,a)
  data.0_Nonsalience_RedTarget_cycle=ddply(data.0_Nonsalience_RedTarget,.(cycle),summarize,
                                         U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                                         U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                                         U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                                         U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                                         U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                                         U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                                         U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                                         U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                                         U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                                         U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_Nonsalience_RedTarget_cycle)[names(data.0_Nonsalience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_Nonsalience_RedTarget_cycle)[names(data.0_Nonsalience_RedTarget_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_Nonsalience_RedTarget_cycle <- melt(data.0_Nonsalience_RedTarget_cycle, id=c("cycle"))
  colnames(data.0_Nonsalience_RedTarget_cycle)[2] <- "Unit"
  data.0_Nonsalience_RedTarget_cycle$Subject=sub
  ##
  
  ##### 
  ##### Cannon Angle 
  a=c(data.raw$CannonAngle==0&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6"))  
  data.0=subset(data.raw,a)
  data.0_cycle=ddply(data.0,.(cycle),summarize,
                       U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                       U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                       U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                       U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                       U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                       U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                       U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                       U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                       U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                       U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_cycle)[names(data.0_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_cycle)[names(data.0_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_cycle <- melt(data.0_cycle, id=c("cycle"))
  colnames(data.0_cycle)[2] <- "Unit"
  data.0_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==180&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6"))  
  data.180=subset(data.raw,a)
  data.180_cycle=ddply(data.180,.(cycle),summarize,
                       U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                       U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                       U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                       U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                       U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                       U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                       U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                       U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                       U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                       U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.180_cycle)[names(data.180_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.180_cycle)[names(data.180_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.180_cycle <- melt(data.180_cycle, id=c("cycle"))
  colnames(data.180_cycle)[2] <- "Unit"
  data.180_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==0&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6")&data.raw$TargetColor=="Red")  
  data.0_Red=subset(data.raw,a)
  data.0_Red_cycle=ddply(data.0_Red,.(cycle),summarize,
                     U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                     U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                     U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                     U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                     U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                     U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                     U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                     U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                     U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                     U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_Red_cycle)[names(data.0_Red_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_Red_cycle)[names(data.0_Red_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_Red_cycle <- melt(data.0_Red_cycle, id=c("cycle"))
  colnames(data.0_Red_cycle)[2] <- "Unit"
  data.0_Red_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==0&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6")&data.raw$TargetColor=="Blue")  
  data.0_Blue=subset(data.raw,a)
  data.0_Blue_cycle=ddply(data.0_Blue,.(cycle),summarize,
                         U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                         U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                         U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                         U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                         U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                         U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                         U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                         U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                         U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                         U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.0_Blue_cycle)[names(data.0_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.0_Blue_cycle)[names(data.0_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.0_Blue_cycle <- melt(data.0_Blue_cycle, id=c("cycle"))
  colnames(data.0_Blue_cycle)[2] <- "Unit"
  data.0_Blue_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==180&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6")&data.raw$TargetColor=="Red")  
  data.180_Red=subset(data.raw,a)
  data.180_Red_cycle=ddply(data.180_Red,.(cycle),summarize,
                         U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                         U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                         U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                         U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                         U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                         U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                         U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                         U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                         U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                         U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.180_Red_cycle)[names(data.180_Red_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.180_Red_cycle)[names(data.180_Red_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.180_Red_cycle <- melt(data.180_Red_cycle, id=c("cycle"))
  colnames(data.180_Red_cycle)[2] <- "Unit"
  data.180_Red_cycle$Subject=sub
  ##
  
  a=c(data.raw$CannonAngle==180&(data.raw$Ratio=="6:2"|data.raw$Ratio=="4:4"|data.raw$Ratio=="2:6")&data.raw$TargetColor=="Blue")  
  data.180_Blue=subset(data.raw,a)
  data.180_Blue_cycle=ddply(data.180_Blue,.(cycle),summarize,
                          U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                          U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                          U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                          U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                          U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                          U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                          U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                          U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                          U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                          U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.180_Blue_cycle)[names(data.180_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.180_Blue_cycle)[names(data.180_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.180_Blue_cycle <- melt(data.180_Blue_cycle, id=c("cycle"))
  colnames(data.180_Blue_cycle)[2] <- "Unit"
  data.180_Blue_cycle$Subject=sub
  ##

  ###
  ###Orientation
  a=c(data.raw$Orientation=="U"|data.raw$Orientation=="UL"|data.raw$Orientation=="UR")
  data.Up=subset(data.raw,a)
  a=c(data.Up$Ratio=="2:6"|data.Up$Ratio=="4:4"|data.Up$Ratio=="6:2")
  data.Up=subset(data.Up,a)
  data.Up_cycle=ddply(data.Up,.(cycle),summarize,
                               U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                               U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                               U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                               U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                               U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                               U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                               U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                               U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                               U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                               U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Up_cycle)[names(data.Up_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Up_cycle)[names(data.Up_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Up_cycle <- melt(data.Up_cycle, id=c("cycle"))
  colnames(data.Up_cycle)[2] <- "Unit"
  data.Up_cycle$Subject=sub
  ##
  
  a=c(data.raw$Orientation=="D"|data.raw$Orientation=="DL"|data.raw$Orientation=="DR")
  data.Down=subset(data.raw,a)
  a=c(data.Down$Ratio=="2:6"|data.Down$Ratio=="4:4"|data.Down$Ratio=="6:2")
  data.Down=subset(data.Down,a)
  data.Down_cycle=ddply(data.Down,.(cycle),summarize,
                      U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                      U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                      U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                      U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                      U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                      U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                      U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                      U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                      U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                      U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Down_cycle)[names(data.Down_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Down_cycle)[names(data.Down_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Down_cycle <- melt(data.Down_cycle, id=c("cycle"))
  colnames(data.Down_cycle)[2] <- "Unit"
  data.Down_cycle$Subject=sub
  ##
 
  a=c(data.raw$Orientation=="U"|data.raw$Orientation=="UL"|data.raw$Orientation=="UR")
  data.Up=subset(data.raw,a)
  a=c((data.Up$Ratio=="2:6"|data.Up$Ratio=="4:4"|data.Up$Ratio=="6:2")&(data.Up$TargetColor=="Red"))
  data.Up_Red=subset(data.Up,a)
  data.Up_Red_cycle=ddply(data.Up_Red,.(cycle),summarize,
                      U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                      U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                      U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                      U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                      U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                      U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                      U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                      U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                      U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                      U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Up_Red_cycle)[names(data.Up_Red_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Up_Red_cycle)[names(data.Up_Red_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Up_Red_cycle <- melt(data.Up_Red_cycle, id=c("cycle"))
  colnames(data.Up_Red_cycle)[2] <- "Unit"
  data.Up_Red_cycle$Subject=sub
  
  ##
  a=c(data.raw$Orientation=="U"|data.raw$Orientation=="UL"|data.raw$Orientation=="UR")
  data.Up=subset(data.raw,a)
  a=c((data.Up$Ratio=="2:6"|data.Up$Ratio=="4:4"|data.Up$Ratio=="6:2")&(data.Up$TargetColor=="Blue"))
  data.Up_Blue=subset(data.Up,a)
  data.Up_Blue_cycle=ddply(data.Up_Blue,.(cycle),summarize,
                          U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                          U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                          U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                          U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                          U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                          U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                          U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                          U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                          U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                          U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Up_Blue_cycle)[names(data.Up_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Up_Blue_cycle)[names(data.Up_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Up_Blue_cycle <- melt(data.Up_Blue_cycle, id=c("cycle"))
  colnames(data.Up_Blue_cycle)[2] <- "Unit"
  data.Up_Blue_cycle$Subject=sub
  ##
  
  a=c(data.raw$Orientation=="D"|data.raw$Orientation=="DL"|data.raw$Orientation=="DR")
  data.Down=subset(data.raw,a)
  a=c((data.Down$Ratio=="2:6"|data.Down$Ratio=="4:4"|data.Down$Ratio=="6:2")&(data.Down$TargetColor=="Red"))
  data.Down_Red=subset(data.Down,a)
  data.Down_Red_cycle=ddply(data.Down_Red,.(cycle),summarize,
                        U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                        U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                        U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                        U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                        U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                        U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                        U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                        U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                        U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                        U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Down_Red_cycle)[names(data.Down_Red_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Down_Red_cycle)[names(data.Down_Red_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Down_Red_cycle <- melt(data.Down_Red_cycle, id=c("cycle"))
  colnames(data.Down_Red_cycle)[2] <- "Unit"
  data.Down_Red_cycle$Subject=sub
  ##
  
  a=c(data.raw$Orientation=="D"|data.raw$Orientation=="DL"|data.raw$Orientation=="DR")
  data.Down=subset(data.raw,a)
  a=c((data.Down$Ratio=="2:6"|data.Down$Ratio=="4:4"|data.Down$Ratio=="6:2")&(data.Down$TargetColor=="Blue"))
  data.Down_Blue=subset(data.Down,a)
  data.Down_Blue_cycle=ddply(data.Down_Blue,.(cycle),summarize,
                            U_0=mean(U_0),U_1=mean(U_1),U_2=mean(U_2),U_3=mean(U_3),U_4=mean(U_4),U_5=mean(U_5),U_6=mean(U_6),U_7=mean(U_7),U_8=mean(U_8),U_9=mean(U_9),U_10=mean(U_10),
                            U_11=mean(U_11),U_12=mean(U_12),U_13=mean(U_13),U_14=mean(U_14),U_15=mean(U_15),U_16=mean(U_16),U_17=mean(U_17),U_18=mean(U_18),U_19=mean(U_19),U_20=mean(U_20),
                            U_21=mean(U_21),U_22=mean(U_22),U_23=mean(U_23),U_24=mean(U_24),U_25=mean(U_25),U_26=mean(U_26),U_27=mean(U_27),U_28=mean(U_28),U_29=mean(U_29),U_30=mean(U_30),
                            U_31=mean(U_31),U_32=mean(U_32),U_33=mean(U_33),U_34=mean(U_34),U_35=mean(U_35),U_36=mean(U_36),U_37=mean(U_37),U_38=mean(U_38),U_39=mean(U_39),U_40=mean(U_40),  
                            U_41=mean(U_41),U_42=mean(U_42),U_43=mean(U_43),U_44=mean(U_44),U_45=mean(U_45),U_46=mean(U_46),U_47=mean(U_47),U_48=mean(U_48),U_49=mean(U_49),U_50=mean(U_50),
                            U_51=mean(U_51),U_52=mean(U_52),U_53=mean(U_53),U_54=mean(U_54),U_55=mean(U_55),U_56=mean(U_56),U_57=mean(U_57),U_58=mean(U_58),U_59=mean(U_59),U_60=mean(U_60),
                            U_61=mean(U_61),U_62=mean(U_62),U_63=mean(U_63),U_64=mean(U_64),U_65=mean(U_65),U_66=mean(U_66),U_67=mean(U_67),U_68=mean(U_68),U_69=mean(U_69),U_70=mean(U_70),
                            U_71=mean(U_71),U_72=mean(U_72),U_73=mean(U_73),U_74=mean(U_74),U_75=mean(U_75),U_76=mean(U_76),U_77=mean(U_77),U_78=mean(U_78),U_79=mean(U_79),U_80=mean(U_80),
                            U_81=mean(U_81),U_82=mean(U_82),U_83=mean(U_83),U_84=mean(U_84),U_85=mean(U_85),U_86=mean(U_86),U_87=mean(U_87),U_88=mean(U_88),U_89=mean(U_89),U_90=mean(U_90),  
                            U_91=mean(U_91),U_92=mean(U_92),U_93=mean(U_93),U_94=mean(U_94),U_95=mean(U_95),U_96=mean(U_96),U_97=mean(U_97),U_98=mean(U_98),U_99=mean(U_99))
  for (n in c(0:99))
  {
    if (n <10) 
    {names(data.Down_Blue_cycle)[names(data.Down_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.Down_Blue_cycle)[names(data.Down_Blue_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.Down_Blue_cycle <- melt(data.Down_Blue_cycle, id=c("cycle"))
  colnames(data.Down_Blue_cycle)[2] <- "Unit"
  data.Down_Blue_cycle$Subject=sub
  ##
  
  
  
  
   
  data.salience_cycle_all=rbind(data.salience_cycle_all,data.salience_cycle)
  data.nonSalience_cycle_all=rbind(data.nonSalience_cycle_all,data.nonSalience_cycle)
  data.salience_RedTarget_cycle_all=rbind(data.salience_RedTarget_cycle_all,data.salience_RedTarget_cycle)
  data.salience_BlueTarget_cycle_all=rbind(data.salience_BlueTarget_cycle_all,data.salience_BlueTarget_cycle)
  data.Nonsalience_BlueTarget_cycle_all=rbind(data.Nonsalience_BlueTarget_cycle_all,data.Nonsalience_BlueTarget_cycle)
  data.Nonsalience_RedTarget_cycle_all=rbind(data.Nonsalience_RedTarget_cycle_all,data.Nonsalience_RedTarget_cycle)
  
  data.0_salience_cycle_all=rbind(data.0_salience_cycle_all,data.0_salience_cycle)
  data.0_nonSalience_cycle_all=rbind(data.0_nonSalience_cycle_all,data.0_nonSalience_cycle)
  data.0_salience_RedTarget_cycle_all=rbind(data.0_salience_RedTarget_cycle_all,data.0_salience_RedTarget_cycle)
  data.0_salience_BlueTarget_cycle_all=rbind(data.0_salience_BlueTarget_cycle_all,data.0_salience_BlueTarget_cycle)
  data.0_Nonsalience_BlueTarget_cycle_all=rbind(data.0_Nonsalience_BlueTarget_cycle_all,data.0_Nonsalience_BlueTarget_cycle)
  data.0_Nonsalience_RedTarget_cycle_all=rbind(data.0_Nonsalience_RedTarget_cycle_all,data.0_Nonsalience_RedTarget_cycle)
  
  
  data.180_cycle_all=rbind(data.180_cycle_all,data.180_cycle)
  data.0_cycle_all=rbind(data.0_cycle_all,data.0_cycle)
  data.180_Red_cycle_all=rbind(data.180_Red_cycle_all,data.180_Red_cycle)
  data.180_Blue_cycle_all=rbind(data.180_Blue_cycle_all,data.180_Blue_cycle)
  data.0_Red_cycle_all=rbind(data.0_Red_cycle_all,data.0_Red_cycle)
  data.0_Blue_cycle_all=rbind(data.0_Blue_cycle_all,data.0_Blue_cycle)

  data.Up_cycle_all=rbind(data.Up_cycle_all,data.Up_cycle)
  data.Down_cycle_all=rbind(data.Down_cycle_all,data.Down_cycle)
  data.Up_Red_cycle_all=rbind(data.Up_Red_cycle_all,data.Up_Red_cycle)
  data.Up_Blue_cycle_all=rbind(data.Up_Blue_cycle_all,data.Up_Blue_cycle)
  data.Down_Red_cycle_all=rbind(data.Down_Red_cycle_all,data.Down_Red_cycle)
  data.Down_Blue_cycle_all=rbind(data.Down_Blue_cycle_all,data.Down_Blue_cycle)
  
  
  print(sub)

}
data.salience_cycle_all=data.salience_cycle_all[-1,]
data.nonSalience_cycle_all=data.nonSalience_cycle_all[-1,]
data.salience_RedTarget_cycle_all=data.salience_RedTarget_cycle_all[-1,]
data.salience_BlueTarget_cycle_all=data.salience_BlueTarget_cycle_all[-1,]
data.Nonsalience_RedTarget_cycle_all=data.Nonsalience_RedTarget_cycle_all[-1,]
data.Nonsalience_BlueTarget_cycle_all=data.Nonsalience_BlueTarget_cycle_all[-1,]

data.0_salience_cycle_all=data.0_salience_cycle_all[-1,]
data.0_nonSalience_cycle_all=data.0_nonSalience_cycle_all[-1,]
data.0_salience_RedTarget_cycle_all=data.0_salience_RedTarget_cycle_all[-1,]
data.0_salience_BlueTarget_cycle_all=data.0_salience_BlueTarget_cycle_all[-1,]
data.0_Nonsalience_BlueTarget_cycle_all=data.0_Nonsalience_BlueTarget_cycle_all[-1,]
data.0_Nonsalience_RedTarget_cycle_all=data.0_Nonsalience_RedTarget_cycle_all[-1,]


data.180_cycle_all=data.180_cycle_all[-1,]
data.0_cycle_all=data.0_cycle_all[-1,]
data.180_Red_cycle_all=data.180_Red_cycle_all[-1,]
data.180_Blue_cycle_all=data.180_Blue_cycle_all[-1,]
data.0_Red_cycle_all=data.0_Red_cycle_all[-1,]
data.0_Blue_cycle_all=data.0_Blue_cycle_all[-1,]

data.Up_cycle_all=data.Up_cycle_all[-1,]
data.Down_cycle_all=data.Down_cycle_all[-1,]
data.Up_Red_cycle_all=data.Up_Red_cycle_all[-1,]
data.Up_Blue_cycle_all=data.Up_Blue_cycle_all[-1,]
data.Down_Red_cycle_all=data.Down_Red_cycle_all[-1,]
data.Down_Blue_cycle_all=data.Down_Blue_cycle_all[-1,]

##Salience_180
p=act_cycle(data.salience_cycle_all,23.23)
p
ggsave(p, file=paste("gray_30_Salience_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_cycle_all,paste(pa,'/','gray_data.salience_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.nonSalience_cycle_all,24.82)
p
ggsave(p, file=paste("gray_30_nonSalience_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.nonSalience_cycle_all,paste(pa,'/','gray_data.nonSalience_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.salience_RedTarget_cycle_all,Salience_RedTarget_meanCycle)
p
ggsave(p, file=paste("gray_30_Salience_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_RedTarget_cycle_all,paste(pa,'/','gray_data.salience_RedTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.salience_BlueTarget_cycle_all,Salience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("gray_30_Salience_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.salience_BlueTarget_cycle_all,paste(pa,'/','gray_data.salience_BlueTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.Nonsalience_BlueTarget_cycle_all,NonSalience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("gray_30_NonSalience_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_BlueTarget_cycle_all,paste(pa,'/','gray_data.Nonsalience_BlueTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.Nonsalience_RedTarget_cycle_all,NonSalience_RedTarget_meanCycle)
p
ggsave(p, file=paste("gray_30_NonSalience_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Nonsalience_RedTarget_cycle_all,paste(pa,'/','gray_data.Nonsalience_RedTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##


##Salience_0
p=act_cycle(data.0_salience_cycle_all,21.05)
p
ggsave(p, file=paste("gray_30_Salience_0_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_salience_cycle_all,paste(pa,'/','gray_data.0_salience_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_nonSalience_cycle_all,20.80)
p
ggsave(p, file=paste("gray_30_nonSalience_0_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_nonSalience_cycle_all,paste(pa,'/','gray_data.0_nonSalience_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_salience_RedTarget_cycle_all,21.12)
p
ggsave(p, file=paste("gray_30_Salience_0_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_salience_RedTarget_cycle_all,paste(pa,'/','gray_data.0_salience_RedTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_salience_BlueTarget_cycle_all,20.98)
p
ggsave(p, file=paste("gray_30_Salience_0_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_salience_BlueTarget_cycle_all,paste(pa,'/','gray_data.0_salience_BlueTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_Nonsalience_BlueTarget_cycle_all,20.77)
p
ggsave(p, file=paste("gray_30_NonSalience_0_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_Nonsalience_BlueTarget_cycle_all,paste(pa,'/','gray_data.0_Nonsalience_BlueTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_Nonsalience_RedTarget_cycle_all,20.84)
p
ggsave(p, file=paste("gray_30_NonSalience_0_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_Nonsalience_RedTarget_cycle_all,paste(pa,'/','gray_data.Nonsalience_RedTarget_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##



##Cannon Angle
p=act_cycle(data.180_cycle_all,24.22)
p
ggsave(p, file=paste("gray_30_180_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.180_cycle_all,paste(pa,'/','gray_data.180_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_cycle_all,21.15)
p
ggsave(p, file=paste("gray_30_0_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_cycle_all,paste(pa,'/','gray_data.0_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.180_Red_cycle_all,24.10)
p
ggsave(p, file=paste("gray_30_180_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.180_Red_cycle_all,paste(pa,'/','gray_data.180_Red_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.180_Blue_cycle_all,24.34)
p
ggsave(p, file=paste("gray_30_180_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.180_Blue_cycle_all,paste(pa,'/','gray_data.180_Blue_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
##
p=act_cycle(data.0_Red_cycle_all,21.19)
p
ggsave(p, file=paste("gray_30_0_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_Red_cycle_all,paste(pa,'/','gray_data.0_Red_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.0_Blue_cycle_all,21.11)
p
ggsave(p, file=paste("gray_30_0_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.0_Blue_cycle_all,paste(pa,'/','gray_data.0_Blue_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##


##Orientation
p=act_cycle(data.Up_cycle_all,20.65)
p
ggsave(p, file=paste("gray_30_Up_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Up_cycle_all,paste(pa,'/','gray_data.Up_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle(data.Down_cycle_all,25.47)
p
ggsave(p, file=paste("gray_30_Down_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Down_cycle_all,paste(pa,'/','gray_data.Down_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle_1(data.Up_Red_cycle_all)
p
ggsave(p, file=paste("gray_30_Up_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Up_Red_cycle_all,paste(pa,'/','gray_data.Up_Red_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle_1(data.Up_Blue_cycle_all)
p
ggsave(p, file=paste("gray_30_Up_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Up_Blue_cycle_all,paste(pa,'/','gray_data.Up_Blue_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
##
p=act_cycle_1(data.Down_Red_cycle_all)
p
ggsave(p, file=paste("gray_30_Down_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Down_Red_cycle_all,paste(pa,'/','gray_data.Down_Red_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##
p=act_cycle_1(data.Down_Blue_cycle_all)
p
ggsave(p, file=paste("gray_30_Down_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
write.table(data.Down_Blue_cycle_all,paste(pa,'/','gray_data.Down_Blue_cycle_all','.csv',sep=''),sep=',',col.names=NA,qmethod="double")
##


