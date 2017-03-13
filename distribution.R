## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)


##act_eq of each unit through cycles
act_cycle=function(x,limit)
{ 
  x$LineGroup = paste(x$Unit)
  p = ggplot(x, aes(y=value, color=Unit, x=cycle))
  ##画上点和errorbar，设置点的大小，添加errorbar
  p = p + geom_point(size=4)
  ##画上线条
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  ##添加横纵坐标
  p = p + ylab("Act_eq")
  p = p + xlab("Time (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  #   ##设置线条对应的颜色属???
  #   p = p + scale_color_manual(values=c("#0000FF", "929591","#FF0000","#00FF00"))
  ##添加标题
  #p = p + labs(title = filename)
  ##采用白背景，黑线主题
  p = p + theme_classic()+theme(legend.position="none")
  # +theme(legend.position = c(.15,.26))+theme(legend.box ="vertical")+geom_vline(xintercept=meanRT,lty=2,col="black")
  
  p
  
}

 for (sub in c(1:30)){
  # sub=17
  ##Set directory 
  pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,'/',sep='')
  pa=paste(pa0,"test_results",sep='')
  data.raw<-read.table(paste(pa0,'CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL)
  
  data.raw$cycle=data.raw$cycle+1  
  a=c(data.raw$cycle==100)
  b=data.raw$trial[a]
  for(i in (1:length(b)))
  {  data.raw<-data.raw[(data.raw$trial!=b[i]),]}
 
   data.raw=subset(data.raw,cycle<=26)
    for (n in c(0:99))
        {names(data.raw)[names(data.raw)==paste("Hidden_act_eq_",n,sep='')] <- paste('U_',n,sep='')}

 
  
#   for (n in c(0:99)){
#     if (n <10) 
#       {names(data.raw)[names(data.raw)==paste("Hidden_act_eq_",n,sep='')] <- paste('U_','(',1,',',n+1,')',sep='')}
#     if (n>=10)
#       {a=as.numeric(strsplit(as.character(n), "")[[1]])
#        names(data.raw)[names(data.raw)==paste("U_",n,sep='')] <- paste('Hidden_act_eq_','(',a[1]+1,',',a[2]+1,')',sep='')}
#   
#   }
#   
  
  
  data.EFOR=subset(data.raw,E_Ori!="")
  data.EFOR=subset(data.EFOR,TargetColor=='')

  data.EFOR_cycle=ddply(data.EFOR,.(cycle),summarize,
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
  
#   myvars1=c('cycle','U_9','U_1','U_16')
#   data.EFOR_cycle=data.EFOR_cycle[myvars1]
    for (n in c(0:99)){
      if (n <10) 
        {names(data.EFOR_cycle)[names(data.EFOR_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
      if (n>=10)
        {a=as.numeric(strsplit(as.character(n), "")[[1]])
         names(data.EFOR_cycle)[names(data.EFOR_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
    
    }
    
  
  data.EFOR_cycle <- melt(data.EFOR_cycle, id=c("cycle"))
  colnames(data.EFOR_cycle)[2] <- "Unit"
   p=act_cycle(data.EFOR_cycle)
  p
  ggsave(p, file=paste("EFOR_Hidden_Act_eq.png",sep=''),width=15, height=4, dpi=150, bg="transparent",path=pa)
 
  
  data.BIFOR=subset(data.raw,B_Ori!="")
  data.BIFOR=subset(data.BIFOR,TargetColor=='Blue')
  data.BIFOR=subset(data.BIFOR,Ratio=="High")
  data.BIFOR_cycle=ddply(data.BIFOR,.(cycle),summarize,
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
    {names(data.BIFOR_cycle)[names(data.BIFOR_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.BIFOR_cycle)[names(data.BIFOR_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.BIFOR_cycle <- melt(data.BIFOR_cycle, id=c("cycle"))
  colnames(data.BIFOR_cycle)[2] <- "Unit"
  p=act_cycle(data.BIFOR_cycle)
  p
  ggsave(p, file=paste("BIFOR_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
 
  data.RIFOR=subset(data.raw,R_Ori!="")
  data.RIFOR=subset(data.RIFOR,TargetColor=='Red')
  data.RIFOR=subset(data.RIFOR,Ratio=="High")
  data.RIFOR_cycle=ddply(data.RIFOR,.(cycle),summarize,
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
    {names(data.RIFOR_cycle)[names(data.RIFOR_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.RIFOR_cycle)[names(data.RIFOR_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
   data.RIFOR_cycle <- melt(data.RIFOR_cycle, id=c("cycle"))
  colnames(data.RIFOR_cycle)[2] <- "Unit"
  p=act_cycle(data.RIFOR_cycle)
  p
  ggsave(p, file=paste("RIFOR_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  data.BIFOREFOR=subset(data.raw,B_Ori!="")
  data.BIFOREFOR=subset(data.BIFOREFOR,TargetColor=='Blue')
  data.BIFOREFOR=subset(data.BIFOREFOR,Ratio=="High")
  data.BIFOREFOR=subset(data.BIFOREFOR,E_Ori=="E_U")
  data.BIFOREFOR_cycle=ddply(data.BIFOREFOR,.(cycle),summarize,
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
    {names(data.BIFOREFOR_cycle)[names(data.BIFOREFOR_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.BIFOREFOR_cycle)[names(data.BIFOREFOR_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
   data.BIFOREFOR_cycle <- melt(data.BIFOREFOR_cycle, id=c("cycle"))
  colnames(data.BIFOREFOR_cycle)[2] <- "Unit"
  p=act_cycle(data.BIFOREFOR_cycle)
  p
  ggsave(p, file=paste("BIFOREFOR_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  data.RIFOREFOR=subset(data.raw,R_Ori!="")
  data.RIFOREFOR=subset(data.RIFOREFOR,TargetColor=='Red')
  data.RIFOREFOR=subset(data.RIFOREFOR,Ratio=="High")
  data.RIFOREFOR=subset(data.RIFOREFOR,E_Ori=="E_U")
  data.RIFOREFOR_cycle=ddply(data.RIFOREFOR,.(cycle),summarize,
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
    {names(data.RIFOREFOR_cycle)[names(data.RIFOREFOR_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.RIFOREFOR_cycle)[names(data.RIFOREFOR_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.RIFOREFOR_cycle <- melt(data.RIFOREFOR_cycle, id=c("cycle"))
  colnames(data.RIFOREFOR_cycle)[2] <- "Unit"
  p=act_cycle(data.RIFOREFOR_cycle)
  p
  ggsave(p, file=paste("RIFOREFOR_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  data.BIFORRIFOR=subset(data.raw,CannonAngle!="")
  data.BIFORRIFOR=subset(data.BIFORRIFOR,E_Ori=="")
  data.BIFORRIFOR_0=subset(data.BIFORRIFOR,CannonAngle==0)
  
  data.BIFORRIFOR_0_cycle=ddply(data.BIFORRIFOR_0,.(cycle),summarize,
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
    {names(data.BIFORRIFOR_0_cycle)[names(data.BIFORRIFOR_0_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.BIFORRIFOR_0_cycle)[names(data.BIFORRIFOR_0_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
  data.BIFORRIFOR_0_cycle <- melt(data.BIFORRIFOR_0_cycle, id=c("cycle"))
  colnames(data.BIFORRIFOR_0_cycle)[2] <- "Unit"
  p=act_cycle(data.BIFORRIFOR_0_cycle)
  p
  ggsave(p, file=paste("BIFOR_RIFOR_0_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  data.BIFORRIFOR_180=subset(data.BIFORRIFOR,CannonAngle==180)
  data.BIFORRIFOR_180_cycle=ddply(data.BIFORRIFOR_180,.(cycle),summarize,
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
    {names(data.BIFORRIFOR_180_cycle)[names(data.BIFORRIFOR_180_cycle)==paste("U_",n,sep='')] <- paste('(',1,',',n+1,')',sep='')}
    if (n>=10)
    {a=as.numeric(strsplit(as.character(n), "")[[1]])
    names(data.BIFORRIFOR_180_cycle)[names(data.BIFORRIFOR_180_cycle)==paste("U_",n,sep='')] <- paste('(',a[1]+1,',',a[2]+1,')',sep='')}
  }
   data.BIFORRIFOR_180_cycle <- melt(data.BIFORRIFOR_180_cycle, id=c("cycle"))
  colnames(data.BIFORRIFOR_180_cycle)[2] <- "Unit"
  p=act_cycle(data.BIFORRIFOR_180_cycle)
  p
  ggsave(p, file=paste("BIFOR_RIFOR_180_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  
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
  p=act_cycle(data.salience_RedTarget_cycle)
  p
  ggsave(p, file=paste("30_Salience_Red_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  
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
  p=act_cycle(data.salience_BlueTarget_cycle)
  p
  ggsave(p, file=paste("30_Salience_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  
  
  
  
  
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
  p=act_cycle(data.Nonsalience_BlueTarget_cycle)
  p
  ggsave(p, file=paste("30_NonSalience_Blue_Hidden_Act_eq.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  

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
  # data.Nonsalience_cycle=subset(data.Nonsalience_cycle,Unit=="U_9")
  p=act_cycle(data.Nonsalience_RedTarget_cycle)
  p
  ggsave(p, file=paste("30_NonSalience_Red_U_20.png",sep=''),width=20, height=4, dpi=150, bg="transparent",path=pa)
  
  
  
  
  
  
  
  
  
 }
  
  
  
   
 