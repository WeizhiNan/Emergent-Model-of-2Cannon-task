## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)

##act_w of each unit through cycles
act_cycle_1=function(x,titlename,meanCycle)
{ 
  x$LineGroup = paste(x$TargetCannonOrientation)
  p = ggplot(x, aes(y=value, color=TargetCannonOrientation, x=cycle))
  p = p + geom_point(size=4)
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  p = p + ylab("Act_w")
  p = p + xlab("Time (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  p = p + theme(axis.title = element_text( face="bold", size=22))
  p = p + theme(axis.text.x  = element_text(face="bold",size=16))
  p = p + theme(axis.text.y  = element_text(face="bold",size=16))
  p = p + scale_color_manual(values=c("blue","red"))
  # p = p + labs(title = titlename)
  
  p = p + theme_classic()+ scale_fill_discrete(labels=c("Nonsalient Cannon", "Salient Cannon"))
  p = p + theme(legend.position = c(.15,.90))+theme(legend.box ="vertical", legend.background = element_rect(fill="transparent"))
  p = p + geom_vline(xintercept=meanCycle,linetype = "longdash",size=1.5,col="Black")
  p = p + geom_vline(xintercept=10,linetype = "longdash",size=1.5,col="Green")
  p
  
}
##act_w of each unit through cycles
act_cycle_2=function(x,titlename,meanCycle)
{ 
  x$LineGroup = paste(x$Cannon)
  p = ggplot(x, aes(y=value, color=CannonSalience, x=cycle))
  p = p + geom_point(size=4)
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  p = p + ylab("Act_w")
  p = p + xlab("Time (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  p = p + scale_color_manual(values=c("blue","red"))
  # p = p + labs(title = titlename)
  p = p + theme(axis.title = element_text( face="bold", size=22))
  p = p + theme(axis.text.x  = element_text(face="bold",size=16))
  p = p + theme(axis.text.y  = element_text(face="bold",size=16))
  p = p + theme_classic()+ scale_fill_discrete(labels=c("Nonsalient Cannon", "Salient Cannon"))
  p = p + theme(legend.position = c(.10,.90))+theme(legend.box ="vertical", legend.background = element_rect(fill="transparent"))
  p = p + geom_vline(xintercept=meanCycle,linetype = "longdash",size=1.5,col="Black")
  p = p + geom_vline(xintercept=10,linetype = "longdash",size=1.5,col="Green")
  p
  
}

Salience_RedTarget_meanCycle=23.27
Salience_BlueTarget_meanCycle=23.20
NonSalience_RedTarget_meanCycle=24.47
NonSalience_BlueTarget_meanCycle=25.16
Salience=mean(Salience_RedTarget_meanCycle,Salience_BlueTarget_meanCycle)
nonSalience=mean(NonSalience_RedTarget_meanCycle,NonSalience_BlueTarget_meanCycle)

BlueDown=25.57
BlueUp=20.64
RedDown=25.40
RedUp=20.66
Up=mean(BlueUp,RedUp)
Down=mean(BlueDown,RedDown)

##Set directory 
pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sep='')
pa=paste(pa0,'test_results',sep='/')
pa_cycle10_0.0=paste(pa,'cycle10_subset_0.0',sep='/')
pa_cycle10_0.20=paste(pa,'cycle10_subset_0.20',sep='/')
pa_export=paste(pa,'standard_plot',sep='/')


############ Act_w("End of Cue",0.0) in Orientation Effect
Cycle10_RedUp_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_RedUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,cycle<=26)
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,variable!="Ta")
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,variable!="EFOR")
a=c(Cycle10_RedUp_cycle$variable=="RC")
Cycle10_RedUp_cycle$TargetCannonOrientation[a]="Up"
Cycle10_RedUp_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_BlueUp_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_BlueUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,cycle<=26)
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,variable!="Ta")
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,variable!="EFOR")
a=c(Cycle10_BlueUp_cycle$variable=="BC")
Cycle10_BlueUp_cycle$TargetCannonOrientation[a]="Up"
Cycle10_BlueUp_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_RedDown_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_RedDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,cycle<=26)
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,variable!="Ta")
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,variable!="EFOR")
a=c(Cycle10_RedDown_cycle$variable=="BC")
Cycle10_RedDown_cycle$TargetCannonOrientation[a]="Up"
Cycle10_RedDown_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_BlueDown_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_BlueDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,cycle<=26)
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,variable!="Ta")
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,variable!="EFOR")
a=c(Cycle10_BlueDown_cycle$variable=="RC")
Cycle10_BlueDown_cycle$TargetCannonOrientation[a]="Up"
Cycle10_BlueDown_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_Up_cycle=rbind(Cycle10_RedUp_cycle,Cycle10_BlueUp_cycle)
Cycle10_Up_cycle_1=ddply(Cycle10_Up_cycle,.(TargetCannonOrientation,cycle),summarize,value=mean(value))
titlename="Target Cannon Point Up"
p=act_cycle_1(Cycle10_Up_cycle_1,titlename,Up)
p1=p
ggsave(p, file=paste("Cycle10_Up_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

Cycle10_Down_cycle=rbind(Cycle10_RedDown_cycle,Cycle10_BlueDown_cycle)
Cycle10_Down_cycle_1=ddply(Cycle10_Down_cycle,.(TargetCannonOrientation,cycle),summarize,value=mean(value))
titlename="Target Cannon Point Down"
p=act_cycle_1(Cycle10_Down_cycle_1,titlename,Down)
p2=p
ggsave(p, file=paste("Cycle10_Down_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

require(gridExtra)
p=grid.arrange(p1, p2, ncol=2)
ggsave(p, file=paste("Cycle10_Orientation Effect_0.0_1.png",sep=''),width=16, height=4, dpi=150, bg="transparent",path=pa_export)



############ Act_w("End of Cue",0.20) in Orientation Effect
Cycle10_RedUp_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_RedUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,cycle<=26)
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,variable!="Ta")
Cycle10_RedUp_cycle=subset(Cycle10_RedUp_cycle,variable!="EFOR")
a=c(Cycle10_RedUp_cycle$variable=="RC")
Cycle10_RedUp_cycle$TargetCannonOrientation[a]="Up"
Cycle10_RedUp_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_BlueUp_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_BlueUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,cycle<=26)
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,variable!="Ta")
Cycle10_BlueUp_cycle=subset(Cycle10_BlueUp_cycle,variable!="EFOR")
a=c(Cycle10_BlueUp_cycle$variable=="BC")
Cycle10_BlueUp_cycle$TargetCannonOrientation[a]="Up"
Cycle10_BlueUp_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_RedDown_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_RedDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,cycle<=26)
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,variable!="Ta")
Cycle10_RedDown_cycle=subset(Cycle10_RedDown_cycle,variable!="EFOR")
a=c(Cycle10_RedDown_cycle$variable=="BC")
Cycle10_RedDown_cycle$TargetCannonOrientation[a]="Up"
Cycle10_RedDown_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_BlueDown_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_BlueDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,cycle<=26)
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,variable!="Ta")
Cycle10_BlueDown_cycle=subset(Cycle10_BlueDown_cycle,variable!="EFOR")
a=c(Cycle10_BlueDown_cycle$variable=="RC")
Cycle10_BlueDown_cycle$TargetCannonOrientation[a]="Up"
Cycle10_BlueDown_cycle$TargetCannonOrientation[!a]="Down"

Cycle10_Up_cycle=rbind(Cycle10_RedUp_cycle,Cycle10_BlueUp_cycle)
Cycle10_Up_cycle_1=ddply(Cycle10_Up_cycle,.(TargetCannonOrientation,cycle),summarize,value=mean(value))
titlename="Target Cannon Point Up"
p=act_cycle_1(Cycle10_Up_cycle_1,titlename,Up)
p1=p
p11=p
ggsave(p, file=paste("Cycle10_Up_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

Cycle10_Down_cycle=rbind(Cycle10_RedDown_cycle,Cycle10_BlueDown_cycle)
Cycle10_Down_cycle_1=ddply(Cycle10_Down_cycle,.(TargetCannonOrientation,cycle),summarize,value=mean(value))
titlename="Target Cannon Point Down"
p=act_cycle_1(Cycle10_Down_cycle_1,titlename,Down)
p2=p
p22=p
ggsave(p, file=paste("Cycle10_Down_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

require(gridExtra)
p=grid.arrange(p1, p2, ncol=2)
ggsave(p, file=paste("Cycle10_Orientation Effect_0.20_1.png",sep=''),width=16, height=4, dpi=150, bg="transparent",path=pa_export)


############ Act_w("End of Cue",0.0) in Salience Effect
Cycle10_data.Nonsalience_BlueTarget_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,cycle<=26)
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,variable!="Ta")
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.Nonsalience_BlueTarget_cycle$variable=="RC")
Cycle10_data.Nonsalience_BlueTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.Nonsalience_BlueTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_data.Nonsalience_RedTarget_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,cycle<=26)
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,variable!="Ta")
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.Nonsalience_RedTarget_cycle$variable=="BC")
Cycle10_data.Nonsalience_RedTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.Nonsalience_RedTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_data.salience_BlueTarget_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_data.salience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,cycle<=26)
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,variable!="Ta")
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.salience_BlueTarget_cycle$variable=="BC")
Cycle10_data.salience_BlueTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.salience_BlueTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_data.salience_RedTarget_cycle<-read.table(paste(pa_cycle10_0.0,'/','Cycle10_data.salience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,cycle<=26)
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,variable!="Ta")
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.salience_RedTarget_cycle$variable=="RC")
Cycle10_data.salience_RedTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.salience_RedTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_Salience_cycle=rbind(Cycle10_data.salience_RedTarget_cycle,Cycle10_data.salience_BlueTarget_cycle)
Cycle10_Salience_cycle=ddply(Cycle10_Salience_cycle,.(CannonSalience,cycle),summarize,value=mean(value))
titlename="Salient Cannon As the Target Cannon"
p=act_cycle_2(Cycle10_Salience_cycle,titlename,Salience)
p1=p
ggsave(p, file=paste("Cycle10_Salience_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

Cycle10_NonSalience_cycle=rbind(Cycle10_data.Nonsalience_RedTarget_cycle,Cycle10_data.Nonsalience_BlueTarget_cycle)
Cycle10_NonSalience_cycle=ddply(Cycle10_NonSalience_cycle,.(CannonSalience,cycle),summarize,value=mean(value))
titlename="Non-Salient Cannon As the Target Cannon"
p=act_cycle_2(Cycle10_NonSalience_cycle,titlename,Salience)
p2=p
ggsave(p, file=paste("Cycle10_NonSalience_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

require(gridExtra)
p=grid.arrange(p1, p2, ncol=2)
ggsave(p, file=paste("Cycle10_Salience Effect_0.0_1.png",sep=''),width=16, height=4, dpi=150, bg="transparent",path=pa_export)




############ Act_w("End of Cue",0.20) in Salience Effect
Cycle10_data.Nonsalience_BlueTarget_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,cycle<=26)
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,variable!="Ta")
Cycle10_data.Nonsalience_BlueTarget_cycle=subset(Cycle10_data.Nonsalience_BlueTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.Nonsalience_BlueTarget_cycle$variable=="RC")
Cycle10_data.Nonsalience_BlueTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.Nonsalience_BlueTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_data.Nonsalience_RedTarget_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,cycle<=26)
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,variable!="Ta")
Cycle10_data.Nonsalience_RedTarget_cycle=subset(Cycle10_data.Nonsalience_RedTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.Nonsalience_RedTarget_cycle$variable=="BC")
Cycle10_data.Nonsalience_RedTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.Nonsalience_RedTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_data.salience_BlueTarget_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_data.salience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,cycle<=26)
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,variable!="Ta")
Cycle10_data.salience_BlueTarget_cycle=subset(Cycle10_data.salience_BlueTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.salience_BlueTarget_cycle$variable=="BC")
Cycle10_data.salience_BlueTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.salience_BlueTarget_cycle$CannonSalience[!a]="NonSalient"

substrate 
Cycle10_data.salience_RedTarget_cycle<-read.table(paste(pa_cycle10_0.20,'/','Cycle10_data.salience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,cycle<=26)
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,variable!="Ta")
Cycle10_data.salience_RedTarget_cycle=subset(Cycle10_data.salience_RedTarget_cycle,variable!="EFOR")
a=c(Cycle10_data.salience_RedTarget_cycle$variable=="RC")
Cycle10_data.salience_RedTarget_cycle$CannonSalience[a]="Salient"
Cycle10_data.salience_RedTarget_cycle$CannonSalience[!a]="NonSalient"


Cycle10_Salience_cycle=rbind(Cycle10_data.salience_RedTarget_cycle,Cycle10_data.salience_BlueTarget_cycle)
Cycle10_Salience_cycle=ddply(Cycle10_Salience_cycle,.(CannonSalience,cycle),summarize,value=mean(value))
titlename="Salient Cannon As the Target Cannon"
p=act_cycle_2(Cycle10_Salience_cycle,titlename,Salience)
p1=p
p33=p
ggsave(p, file=paste("Cycle10_Salience_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

Cycle10_NonSalience_cycle=rbind(Cycle10_data.Nonsalience_RedTarget_cycle,Cycle10_data.Nonsalience_BlueTarget_cycle)
Cycle10_NonSalience_cycle=ddply(Cycle10_NonSalience_cycle,.(CannonSalience,cycle),summarize,value=mean(value))
titlename="Non-Salient Cannon As the Target Cannon"
p=act_cycle_2(Cycle10_NonSalience_cycle,titlename,Salience)
p2=p
p44=p
ggsave(p, file=paste("Cycle10_NonSalience_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)

require(gridExtra)
p=grid.arrange(p1, p2, ncol=2)
ggsave(p, file=paste("Cycle10_Salience Effect_0.20_1.png",sep=''),width=16, height=4, dpi=150, bg="transparent",path=pa_export)








require(gridExtra)
p=grid.arrange(p33, p44,p11,p22, ncol=2)
ggsave(p, file=paste("Cycle10_SE_TCOE_0.20_1.png",sep=''),width=16, height=8, dpi=150, bg="transparent",path=pa_export)






