## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)

##act_w of each unit through cycles
act_cycle_1=function(x,titlename,meanCycle)
{ 
  x$LineGroup = paste(x$Representation)
  p = ggplot(x, aes(y=value, color=Representation, x=cycle))
  p = p + geom_point(size=4)
  p = p + geom_line(aes(group=LineGroup), size=1.5)
  p = p + ylab("Act_w")
  p = p + xlab("Time Distribution (cycle)")+scale_x_continuous(breaks = c(0,5,10,15,20,25))
  p = p + scale_color_manual(values=c("blue","red"))
  p = p + labs(title = titlename)
  p = p + theme_classic()
  p=p +theme(legend.position = c(.1,.85))+theme(legend.box ="vertical")
  p=p + geom_vline(xintercept=meanCycle,linetype = "longdash",size=1.5,col="Black")
  p=p + geom_vline(xintercept=10,linetype = "longdash",size=1.5,col="Green")
  p
  
}

##act_w of each unit through cycles
temp_process=function(x,titlename,meanCycle)
{ 
  x=subset(x,cycle<=26)
  names(x)[names(x)=="variable"] <- "Rep"
  x=subset(x,Rep!="Ta")
  x=subset(x,Rep!="EFOR")
  a=c(x$Rep=="BC")
  x$Representation[a]="Blue Cannon"
  x$Representation[!a]="Red Cannon"
  p=act_cycle_1(x,titlename,meanCycle)
  p 
  
}

Salience_RedTarget_meanCycle=23.27
Salience_BlueTarget_meanCycle=23.20
NonSalience_RedTarget_meanCycle=24.47
NonSalience_BlueTarget_meanCycle=25.16

BlueDown=25.57
BlueUp=20.64
RedDown=25.40
RedUp=20.66





##Set directory 
pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/17',sep='')
pa=paste(pa0,'test_results',sep='/')
pa_EoT_0.0=paste(pa,'subset_0.0',sep='/')
pa_EoT_0.20=paste(pa,'subset_0.20',sep='/')
pa_export=paste(pa,'standard_plot',sep='/')



############ Act_w("End of Cue",0.0) in Orientation Effect
RedUp_cycle<-read.table(paste(pa_EoT_0.0,'/','RedUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=RedUp_cycle
titlename="Red Target Cannon Point Up"
p=temp_process(temp,titlename,RedUp)
p
ggsave(p, file=paste("RedUp_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p1=p

BlueUp_cycle<-read.table(paste(pa_EoT_0.0,'/','BlueUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=BlueUp_cycle
titlename="Blue Target Cannon Point Up"
p=temp_process(temp,titlename,BlueUp)
p
ggsave(p, file=paste("BlueUp_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p2=p

RedDown_cycle<-read.table(paste(pa_EoT_0.0,'/','RedDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=RedDown_cycle
titlename="Red Target Cannon Point Down"
p=temp_process(temp,titlename,RedDown)
p
ggsave(p, file=paste("RedDown_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p3=p

BlueDown_cycle<-read.table(paste(pa_EoT_0.0,'/','BlueDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=BlueDown_cycle
titlename="Blue Target Cannon Point Down"
p=temp_process(temp,titlename,BlueDown)
p
ggsave(p, file=paste("BlueDown_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p4=p

require(gridExtra)
p=grid.arrange(p1, p2,p3,p4, ncol=2)
ggsave(p, file=paste("Orientation Effect_0.0.png",sep=''),width=16, height=8, dpi=150, bg="transparent",path=pa_export)
###################
###################

############ Act_w("End of Cue",0.0) in Salience Effect 
data.Nonsalience_BlueTarget_cycle<-read.table(paste(pa_EoT_0.0,'/','data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.Nonsalience_BlueTarget_cycle
titlename="Red Salient Cannon and Blue Target"
p=temp_process(temp,titlename,NonSalience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceBlueTarget_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p1=p

data.Nonsalience_RedTarget_cycle<-read.table(paste(pa_EoT_0.0,'/','data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.Nonsalience_RedTarget_cycle
titlename="Blue Salient Cannon and Red Target"
p=temp_process(temp,titlename,NonSalience_RedTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceRedTarget_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p2=p


data.salience_BlueTarget_cycle<-read.table(paste(pa_EoT_0.0,'/','data.salience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.salience_BlueTarget_cycle
titlename="Blue Salient Cannon and Blue Target"
p=temp_process(temp,titlename,Salience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("BlueSalienceBlueTarget_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p3=p

data.salience_RedTarget_cycle<-read.table(paste(pa_EoT_0.0,'/','data.salience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.salience_RedTarget_cycle
titlename="Red Salient Cannon and Red Target"
p=temp_process(temp,titlename,Salience_RedTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceRedTarget_0.0.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p4=p

require(gridExtra)
p=grid.arrange(p1, p2,p3,p4, ncol=2)
ggsave(p, file=paste("Salience Effect_0.0.png",sep=''),width=16, height=8, dpi=150, bg="transparent",path=pa_export)
###################
###################



############ Act_w("End of Cue",0.20) in Orientation Effect
RedUp_cycle<-read.table(paste(pa_EoT_0.20,'/','RedUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=RedUp_cycle
titlename="Red Target Cannon Point Up"
p=temp_process(temp,titlename,RedUp)
p
ggsave(p, file=paste("RedUp_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p1=p

BlueUp_cycle<-read.table(paste(pa_EoT_0.20,'/','BlueUp_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=BlueUp_cycle
titlename="Blue Target Cannon Point Up"
p=temp_process(temp,titlename,BlueUp)
p
ggsave(p, file=paste("BlueUp_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p2=p

RedDown_cycle<-read.table(paste(pa_EoT_0.20,'/','RedDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=RedDown_cycle
titlename="Red Target Cannon Point Down"
p=temp_process(temp,titlename,RedDown)
p
ggsave(p, file=paste("RedDown_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p3=p

BlueDown_cycle<-read.table(paste(pa_EoT_0.20,'/','BlueDown_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=BlueDown_cycle
titlename="Blue Target Cannon Point Down"
p=temp_process(temp,titlename,BlueDown)
p
ggsave(p, file=paste("BlueDown_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p4=p

require(gridExtra)
p=grid.arrange(p1, p2,p3,p4, ncol=2)
ggsave(p, file=paste("Orientation Effect_0.20.png",sep=''),width=16, height=8, dpi=150, bg="transparent",path=pa_export)
###################
###################

############ Act_w("End of Cue",0.20) in Salience Effect 
data.Nonsalience_BlueTarget_cycle<-read.table(paste(pa_EoT_0.20,'/','data.Nonsalience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.Nonsalience_BlueTarget_cycle
titlename="Red Salient Cannon and Blue Target"
p=temp_process(temp,titlename,NonSalience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceBlueTarget_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p1=p

data.Nonsalience_RedTarget_cycle<-read.table(paste(pa_EoT_0.20,'/','data.Nonsalience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.Nonsalience_RedTarget_cycle
titlename="Blue Salient Cannon and Red Target"
p=temp_process(temp,titlename,NonSalience_RedTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceRedTarget_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p2=p


data.salience_BlueTarget_cycle<-read.table(paste(pa_EoT_0.20,'/','data.salience_BlueTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.salience_BlueTarget_cycle
titlename="Blue Salient Cannon and Blue Target"
p=temp_process(temp,titlename,Salience_BlueTarget_meanCycle)
p
ggsave(p, file=paste("BlueSalienceBlueTarget_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p3=p

data.salience_RedTarget_cycle<-read.table(paste(pa_EoT_0.20,'/','data.salience_RedTarget_cycle_average','.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
temp=data.salience_RedTarget_cycle
titlename="Red Salient Cannon and Red Target"
p=temp_process(temp,titlename,Salience_RedTarget_meanCycle)
p
ggsave(p, file=paste("RedSalienceRedTarget_0.20.png",sep=''),width=8, height=4, dpi=150, bg="transparent",path=pa_export)
p4=p

require(gridExtra)
p=grid.arrange(p1, p2,p3,p4, ncol=2)
ggsave(p, file=paste("Salience Effect_0.20.png",sep=''),width=16, height=8, dpi=150, bg="transparent",path=pa_export)
###################
###################




