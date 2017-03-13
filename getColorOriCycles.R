
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)


Cycles=c(100)
SSE=c(100)
CannonAngle=c(100)
TargetColor=c('Red')
Ratio=c('2:6')
Orientation=c('Left')
Subject=c(1)
out=c(100)
data.all=data.frame(Cycles,SSE,CannonAngle,TargetColor,Ratio,Orientation,Subject,out)
data.all[,]=NA

for (sub in c(1:30)){
  # sub=1
  ##Set directory 
  pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,sep='')
  data.raw<-read.table(paste(pa0,'TrialOutput.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
  ## 
  myvars1=c('minus_cycles','sse',
            'CannonAngle','TargetColor','Ratio','Orientation')
  myvars=c('Cycles','SSE',
           'CannonAngle','TargetColor','Ratio','Orientation')
  data.raw=data.raw[myvars1]
  names(data.raw)=myvars
  data.raw=subset(data.raw,CannonAngle>=0)
  data.raw=subset(data.raw,Ratio!="High")
  data.raw$Subject=sub
  a=c(data.raw$Cycles==100)
  data.raw$out=c(0)
  data.raw$out[a]=1
  data.all=rbind(data.all,data.raw)
}


data.raw=subset(data.all,CannonAngle>=0)
Sub=nrow(data.raw)

data.raw$Cycles1=data.raw$Cycles
data.raw$Cycles=data.raw$Cycles-10
data.correct=subset(data.raw,SSE!=1)
Delete.correct=(nrow(data.raw)-nrow(data.correct))/nrow(data.raw)
##delete the 200 cycles trials
data.out=subset(data.correct,out!=1)
data.out=data.correct
data.out$Sub=factor(data.out$Sub)
data.Subject=ddply(data.out,.(Sub),summarize,Cycles.SD=sd(Cycles),Cycle=mean(Cycles))
data.Subject=data.Subject[data.Subject$Cycles<(mean(data.Subject$Cycles)+3*sd(data.Subject$Cycles)),]
data.3SD=data.out[data.out$Cycles<(mean(data.out$Cycles)+3*sd(data.out$Cycles)),]
nSubject=nrow(data.Subject)

data.3SD$CannonAngle=factor(data.3SD$CannonAngle)


a=c(data.3SD$Orientation=="U"|data.3SD$Orientation=="UL"|data.3SD$Orientation=="UR")
data.3SD$Orientation[a]="Up"
a=c(data.3SD$Orientation=="D"|data.3SD$Orientation=="DL"|data.3SD$Orientation=="DR")
data.3SD$Orientation[a]="Down"
data.3SD=subset(data.3SD,Orientation!="L")
data.3SD=subset(data.3SD,Orientation!="R")
data.ColOri.Subject=ddply(data.3SD,.(Sub,TargetColor,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.ColOri=ddply(data.ColOri.Subject,.(TargetColor,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))


pa=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results')
write.csv(data.ColOri,paste(pa,'ColOri_UpDown Cycles.csv',sep='/'))
