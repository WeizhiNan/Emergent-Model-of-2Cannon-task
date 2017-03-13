###################################################
##?????????????????????????????????
###################################################
rm(list=ls())
##??????????????????
library(ggplot2)
library(plyr)


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

pa=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results',sep='')
write.table(data.all,paste(pa,'All_subjects.csv',sep='/'),sep=',',col.names=NA,qmethod="double")



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


data.AngColRat.Subject=ddply(data.3SD,.(Sub,CannonAngle,TargetColor,Ratio),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngColRat=ddply(data.AngColRat.Subject,.(CannonAngle,TargetColor,Ratio),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngColRat$Cycles.SE=data.AngColRat$Cycles.SD/sqrt(nSubject)


limits <- aes(ymax = Cycles + Cycles.SE/2, ymin=Cycles - Cycles.SE/2)
data.AngColRat$LineGroup = paste(as.character(data.AngColRat$CannonAngle),
                                 as.character(data.AngColRat$TargetColor), sep="-")
p = ggplot(data.AngColRat, aes(color=TargetColor, shape = CannonAngle, y=Cycles, x=Ratio))
p = p + geom_point(size=4) + geom_errorbar(limits, width=0.1, size=0.8)
p = p + geom_line(aes(group=LineGroup), size=1.5)+ylim(5,25)
p = p + ylab("Cycles")
p = p + xlab("Pellet Color Ratio (Blue : Red)")
p = p + scale_color_manual(values=c("#0000FF", "#FF0000"))
#p = p + labs(title = "TargetColor CannonAngle Ratio")
p =  p + theme_classic()
p=p+theme(legend.position = c(.35, .9))+theme(legend.box ="horizontal")
ggsave(p, file=paste("CannonAngle_TargetColor_Ratio Cycles.eps"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file=paste("CannonAngle_TargetColor_Ratio Cycles.png"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)

data.3SD1=data.3SD

a=c(data.3SD$Orientation=="U"|data.3SD$Orientation=="UL"|data.3SD$Orientation=="UR")
data.3SD$Orientation[a]="Up"
a=c(data.3SD$Orientation=="D"|data.3SD$Orientation=="DL"|data.3SD$Orientation=="DR")
data.3SD$Orientation[a]="Down"
data.3SD=subset(data.3SD,Orientation!="L")
data.3SD=subset(data.3SD,Orientation!="R")
data.AngOri.Subject=ddply(data.3SD,.(Sub,CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri=ddply(data.AngOri.Subject,.(CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri$Cycles.SE=data.AngOri$Cycles.SD/sqrt(nSubject)
names(data.AngOri)[2]=c("TargetCannonOrientation")
limits <- aes(ymax = Cycles + Cycles.SE/2, ymin=Cycles - Cycles.SE/2)
data.AngOri$LineGroup = paste(as.character(data.AngOri$TargetCannonOrientation))
p = ggplot(data.AngOri, aes(color=TargetCannonOrientation,  y=Cycles, x=CannonAngle))#+ylim(700,1300)
p = p + geom_point(size=4) + geom_errorbar(limits, width=0.1, size=0.8)
p = p + geom_line(aes(group=LineGroup), size=1.5)+ylim(5,25)
p = p + ylab("Cycles")
p = p + xlab("Cannon Angle")
p = p + scale_color_manual(values=c("#0000FF", "#FF0000"))
# p = p + scale_color_manual(values=c("black", "gray"))
#p = p + labs(title = "CannonAngle Orientation")
p = p + theme_classic()
p=p+theme(legend.position = c(.3, .9))

ggsave(p, file=paste("CannonAngle_OrientationUD_Cycles_1.eps"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file=paste("CannonAngle_OrientationUD_Cycles_1.png"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)


a=c(data.3SD$Orientation=="U"|data.3SD$Orientation=="UL"|data.3SD$Orientation=="UR")
data.3SD$Orientation[a]="Up"
a=c(data.3SD$Orientation=="D"|data.3SD$Orientation=="DL"|data.3SD$Orientation=="DR")
data.3SD$Orientation[a]="Down"
data.3SD=subset(data.3SD,Orientation!="L")
data.3SD=subset(data.3SD,Orientation!="R")
data.AngOri.Subject=ddply(data.3SD,.(Sub,CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri=ddply(data.AngOri.Subject,.(CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri$Cycles.SE=data.AngOri$Cycles.SD/sqrt(nSubject)
names(data.AngOri)[2]=c("TargetCannonOrientation")
limits <- aes(ymax = Cycles + Cycles.SE/2, ymin=Cycles - Cycles.SE/2)
data.AngOri$LineGroup = paste(as.character(data.AngOri$TargetCannonOrientation))
p = ggplot(data.AngOri, aes(color=TargetCannonOrientation,  y=Cycles, x=CannonAngle))#+ylim(700,1300)
p = p + geom_point(size=4) + geom_errorbar(limits, width=0.1, size=0.8)
p = p + geom_line(aes(group=LineGroup), size=1.5)+ylim(5,25)
p = p + ylab("Cycles")
p = p + xlab("Cannon Angle")
# p = p + scale_color_manual(values=c("#0000FF", "#FF0000"))
p = p + scale_color_manual(values=c("black", "gray"))
#p = p + labs(title = "CannonAngle Orientation")
p = p + theme_classic()
p=p+theme(legend.position = c(.3, .9))

ggsave(p, file=paste("CannonAngle_OrientationUD_Cycles.eps"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file=paste("CannonAngle_OrientationUD_Cycles.png"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)


data.AngOri.Subject1=ddply(data.3SD1,.(Sub,CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri1=ddply(data.AngOri.Subject1,.(CannonAngle,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
data.AngOri1$Cycles.SE=data.AngOri1$Cycles.SD/sqrt(nSubject)


data.AngOri1$CannonAngle=factor(data.AngOri1$CannonAngle)

limits <- aes(ymax = Cycles + Cycles.SE/2, ymin=Cycles - Cycles.SE/2)
data.AngOri1$LineGroup = paste(as.character(as.character(data.AngOri1$CannonAngle)))
p = ggplot(data.AngOri1, aes(shape=CannonAngle,  y=Cycles, x=Orientation))#+ylim(700,1300)
p = p + geom_point(size=4) + geom_errorbar(limits, width=0.1, size=0.8)+scale_x_discrete(limits=c("U","UR","R","DR","D","DL","L","UL"))
p = p + geom_line(aes(group=LineGroup), size=1.5)+ylim(5,25)
p = p + ylab("Cycles")
p = p + xlab("Target Cannon Orientation")
p = p + scale_color_manual(values=c("black", "gray"))
#p = p + labs(title = "CannonAngle Orientation")
p = p + theme_classic()
p=p+theme(legend.position = c(.2, .9))
ggsave(p, file=paste("CannonAngle_Orientation_Cycles.eps"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file=paste("CannonAngle_Orientation_Cycles.png"),
       width=4, height=4, dpi=150, bg="transparent",path=pa)




##??????repeated-measure ANOVA???eta-squared??????
es=function(x,n=NULL) {
  if(is.null(n)) n=length(names(x))-2
  k=NULL
  p=summary(x)
  for(i in (1:n)+1) {
    l=unlist(p[i][[1]])
    k=c(k,l[3]/sum(l[3:4]))
  }
  names(k)=names(p)[(1:n)+1]
  k
}

sink(paste(pa,'bartlett AngColRat Cycles.txt',sep='/'))
##??????????????????
bartlett.test(Cycles~interaction(CannonAngle,Ratio,TargetColor),data=data.AngColRat.Subject)
sink()

##????????????,????????????????????????????????????????????????
AngColRat=aov(Cycles ~ CannonAngle*TargetColor*Ratio + Error(Sub/(CannonAngle*TargetColor*Ratio)), data=data.AngColRat.Subject)
sink(paste(pa,'aov TargetColor_Ratio_CannonAngle Cycles.txt',sep='/'))
summary(AngColRat)
cat('\n\nEta-Squared\n')
es(AngColRat)
sink()

##??????????????????
sink(paste(pa,'bartlett AngOri.txt',sep='/'))
bartlett.test(Cycles~interaction(CannonAngle,Orientation),data=data.AngOri.Subject)
sink()
##?????????????????????????????????
Result=(aov(Cycles ~ CannonAngle*Orientation+ Error(Sub/(CannonAngle*Orientation)),data=data.AngOri.Subject))
sink(paste(pa,'aov Orientation_CannonAngle Cycles.txt',sep='/'))
summary(Result)
cat('\n\nEta-Squared\n')
es(Result)
sink()

##????????????????????????
write.csv(data.AngOri,paste(pa,'AngOri_UpDown Cycles.csv',sep='/'))
write.csv(data.AngOri1,paste(pa,'AngOri8 Cycles.csv',sep='/'))
write.csv(data.AngColRat,paste(pa,'AngColRat Cycles.csv',sep='/'))



###SPSS
library(reshape)
data.SPSS.AngColRat=cast(data.AngColRat.Subject,Sub~CannonAngle~TargetColor~Ratio,value="Cycles")
write.table(data.SPSS.AngColRat,paste(pa,'data_SPSS_AngColRat.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
data.SPSS.AngOri=cast(data.AngOri.Subject,Sub~CannonAngle~Orientation,value="Cycles")
write.table(data.SPSS.AngOri,paste(pa,'data_SPSS_AngOri.csv',sep='/'),sep=',',col.names=NA,qmethod="double")

data.AngColRat.Subject.180=subset(data.AngColRat.Subject,CannonAngle==180)
data.SPSS.AngColRat.180=cast(data.AngColRat.Subject.180,Sub~TargetColor~Ratio,value="Cycles")
write.table(data.SPSS.AngColRat.180,paste(pa,'data_SPSS_AngColRat.180.csv',sep='/'),sep=',',col.names=NA,qmethod="double")

data.AngColRat.Subject.0=subset(data.AngColRat.Subject,CannonAngle==0)
data.SPSS.AngColRat.0=cast(data.AngColRat.Subject.0,Sub~TargetColor~Ratio,value="Cycles")
write.table(data.SPSS.AngColRat.0,paste(pa,'data_SPSS_AngColRat.0.csv',sep='/'),sep=',',col.names=NA,qmethod="double")



# CA.subjuect=ddply(data.3SD,.(Sub,CannonAngle),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
# CA.180.subject=subset(CA.subjuect,CannonAngle==180)
# CA.0.subject=subset(CA.subjuect,CannonAngle==0)
# CAE.Cycles=CA.180.subject$Cycles-CA.0.subject$Cycles
# 
# 
# TCOE.subject=ddply(data.3SD,.(Sub,Orientation),summarize,Cycles.SD=sd(Cycles),Cycles=mean(Cycles))
# TCOE.Up.subject=subset(TCOE.subject,Orientation=='Up')
# TCOE.Down.subject=subset(TCOE.subject,Orientation=='Down')
# TCOE.Cycles=TCOE.Down.subject$Cycles-TCOE.Up.subject$Cycles
# 
# 
# SE.180.2.subject=subset(data.AngColRat.Subject.180,Ratio=='2:6')
# SE.180.2.Red.subject=subset(SE.180.2.subject,TargetColor=='Red')
# SE.180.2.Blue.subject=subset(SE.180.2.subject,TargetColor=='Blue')
# SE.2.subject.Cycles=SE.180.2.Blue.subject$Cycles-SE.180.2.Red.subject$Cycles
# 
# SE.180.4.subject=subset(data.AngColRat.Subject.180,Ratio=='4:4')
# SE.180.4.Red.subject=subset(SE.180.4.subject,TargetColor=='Red')
# SE.180.4.Blue.subject=subset(SE.180.4.subject,TargetColor=='Blue')
# SE.4.subject.Cycles=SE.180.4.Blue.subject$Cycles-SE.180.4.Red.subject$Cycles
# 
# SE.180.6.subject=subset(data.AngColRat.Subject.180,Ratio=='6:2')
# SE.180.6.Red.subject=subset(SE.180.6.subject,TargetColor=='Red')
# SE.180.6.Blue.subject=subset(SE.180.6.subject,TargetColor=='Blue')
# SE.6.subject.Cycles=SE.180.6.Blue.subject$Cycles-SE.180.6.Red.subject$Cycles

# ##get RT effects
# ##TCOE
# TCOE.RT<-read.table(paste(pa,'TCOE_RT.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# TCOE.RT$RT=sort(TCOE.RT$x)
# TCOE.RT$Cycles=sort(TCOE.Cycles)
# write.table(TCOE.RT,paste(pa,'TCOE_RT_Cycles.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
# fire.reg <- lm(TCOE.RT$RT ~ TCOE.RT$Cycles, data = TCOE.RT) #????????????
# sink(paste(pa,'Regressor in TCOE.txt',sep='/'))
# summary(fire.reg) #???????????????
# anova(fire.reg) #???????????????
# sink()
# p=ggplot(data=TCOE.RT,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of TCOE (ms)")
# p = p + xlab("Cycles of TCOE")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="Cor_RTCycles_TCOE.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="Cor_RTCycles_TCOE.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# 
# 
# ##CAE
# CAE.RT<-read.table(paste(pa,'CAE_RT.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# CAE.RT$RT=sort(CAE.RT$x)
# CAE.RT$Cycles=sort(CAE.Cycles)
# write.table(CAE.RT,paste(pa,'CAE_RT_Cycles.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
# cor.test(CAE.RT$RT,CAE.RT$Cycles)
# plot(CAE.RT$RT ~ CAE.RT$Cycles)
# fire.reg <- lm(CAE.RT$RT ~ CAE.RT$Cycles, data =CAE.RT) #????????????
# sink(paste(pa,'Regressor in CAE.txt',sep='/'))
# summary(fire.reg) #???????????????
# anova(fire.reg) #???????????????
# sink()
# p=ggplot(data=CAE.RT,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of CAE (ms)")
# p = p + xlab("Cycles of CAE")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="Cor_RTCycles_CAE.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="Cor_RTCycles_CAE.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# 
# 
# ##SE
# #2:6
# SE.2.RT<-read.table(paste(pa,'SE_2_RT.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# SE.2.RT$RT=sort(SE.2.RT$x)
# SE.2.RT$Cycles=sort(SE.2.subject.Cycles)
# write.table(SE.2.RT,paste(pa,'SE_2_RT_Cycles.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
# cor.test(SE.2.RT$RT,SE.2.RT$Cycles)
# plot(SE.2.RT$RT ~ SE.2.RT$Cycles)
# fire.reg <- lm(SE.2.RT$RT ~ SE.2.RT$Cycles, data =SE.2.RT) #????????????
# sink(paste(pa,'Regressor in SE_2.txt',sep='/'))
# summary(fire.reg) #???????????????
# sink()
# p=ggplot(data=SE.2.RT,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of SE_2 (ms)")
# p = p + xlab("Cycles of SE_2")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="Cor_RTCycles_SE_2.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="Cor_RTCycles_SE_2CAE.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# 
# 
# 
# #4:4
# SE.4.RT<-read.table(paste(pa,'SE_4_RT.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# SE.4.RT$RT=sort(SE.4.RT$x)
# SE.4.RT$Cycles=sort(SE.4.subject.Cycles)
# write.table(SE.4.RT,paste(pa,'SE_4_RT_Cycles.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
# cor.test(SE.4.RT$RT,SE.4.RT$Cycles)
# plot(SE.4.RT$RT ~ SE.4.RT$Cycles)
# fire.reg <- lm(SE.4.RT$RT ~ SE.4.RT$Cycles, data =SE.4.RT) #????????????
# sink(paste(pa,'Regressor in SE_4.txt',sep='/'))
# summary(fire.reg) #???????????????
# sink()
# p=ggplot(data=SE.4.RT,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of SE_4 (ms)")
# p = p + xlab("Cycles of SE_4")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="Cor_RTCycles_SE_4.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="Cor_RTCycles_SE_4CAE.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# 
# #6:2
# SE.6.RT<-read.table(paste(pa,'SE_6_RT.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# SE.6.RT$RT=sort(SE.6.RT$x)
# SE.6.RT$Cycles=sort(SE.6.subject.Cycles)
# write.table(SE.6.RT,paste(pa,'SE_6_RT_Cycles.csv',sep='/'),sep=',',col.names=NA,qmethod="double")
# cor.test(SE.6.RT$RT,SE.6.RT$Cycles)
# plot(SE.6.RT$RT ~ SE.6.RT$Cycles)
# fire.reg <- lm(SE.6.RT$RT ~ SE.6.RT$Cycles, data =SE.6.RT) #????????????
# sink(paste(pa,'Regressor in SE_6.txt',sep='/'))
# summary(fire.reg) #???????????????
# sink()
# p=ggplot(data=SE.6.RT,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of SE_6 (ms)")
# p = p + xlab("Cycles of SE_6")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="Cor_RTCycles_SE_6.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="Cor_RTCycles_SE_6CAE.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)



# 
# ##Cannon 180, Ratio,TargetColor
# RatCol.180<-read.table(paste(pa,'RT_AngColTar_180.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# RatCol.180=ddply(RatCol.180,.(Angle,ColorRatio,TargetColor,Subject),summarize,RT=mean(RT))
# data.AngColRat.Subject.180=ddply(data.AngColRat.Subject.180,.(CannonAngle,Ratio,TargetColor,Sub),summarize,Cycles=mean(Cycles))
# data.AngColRat.Subject.180$RT=RatCol.180$RT
# cor.test(data.AngColRat.Subject.180$RT,data.AngColRat.Subject.180$Cycles)
# plot(data.AngColRat.Subject.180$RT ~ data.AngColRat.Subject.180$Cycles)
# fire.reg <- lm(data.AngColRat.Subject.180$RT ~ data.AngColRat.Subject.180$Cycles, data =data.AngColRat.Subject.180) #????????????
# sink(paste(pa,'Regressor in AngColRat_180.txt',sep='/'))
# summary(fire.reg) #???????????????
# sink()
# p=ggplot(data=data.AngColRat.Subject.180,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of ColRat180 (ms)")
# p = p + xlab("Cycles of ColRat180")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="ColRat180.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="ColRat180.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# 
# 
# 
# 
# ##Cannon 0, Ratio,TargetColor
# RatCol.0<-read.table(paste(pa,'RT_AngColRat_0.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
# RatCol.0=ddply(RatCol.0,.(Angle,ColorRatio,TargetColor,Subject),summarize,RT=mean(RT))
# data.AngColRat.Subject.0=ddply(data.AngColRat.Subject.0,.(CannonAngle,Ratio,TargetColor,Sub),summarize,Cycles=mean(Cycles))
# data.AngColRat.Subject.0$RT=RatCol.0$RT
# cor.test(data.AngColRat.Subject.0$RT,data.AngColRat.Subject.0$Cycles)
# plot(data.AngColRat.Subject.0$RT ~ data.AngColRat.Subject.0$Cycles)
# fire.reg <- lm(data.AngColRat.Subject.0$RT ~ data.AngColRat.Subject.0$Cycles, data =data.AngColRat.Subject.0) #????????????
# sink(paste(pa,'Regressor in AngColRat_0.txt',sep='/'))
# summary(fire.reg) #???????????????
# sink()
# p=ggplot(data=data.AngColRat.Subject.0,aes(y=RT, x=Cycles))
# p=p+stat_smooth(method=lm,size=2,color="black",span=1) + geom_point(size=6)+theme_classic()
# ##??????????????????
# p = p + ylab("RT of ColRat0 (ms)")
# p = p + xlab("Cycles of ColRat0")
# ##?????????legend???eps???????????????,???????????????
# ggsave(p, file="ColRat0.eps",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)
# ggsave(p, file="ColRat0.png",
#        width=8, height=4, dpi=150, bg="transparent",path=pa)


##Ratio,TargetColor
RatCol<-read.table(paste(pa,'RT_AngColRat.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
RatCol=ddply(RatCol,.(Angle,ColorRatio,TargetColor,Subject),summarize,RT=mean(RT))
data.AngColRat.Subject=ddply(data.AngColRat.Subject,.(CannonAngle,Ratio,TargetColor,Sub),summarize,Cycles=mean(Cycles))
data.AngColRat.Subject$RT=RatCol$RT

data.AngColRat.Subject$Group=paste(data.AngColRat.Subject$Ratio,
                                   data.AngColRat.Subject$TargetColor,
                                   data.AngColRat.Subject$CannonAngle,sep = "_")


##
plot(data.AngColRat.Subject$RT ~ data.AngColRat.Subject$Cycles)
fire.reg <- lm(data.AngColRat.Subject$RT ~ data.AngColRat.Subject$Cycles, data =data.AngColRat.Subject) #????????????
sink(paste(pa,'Regressor in AngColRat.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat.Subject$RT,data.AngColRat.Subject$Cycles)
sink()
p=ggplot(data=data.AngColRat.Subject,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Cannon Angle
             and Pellet Color Ratio (ms)")
p = p + xlab("Cycles of Cannon Angle and Pellet Color Ratio")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)

##0
data.AngColRat.Subject0=subset(data.AngColRat.Subject,CannonAngle==0)
plot(data.AngColRat.Subject0$RT ~ data.AngColRat.Subject0$Cycles)
fire.reg <- lm(data.AngColRat.Subject0$RT ~ data.AngColRat.Subject0$Cycles, data =data.AngColRat.Subject0) #????????????
sink(paste(pa,'Regressor in AngColRat_0.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat.Subject0$RT,data.AngColRat.Subject0$Cycles)
sink()
p=ggplot(data=data.AngColRat.Subject0,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Pellet Color Ratio in CannonAngle 0 (ms)")
p = p + xlab("Cycles of Pellet Color Ratioin CannonAngle 0")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat0.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat0.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)

##180
data.AngColRat.Subject180=subset(data.AngColRat.Subject,CannonAngle==180)
plot(data.AngColRat.Subject180$RT ~ data.AngColRat.Subject180$Cycles)
fire.reg <- lm(data.AngColRat.Subject180$RT ~ data.AngColRat.Subject180$Cycles, data =data.AngColRat.Subject180) #????????????
sink(paste(pa,'Regressor in AngColRat_180.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat.Subject180$RT,data.AngColRat.Subject180$Cycles)
sink()
p=ggplot(data=data.AngColRat.Subject180,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Pellet Color Ratio in CannonAngle 180 (ms)")
p = p + xlab("Cycles of Pellet Color Ratioin CannonAngle 180")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat180.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat180.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)



##AngOri
AngOri<-read.table(paste(pa,'RT_AngSim.csv',sep='/'),header=TRUE,sep=",",na.string=NULL)
AngOri=ddply(AngOri,.(Angle,Simon,Subject),summarize,RT=mean(RT))
# data.AngOri.Subject=subset(data.AngOri.Subject,Orientation!='Left')
# data.AngOri.Subject=subset(data.AngOri.Subject,Orientation!='Right')
data.AngOri.Subject=ddply(data.AngOri.Subject,.(CannonAngle,Orientation,Sub),summarize,Cycles=mean(Cycles))
data.AngOri.Subject$RT=AngOri$RT

data.AngOri.Subject$Group=paste(data.AngOri.Subject$CannonAngle,data.AngOri.Subject$Orientation,sep = "_")
##
cor.test(data.AngOri.Subject$RT,data.AngOri.Subject$Cycles)
plot(data.AngOri.Subject$RT ~ data.AngOri.Subject$Cycles)
fire.reg <- lm(data.AngOri.Subject$RT ~ data.AngOri.Subject$Cycles, data =data.AngOri.Subject) #????????????
sink(paste(pa,'Regressor in AngORi.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngOri.Subject$RT,data.AngOri.Subject$Cycles)
sink()
p=ggplot(data=data.AngOri.Subject,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Cannon Angle and 
             Target Cannon Orientation (ms)")
p = p + xlab("Cycles of cCannon Angle and Target Cannon Orientation ")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="AngOri.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="AngOri.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)



##0
data.AngOri.Subject0=subset(data.AngOri.Subject,CannonAngle==0)
cor.test(data.AngOri.Subject0$RT,data.AngOri.Subject0$Cycles)
plot(data.AngOri.Subject0$RT ~ data.AngOri.Subject0$Cycles)
fire.reg <- lm(data.AngOri.Subject0$RT ~ data.AngOri.Subject0$Cycles, data =data.AngOri.Subject0) #????????????
sink(paste(pa,'Regressor in AngORi0.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngOri.Subject0$RT,data.AngOri.Subject0$Cycles)
sink()
p=ggplot(data=data.AngOri.Subject0,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Target Cannon Orientation in CannonAngle 0 (ms)")
p = p + xlab("Cycles of Target Cannon Orientation in CannonAngle 0")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="AngOri0.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="AngOri0.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
##180
data.AngOri.Subject180=subset(data.AngOri.Subject,CannonAngle==180)
cor.test(data.AngOri.Subject180$RT,data.AngOri.Subject180$Cycles)
plot(data.AngOri.Subject180$RT ~ data.AngOri.Subject180$Cycles)
fire.reg <- lm(data.AngOri.Subject180$RT ~ data.AngOri.Subject180$Cycles, data =data.AngOri.Subject180) #????????????
sink(paste(pa,'Regressor in AngORi180.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngOri.Subject180$RT,data.AngOri.Subject180$Cycles)
sink()
p=ggplot(data=data.AngOri.Subject180,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Target Cannon Orientation in CannonAngle 180 (ms)")
p = p + xlab("Cycles of Target Cannon Orientation in CannonAngle 180")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="AngOri180.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="AngOri180.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)









data.AngColRat=ddply(data.AngColRat.Subject,.(CannonAngle,Ratio,TargetColor),summarize,Cycles=mean(Cycles),RT=mean(RT))
data.AngColRat$Group=paste(data.AngColRat$CannonAngle,data.AngColRat$Ratio,data.AngColRat$TargetColor,sep="_")
##
plot(data.AngColRat$RT ~ data.AngColRat$Cycles)
fire.reg <- lm(data.AngColRat$RT ~ data.AngColRat$Cycles, data =data.AngColRat) #????????????
sink(paste(pa,'Regressor in AngColRat_average.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat$RT,data.AngColRat$Cycles)
sink()
p=ggplot(data=data.AngColRat,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("   RT of Cannon Angle
             and Pellet Color Ratio (ms)")
p = p + xlab("Cycles of Cannon Angle and Pellet Color Ratio")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat_average.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat_average.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)

##0
data.AngColRat.0=subset(data.AngColRat,CannonAngle==0)
plot(data.AngColRat.0$RT ~ data.AngColRat.0$Cycles)
fire.reg <- lm(data.AngColRat.0$RT ~ data.AngColRat.0$Cycles, data =data.AngColRat.0) #????????????
sink(paste(pa,'Regressor in AngColRat_0_average.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat.0$RT,data.AngColRat.0$Cycles)
sink()
p=ggplot(data=data.AngColRat.0,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Pellet Color Ratio in CannonAngle 0 (ms)")
p = p + xlab("Cycles of Pellet Color Ratioin CannonAngle 0")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat0_average.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat0_average.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)

##180
data.AngColRat.180=subset(data.AngColRat,CannonAngle==180)
plot(data.AngColRat.180$RT ~ data.AngColRat.180$Cycles)
fire.reg <- lm(data.AngColRat.180$RT ~ data.AngColRat.180$Cycles, data =data.AngColRat.180) #????????????
sink(paste(pa,'Regressor in AngColRat_180_average.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngColRat.180$RT,data.AngColRat.180$Cycles)
sink()
p=ggplot(data=data.AngColRat.180,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("RT of Pellet Color Ratio in CannonAngle 180 (ms)")
p = p + xlab("Cycles of Pellet Color Ratioin CannonAngle 180")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="ColRat180_average.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="ColRat180_average.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)



data.AngOri=ddply(data.AngOri.Subject,.(CannonAngle,Orientation),summarize,Cycles=mean(Cycles),RT=mean(RT))
data.AngOri$Group=paste(data.AngOri$CannonAngle,data.AngOri$Orientation,sep="-")
##
cor.test(data.AngOri$RT,data.AngOri$Cycles)
plot(data.AngOri$RT ~ data.AngOri$Cycles)
fire.reg <- lm(data.AngOri$RT ~ data.AngOri$Cycles, data =data.AngOri) #????????????
sink(paste(pa,'Regressor in AngORi_average.txt',sep='/'))
summary(fire.reg) #???????????????
cor.test(data.AngOri$RT,data.AngOri$Cycles)
sink()
p=ggplot(data=data.AngOri,aes(y=RT, x=Cycles,color=Group))
p=p+stat_smooth(method=lm,size=2,color="black",span=1) +ylim(400,1650)+ geom_point(size=4)+theme_classic()
##??????????????????
p = p + ylab("    RT of Cannon Angle and 
             Target Cannon Orientation (ms)")
p = p + xlab("Cycles of cCannon Angle and Target Cannon Orientation ")
##?????????legend???eps???????????????,???????????????
ggsave(p, file="AngOri_average.eps",
       width=8, height=4, dpi=150, bg="transparent",path=pa)
ggsave(p, file="AngOri_average.png",
       width=8, height=4, dpi=150, bg="transparent",path=pa)

