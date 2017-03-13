## cluster analysis for the hidden layer 
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape)

##rsquare functon
rsquare=function(input_patternn,data.hidden.act) {
  r=matrix(c(0),nrow=10,ncol=10) 
  R=r
  r.square=r
  R.square=r
  for (i in c(1:10)){
    for (j in c(1:10)){   
      if(sum(data.hidden.act[,10*(i-1)+j])==0){r[i,j]=0;r.square[i,j]=0}
      if(sum(data.hidden.act[,10*(i-1)+j])!=0)
      {r[i,j]=cor(input_pattern,data.hidden.act[,10*(i-1)+j],method=("spearman"));
      r.square[i,j]=summary(lm(input_pattern~data.hidden.act[,10*(i-1)+j]))$r.squared}
      
      if (r[i,j]>thr)(R[i,j]=1)
      if (r.square[i,j]>thr)(R.square[i,j]=1)
    }
  }
  r.square
}
##Rsquare function
Rsquare=function(input_patternn,data.hidden.act) {
  r=matrix(c(0),nrow=10,ncol=10) 
  R=r
  r.square=r
  R.square=r
  for (i in c(1:10)){
    for (j in c(1:10)){   
      if(sum(data.hidden.act[,10*(i-1)+j])==0){r[i,j]=0;
      r.square[i,j]=0}
      if(sum(data.hidden.act[,10*(i-1)+j])!=0)
      {r[i,j]=cor(input_pattern,data.hidden.act[,10*(i-1)+j],method=("spearman"));
      r.square[i,j]=summary(lm(input_pattern~data.hidden.act[,10*(i-1)+j]))$r.squared}
      
      if (r[i,j]>thr)(R[i,j]=1)
      if (r.square[i,j]>thr)(R.square[i,j]=1)
    }
  }
  R.square
}
##heatmappolt
heatmapplot=function(x,limit)
{ 
  
  colnames(x) <- 1:10
  rownames(x) <- 1:10
  data.x <- melt(x)
  names(data.x)[names(data.x)=="X1"] <- "Row"
  names(data.x)[names(data.x)=="X2"] <- "Col"
  data.x$Row=factor(data.x$Row)
  data.x$Col=factor(data.x$Col)
  data.x <- ddply(data.x, .(Row), transform,rescale = scale(value))
  
  p =ggplot(data.x, aes(Col,Row)) + geom_tile(aes(fill = value),colour = "white") 
  #  p=p+ scale_fill_gradient2(low="white",mid="#FFC773",high="red",midpoint = median(data.x$value)/2, space = "Lab",
  #                            na.value = "grey50", guide , "colourbar")
  p=p+ scale_fill_gradient(low="white",high="red",limits=limit)
  base_size <- 9
  p=p + theme_gray(base_size = base_size)+scale_x_discrete(expand = c(0, 0)) +scale_y_discrete(expand = c(0, 0)) 
  p=p+labs(x = "",y = "") 
  p=p+theme(axis.ticks=element_blank())
  p=p+ theme(axis.text=element_text(size = base_size *2))
  # p=p+theme(legend.text	=element_text(size = base_size*1.5),legend.title=element_text(size = base_size*1.5))
  p
  # data.x B<- ddply(data.x, .(Row), transform,rescale = scale(value))
  # last_plot() %+% data.x
}

# 
for (threshold in c('0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50',
                    '0.55','0.60','0.65','0.70','0.75','0.80','0.85','0.90','0.95')){
  
  # threshold=c('0.10')
  ##inite tt, the data frame combining all conditions
  ##Different FOR Representations
  EOri_Num=c(100)
  BOri_Num_1=c(100)
  ROri_Num_1=c(100)
  share_BlueRed_Cannon_num_1=c(100)
  share_BlueEFOR_num=c(100)
  share_RedEFOR_num=c(100)
  share_BlueRedEFOR_num=c(100)  
  #Target Location Representations
  TLoc_Num_1=c(100)
  Blue_TLoc_Num_1=c(100)
  Red_TLoc_Num_1=c(100)
  share_BlueRed_target_num_1=c(100)
  share_Blue_target_num_1=c(100)
  share_Red_target_num_1=c(100)
  share_BlueRed_target_num_3_1=c(100)
  #Target Cannon and non-target Cannon Angle Representations  
  TargetCannon_0_Num_1=c(100)
  TargetCannon_180_Num_1=c(100)
  share_targetCannon_0180_num_1=c(100)
  #Target Cannon Orientation Representations  
  TargetCannon_Up_Num_1=c(100)
  TargetCannon_Down_Num_1=c(100)
  share_targetCannon_UpDown_num_1=c(100)
  #TargetCannon salience representations  
  TargetCannon_salience_Low_Num_1=c(100)
  TargetCannon_salience_Medium_Num_1=c(100)
  TargetCannon_salience_High_Num_1=c(100)
  share_salience_num_1=c(100)
  tt=data.frame(
    EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
    TLoc_Num_1,Blue_TLoc_Num_1,Red_TLoc_Num_1,share_BlueRed_target_num_1,share_Blue_target_num_1,share_Red_target_num_1,share_BlueRed_target_num_3_1,
    TargetCannon_0_Num_1,TargetCannon_180_Num_1,share_targetCannon_0180_num_1,
    TargetCannon_Up_Num_1,TargetCannon_Down_Num_1,share_targetCannon_UpDown_num_1,
    TargetCannon_salience_Low_Num_1,TargetCannon_salience_Medium_Num_1,TargetCannon_salience_High_Num_1,share_salience_num_1
  )
  
  thr=as.numeric(threshold)
  
  for( sub in c(1:30)){  
    # sub=1
    ##Set directory 
    pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,sep='')
    pa=paste(pa0,'test_results',sep='/')
    data.raw<-read.table(paste(pa0,'/','CycleOutput.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    
    data.raw$cycle=data.raw$cycle+1  
    a=c(data.raw$cycle==100)
    b=data.raw$trial[a]
    for(i in (1:length(b)))
    {  data.raw<-data.raw[(data.raw$trial!=b[i]),]}
    
    data.raw=subset(data.raw, cycle==10)
    
    
    data.EFOR=subset(data.raw,E_Ori!="")
    data.EFOR=subset(data.EFOR,TargetColor=='')
    a=c(data.EFOR$E_Ori=='E_UR')
    data.EFOR$EFOR[a]=1
    a=c(data.EFOR$E_Ori=='E_R')
    data.EFOR$EFOR[a]=2
    a=c(data.EFOR$E_Ori=='E_DR')
    data.EFOR$EFOR[a]=3
    a=c(data.EFOR$E_Ori=='E_D')
    data.EFOR$EFOR[a]=4
    a=c(data.EFOR$E_Ori=='E_DL')
    data.EFOR$EFOR[a]=5
    a=c(data.EFOR$E_Ori=='E_L')
    data.EFOR$EFOR[a]=6
    a=c(data.EFOR$E_Ori=='E_UL')
    data.EFOR$EFOR[a]=7
    a=c(data.EFOR$E_Ori=='E_U')
    data.EFOR$EFOR[a]=8
    ##EFOR_Orientation
    ##ordre data according to variable E_Ori
    input_pattern=data.EFOR$EFOR
    data.hidden.act=data.EFOR[,18:117]#10*10
    E_Orientation_rsquare=rsquare(input_pattern,data.hidden.act)
    E_Orientation_rsquare_01=Rsquare(input_pattern,data.hidden.act)
    EOri_Num=sum(E_Orientation_rsquare_01)
    # limit=c(0,1)
    # p=heatmapplot(E_Orientation_rsquare,limit)
    # ggsave(p, file=paste("E_Orientation_Rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(E_Orientation_rsquare_01,paste(pa,'/','E_Orientation_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(E_Orientation_rsquare,paste(pa,'/','E_Orientation_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    data.BlueCannon_1=subset(data.raw,B_Ori!='')
    data.BlueCannon_1=subset(data.BlueCannon_1,R_Ori=='')
    data.BlueCannon_1=subset(data.BlueCannon_1,E_Ori=='')
    data.BlueCannon_1=subset(data.BlueCannon_1,Ratio=='High')
    a=c(data.BlueCannon_1$B_Ori=='B_UR')
    data.BlueCannon_1$BlueCannon[a]=1
    a=c(data.BlueCannon_1$B_Ori=='B_R')
    data.BlueCannon_1$BlueCannon[a]=2
    a=c(data.BlueCannon_1$B_Ori=='B_DR')
    data.BlueCannon_1$BlueCannon[a]=3
    a=c(data.BlueCannon_1$B_Ori=='B_D')
    data.BlueCannon_1$BlueCannon[a]=4
    a=c(data.BlueCannon_1$B_Ori=='B_DL')
    data.BlueCannon_1$BlueCannon[a]=5
    a=c(data.BlueCannon_1$B_Ori=='B_L')
    data.BlueCannon_1$BlueCannon[a]=6
    a=c(data.BlueCannon_1$B_Ori=='B_UL')
    data.BlueCannon_1$BlueCannon[a]=7
    a=c(data.BlueCannon_1$B_Ori=='B_U')
    data.BlueCannon_1$BlueCannon[a]=8
    ##BlueCannon_Orientation
    input_pattern=data.BlueCannon_1$BlueCannon
    data.hidden.act=data.BlueCannon_1[,18:117]#10*10
    B_Orientation_rsquare_1=rsquare(input_pattern,data.hidden.act)
    B_Orientation_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    BOri_Num_1=sum(B_Orientation_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(B_Orientation_rsquare,limit)
    # ggsave(p, file=paste("B_Orientation_Rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(B_Orientation_rsquare_01,paste(pa,'/','B_Orientation_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(B_Orientation_rsquare,paste(pa,'/','B_Orientation_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    data.RedCannon_1=subset(data.raw,R_Ori!='')
    data.RedCannon_1=subset(data.RedCannon_1,B_Ori=='')
    data.RedCannon_1=subset(data.RedCannon_1,E_Ori=='')
    data.RedCannon_1=subset(data.RedCannon_1,Ratio=='High')
    a=c(data.RedCannon_1$R_Ori=='R_UR')
    data.RedCannon_1$RedCannon[a]=1
    a=c(data.RedCannon_1$R_Ori=='R_R')
    data.RedCannon_1$RedCannon[a]=2
    a=c(data.RedCannon_1$R_Ori=='R_DR')
    data.RedCannon_1$RedCannon[a]=3
    a=c(data.RedCannon_1$R_Ori=='R_D')
    data.RedCannon_1$RedCannon[a]=4
    a=c(data.RedCannon_1$R_Ori=='R_DL')
    data.RedCannon_1$RedCannon[a]=5
    a=c(data.RedCannon_1$R_Ori=='R_L')
    data.RedCannon_1$RedCannon[a]=6
    a=c(data.RedCannon_1$R_Ori=='R_UL')
    data.RedCannon_1$RedCannon[a]=7
    a=c(data.RedCannon_1$R_Ori=='R_U')
    data.RedCannon_1$RedCannon[a]=8
    ##RedCannon_Orientation
    ##ordre data according to variable E_Ori
    input_pattern=data.RedCannon_1$RedCannon
    data.hidden.act=data.RedCannon_1[,18:117]#10*10
    R_Orientation_rsquare_1=rsquare(input_pattern,data.hidden.act)
    R_Orientation_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    ROri_Num_1=sum(R_Orientation_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(R_Orientation_rsquare,limit)
    # ggsave(p, file=paste("R_Orientation_Rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(R_Orientation_rsquare_01,paste(pa,'/','R_Orientation_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(R_Orientation_rsquare,paste(pa,'/','R_Orientation_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    
    ##compute share neuron number of two color Cannons
    share_Cannon_1=B_Orientation_rsquare_01_1+R_Orientation_rsquare_01_1
    a=c(share_Cannon_1==2)
    share_BlueRed_Cannon_num_1=sum(a)
    
    ##compute share neuron number of blue Cannon and EFOR
    share_blueEFOR=B_Orientation_rsquare_01_1+E_Orientation_rsquare_01
    a=c(share_blueEFOR==2)
    share_BlueEFOR_num=sum(a)
    
    ##compute share neuron number of red Cannon and EFOR
    share_redEFOR=R_Orientation_rsquare_01_1+E_Orientation_rsquare_01
    a=c(share_redEFOR==2)
    share_RedEFOR_num=sum(a)
    
    ##compute share neuron number of three FORs
    share_threeFORs=B_Orientation_rsquare_01_1+R_Orientation_rsquare_01_1+E_Orientation_rsquare_01
    a=c(share_threeFORs==3)
    share_BlueRedEFOR_num=sum(a)
    
    ##Blue_Target_Location
    data.BlueTarget_1=subset(data.raw,B_Ori!='')
    data.BlueTarget_1=subset(data.BlueCannon_1,R_Ori=='')
    data.BlueTarget_1=subset(data.BlueCannon_1,E_Ori=='')
    data.BlueTarget_1=subset(data.BlueCannon_1,Ratio=='High')
    a=c(data.BlueTarget_1$T_Loc=='T_UR')
    data.BlueTarget_1$BlueTarget[a]= 1
    a=c(data.BlueTarget_1$T_Loc=='T_R')
    data.BlueTarget_1$BlueTarget[a]= 2
    a=c(data.BlueTarget_1$T_Loc=='T_DR')
    data.BlueTarget_1$BlueTarget[a]= 3
    a=c(data.BlueTarget_1$T_Loc=='T_D')
    data.BlueTarget_1$BlueTarget[a]= 4
    a=c(data.BlueTarget_1$T_Loc=='T_DL')
    data.BlueTarget_1$BlueTarget[a]= 5
    a=c(data.BlueTarget_1$T_Loc=='T_L')
    data.BlueTarget_1$BlueTarget[a]= 6
    a=c(data.BlueTarget_1$T_Loc=='T_UL')
    data.BlueTarget_1$BlueTarget[a]= 7
    a=c(data.BlueTarget_1$T_Loc=='T_U')
    data.BlueTarget_1$BlueTarget[a]= 8
    input_pattern=data.BlueTarget_1$BlueTarget
    data.hidden.act=data.BlueTarget_1[,18:117]#10*10
    Blue_Target_Location_rsquare_1=rsquare(input_pattern,data.hidden.act)
    Blue_Target_Location_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    Blue_TLoc_Num_1=sum(Blue_Target_Location_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(Blue_Target_Location_rsquare,limit)
    # ggsave(p, file=paste("Blue_Target_Location_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(Blue_Target_Location_rsquare_01,paste(pa,'/','Blue_Target_Location_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(Blue_Target_Location_rsquare,paste(pa,'/','Blue_Target_Location_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    
    ##Red_Target_Location
    data.RedTarget_1=subset(data.raw,R_Ori!='')
    data.RedTarget_1=subset(data.RedTarget_1,B_Ori=='')
    data.RedTarget_1=subset(data.RedTarget_1,E_Ori=='')
    data.RedTarget_1=subset(data.RedTarget_1,Ratio=='High')
    a=c(data.RedTarget_1$T_Loc=='T_UR')
    data.RedTarget_1$RedTarget[a]= 1
    a=c(data.RedTarget_1$T_Loc=='T_R')
    data.RedTarget_1$RedTarget[a]= 2
    a=c(data.RedTarget_1$T_Loc=='T_DR')
    data.RedTarget_1$RedTarget[a]= 3
    a=c(data.RedTarget_1$T_Loc=='T_D')
    data.RedTarget_1$RedTarget[a]= 4
    a=c(data.RedTarget_1$T_Loc=='T_DL')
    data.RedTarget_1$RedTarget[a]= 5
    a=c(data.RedTarget_1$T_Loc=='T_L')
    data.RedTarget_1$RedTarget[a]= 6
    a=c(data.RedTarget_1$T_Loc=='T_UL')
    data.RedTarget_1$RedTarget[a]= 7
    a=c(data.RedTarget_1$T_Loc=='T_U')
    data.RedTarget_1$RedTarget[a]= 8
    input_pattern=data.RedTarget_1$RedTarget
    data.hidden.act=data.RedTarget_1[,18:117]#10*10
    Red_Target_Location_rsquare_1=rsquare(input_pattern,data.hidden.act)
    Red_Target_Location_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    Red_TLoc_Num_1=sum(Red_Target_Location_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(Red_Target_Location_rsquare,limit)
    # ggsave(p, file=paste("Red_Target_Location_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(Red_Target_Location_rsquare_01,paste(pa,'/','Red_Target_Location_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(Red_Target_Location_rsquare,paste(pa,'/','Red_Target_Location_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##Target_Location
    data.Target_1=subset(data.raw,E_Ori!="")
    data.Target_1=subset(data.Target_1,TargetColor=='')
    data.Target_1$Target=0
    a=c(data.Target_1$T_Loc=='T_UR')
    data.Target_1$Target[a]= 1
    a=c(data.Target_1$T_Loc=='T_R')
    data.Target_1$Target[a]= 2
    a=c(data.Target_1$T_Loc=='T_DR')
    data.Target_1$Target[a]= 3
    a=c(data.Target_1$T_Loc=='T_D')
    data.Target_1$Target[a]= 4
    a=c(data.Target_1$T_Loc=='T_DL')
    data.Target_1$Target[a]= 5
    a=c(data.Target_1$T_Loc=='T_L')
    data.Target_1$Target[a]= 6
    a=c(data.Target_1$T_Loc=='T_UL')
    data.Target_1$Target[a]= 7
    a=c(data.Target_1$T_Loc=='T_U')
    data.Target_1$Target[a]= 8
    input_pattern=data.Target_1$Target
    data.hidden.act=data.Target_1[,18:117]#10*10
    Target_Location_rsquare_1=rsquare(input_pattern,data.hidden.act)
    Target_Location_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TLoc_Num_1=sum(Target_Location_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(Target_Location_rsquare,limit)
    # ggsave(p, file=paste("Target_Location_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(Target_Location_rsquare_01,paste(pa,'/','Target_Location_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(Target_Location_rsquare,paste(pa,'/','Target_Location_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##compute share neuron number of two color targets
    share_target_1=Blue_Target_Location_rsquare_01_1+Red_Target_Location_rsquare_01_1
    a=c(share_target_1==2)
    share_BlueRed_target_num_1=sum(a)
    
    ##compute share neuron number of blue targets with target 
    share_Blue_target_1=Blue_Target_Location_rsquare_01_1+Target_Location_rsquare_01_1
    a=c(share_Blue_target_1==2)
    share_Blue_target_num_1=sum(a)
    
    ##compute share neuron number of red targets with target 
    share_Red_target_1=Red_Target_Location_rsquare_01_1+Target_Location_rsquare_01_1
    a=c(share_Red_target_1==2)
    share_Red_target_num_1=sum(a)
    
    ##compute share neuron number of two color targets
    share_target3_1=Blue_Target_Location_rsquare_01_1+Red_Target_Location_rsquare_01_1+Target_Location_rsquare_01_1
    a=c(share_target3_1==3)
    share_BlueRed_target_num_3_1=sum(a)
    
    ## Cannon 0
    data.CannonAngle0_1=subset(data.raw,CannonAngle!='')
    data.CannonAngle0_1=subset(data.CannonAngle0_1,E_Ori=='')
    data.CannonAngle0_1=subset(data.CannonAngle0_1,CannonAngle=="0")
    a=c(data.CannonAngle0_1$TargetColor=="Blue")
    data.CannonAngle0_1$TargetColor1[a]=1
    data.CannonAngle0_1$TargetColor1[!a]=2
    input_pattern=data.CannonAngle0_1$TargetColor1
    data.hidden.act=data.CannonAngle0_1[,18:117]#10*10
    TargetCannon_0_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_0_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_0_Num_1=sum(TargetCannon_0_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(TargetCannon_0_rsquare,limit)
    # ggsave(p, file=paste("TargetCannon_0_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_0_rsquare_01,paste(pa,'/','TargetCannon_0_rsquare_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_0_rsquare,paste(pa,'/','TargetCannon_0_rsquare_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ## Cannon 180
    data.CannonAngle180_1=subset(data.raw,CannonAngle!='')
    data.CannonAngle180_1=subset(data.CannonAngle180_1,E_Ori=='')
    data.CannonAngle180_1=subset(data.CannonAngle180_1,CannonAngle=="180")
    data.CannonAngle180_1=subset(data.CannonAngle180_1,sse==0)
    a=c(data.CannonAngle180_1$TargetColor=="Blue")
    data.CannonAngle180_1$TargetColor1[a]=1
    data.CannonAngle180_1$TargetColor1[!a]=2
    input_pattern=data.CannonAngle180_1$TargetColor1
    data.hidden.act=data.CannonAngle180_1[,18:117]#10*10
    TargetCannon_180_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_180_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_180_Num_1=sum(TargetCannon_180_rsquare_01_1)
    # limit=c(0,1)
    # p=heatmapplot(TargetCannon_180_rsquare,limit)
    # ggsave(p, file=paste("TargetCannon_180_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_180_rsquare_01,paste(pa,'/','TargetCannon_180_rsquare_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_180_rsquare,paste(pa,'/','TargetCannon_180_rsquare_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##compute share neuron number of two target Cannon in two CannonAngle conditions
    share_targetCannon_0180_1=TargetCannon_0_rsquare_01_1+TargetCannon_180_rsquare_01_1
    a=c(share_targetCannon_0180_1==2)
    share_targetCannon_0180_num_1=sum(a)
    
    ##TargetCannon_Up_Num
    data.TargetCannon_Up_1=subset(data.raw,E_Ori=='E_U')
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Ratio=='High')
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,TargetColor!="")
    # data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation=="U")
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation!="D")
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation!="DL")
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation!="DR")
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation!="L")
    data.TargetCannon_Up_1=subset(data.TargetCannon_Up_1,Orientation!="R")
    a=c(data.TargetCannon_Up_1$TargetColor=="Red")
    data.TargetCannon_Up_1$TargetColor1[a]=1
    data.TargetCannon_Up_1$TargetColor1[!a]=2
    input_pattern=data.TargetCannon_Up_1$TargetColor1
    data.hidden.act=data.TargetCannon_Up_1[,18:117]#10*10
    TargetCannon_Up_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_Up_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_Up_Num_1=sum(TargetCannon_Up_rsquare_01_1)
    # p=heatmapplot(TargetCannon_Up_rsquare,limit)
    # ggsave(p, file=paste("TargetCannon_Up_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_Up_rsquare_01,paste(pa,'/','TargetCannon_Up_rsquare_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_Up_rsquare,paste(pa,'/','TargetCannon_Up_rsquare_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##TargetCannon_Down_Num
    data.TargetCannon_Down_1=subset(data.raw,E_Ori=='E_U')
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Ratio=='High')
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,TargetColor!="")
    # data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation=="D")
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation!="U")
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation!="UL")
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation!="UR")
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation!="L")
    data.TargetCannon_Down_1=subset(data.TargetCannon_Down_1,Orientation!="R")
    a=c(data.TargetCannon_Down_1$TargetColor=="Red")
    data.TargetCannon_Down_1$TargetColor1[a]=1
    data.TargetCannon_Down_1$TargetColor1[!a]=2
    input_pattern=data.TargetCannon_Down_1$TargetColor1
    data.hidden.act=data.TargetCannon_Down_1[,18:117]#10*10
    TargetCannon_Down_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_Down_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_Down_Num_1=sum(TargetCannon_Down_rsquare_01_1)
    # p=heatmapplot(TargetCannon_Down_rsquare,limit)
    # ggsave(p, file=paste("TargetCannon_Down_rsquare_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_Down_rsquare_01,paste(pa,'/','TargetCannon_Down_rsquare_01_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_Down_rsquare,paste(pa,'/','TargetCannon_Down_rsquare_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##compute share neuron number of two target Cannon in two orientations conditions
    share_targetCannon_UpDown_1=TargetCannon_Up_rsquare_01_1+TargetCannon_Down_rsquare_01_1
    a=c(share_targetCannon_UpDown_1==2)
    share_targetCannon_UpDown_num_1=sum(a)
    TargetCannon_Down_Num_1
    TargetCannon_Up_Num_1
    share_targetCannon_UpDown_num_1
    
    
    ##TargetCannon_Salience_Low
    data.salience_Low_1=subset(data.raw,Ratio!='')
    data.salience_Low_1=subset(data.salience_Low_1,E_Ori=="")
    data.salience_Low_1=subset(data.salience_Low_1,Ratio=="Low")
    a=c(data.salience_Low_1$TargetColor=="Blue")
    data.salience_Low_1$TargetColor1[a]=1
    data.salience_Low_1$TargetColor1[!a]=2
    input_pattern=data.salience_Low_1$TargetColor1
    data.hidden.act=data.salience_Low_1[,18:117]#10*10
    TargetCannon_salience_Low_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_Low_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_Low_Num_1=sum(TargetCannon_salience_Low_rsquare_01_1)
    # p=heatmapplot(TargetCannon_salience_Low_rsquare_1,limit)
    # ggsave(p, file=paste("TargetCannon_salience_Low_rsquare_1_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_salience_Low_rsquare_01_1,paste(pa,'/','TargetCannon_salience_Low_rsquare_01_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_salience_Low_rsquare_1,paste(pa,'/','TargetCannon_salience_Low_rsquare_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##TargetCannon_Salience_Medium
    data.salience_Medium_1=subset(data.raw,Ratio!='')
    data.salience_Medium_1=subset(data.salience_Medium_1,E_Ori=="")
    data.salience_Medium_1=subset(data.salience_Medium_1,Ratio=="Medium")
    a=c(data.salience_Medium_1$TargetColor=="Blue")
    data.salience_Medium_1$TargetColor1[a]=1
    data.salience_Medium_1$TargetColor1[!a]=2
    input_pattern=data.salience_Medium_1$TargetColor1
    data.hidden.act=data.salience_Medium_1[,18:117]#10*10
    TargetCannon_salience_Medium_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_Medium_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_Medium_Num_1=sum(TargetCannon_salience_Medium_rsquare_01_1)
    # p=heatmapplot(TargetCannon_salience_Medium_rsquare_1,limit)
    # ggsave(p, file=paste("TargetCannon_salience_Medium_rsquare_1_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_salience_Medium_rsquare_01_1,paste(pa,'/','TargetCannon_salience_Medium_rsquare_01_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_salience_Medium_rsquare_1,paste(pa,'/','TargetCannon_salience_Medium_rsquare_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##TargetCannon_Salience_High
    data.salience_High_1=subset(data.raw,Ratio!='')
    data.salience_High_1=subset(data.salience_High_1,E_Ori=="")
    data.salience_High_1=subset(data.salience_High_1,Ratio=="High")
    data.salience_High_1$CannonAngle=1
    a=c(data.salience_High_1$CannonAngle>=0)
    data.salience_High_1$CannonAngle1[a]=0
    data.salience_High_1=subset(data.salience_High_1,CannonAngle==1)
    a=c(data.salience_High_1$TargetColor=="Blue")
    data.salience_High_1$TargetColor1[a]=1
    data.salience_High_1$TargetColor1[!a]=2
    input_pattern=data.salience_High_1$TargetColor1
    data.hidden.act=data.salience_High_1[,18:117]#10*10
    TargetCannon_salience_High_rsquare_1=rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_High_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
    TargetCannon_salience_High_Num_1=sum(TargetCannon_salience_High_rsquare_01_1)
    # p=heatmapplot(TargetCannon_salience_High_rsquare_1,limit)
    # ggsave(p, file=paste("TargetCannon_salience_High_rsquare_1_test_",threshold,".png",sep=''),
    #        width=4, height=4, dpi=150, bg="transparent",path=pa)
    # write.table(TargetCannon_salience_High_rsquare_01_1,paste(pa,'/','TargetCannon_salience_High_rsquare_01_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    # write.table(TargetCannon_salience_High_rsquare_1,paste(pa,'/','TargetCannon_salience_High_rsquare_1_test',n,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##compute share neuron number of three saliences
    share_salience_1=TargetCannon_salience_High_rsquare_01_1+TargetCannon_salience_Medium_rsquare_01_1+TargetCannon_salience_Low_rsquare_01_1
    a=c(share_salience_1==3)
    share_salience_num_1=sum(a)
    
    TLoc_Num_1
    Blue_TLoc_Num_1
    Red_TLoc_Num_1
    share_BlueRed_target_num_1
    share_Blue_target_num_1
    share_Red_target_num_1
    share_BlueRed_target_num_3_1
    
    
    EOri_Num
    BOri_Num_1
    ROri_Num_1
    share_BlueRed_Cannon_num_1
    share_BlueEFOR_num
    share_RedEFOR_num
    share_BlueRedEFOR_num
    
    
    TargetCannon_0_Num_1
    TargetCannon_180_Num_1
    share_targetCannon_0180_num_1
    
    TargetCannon_Up_Num_1
    TargetCannon_Down_Num_1
    share_targetCannon_UpDown_num_1
    
    TargetCannon_salience_Low_Num_1
    TargetCannon_salience_Medium_Num_1
    TargetCannon_salience_High_Num_1
    share_salience_num_1
    
    
    totalnumber=data.frame(
      EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
      TLoc_Num_1,Blue_TLoc_Num_1,Red_TLoc_Num_1,share_BlueRed_target_num_1,share_Blue_target_num_1,share_Red_target_num_1,share_BlueRed_target_num_3_1,
      TargetCannon_0_Num_1,TargetCannon_180_Num_1,share_targetCannon_0180_num_1,
      TargetCannon_Up_Num_1,TargetCannon_Down_Num_1,share_targetCannon_UpDown_num_1,
      TargetCannon_salience_Low_Num_1,TargetCannon_salience_Medium_Num_1,TargetCannon_salience_High_Num_1,share_salience_num_1
    )
    write.table(totalnumber,paste(pa,'/','cycle10_Total Number_',threshold,'_',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##add each subject neuron number into one data.frame
    tt[sub,]=totalnumber[1,]
    
  }
  
  col=ncol(tt)
  row=nrow(tt)
  for(ii in c(1:col)) 
  {
    tt[sub+1,ii]=mean(tt[1:row,ii])
  }
  
  pa1=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results',sep='')
  ##wirte all subjects neuron number out
  write.table(tt,paste(pa1,'/','cycle10_Total Number_',threshold,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
}   





