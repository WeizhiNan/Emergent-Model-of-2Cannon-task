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



threshold=c('0.20')
##inite tt, the data frame combining all conditions
##Different FOR Representations
EOri_Num=c(100)
BOri_Num_1=c(100)
ROri_Num_1=c(100)
share_BlueRed_Cannon_num_1=c(100)
share_BlueEFOR_num=c(100)
share_RedEFOR_num=c(100)
share_BlueRedEFOR_num=c(100)  
BOri_Num_1_26=c(100)
ROri_Num_1_26=c(100)
BOri_Num_1_44=c(100)
ROri_Num_1_44=c(100)
BOri_Num_1_62=c(100)
ROri_Num_1_62=c(100)


tt=data.frame(
  EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
  BOri_Num_1_26,ROri_Num_1_26,
  BOri_Num_1_44,ROri_Num_1_44,
  BOri_Num_1_62,ROri_Num_1_62
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
  data.hidden.act=data.EFOR[,16:115]#10*10
  E_Orientation_rsquare=rsquare(input_pattern,data.hidden.act)
  E_Orientation_rsquare_01=Rsquare(input_pattern,data.hidden.act)
  EOri_Num=sum(E_Orientation_rsquare_01)
  # limit=c(0,1)
  # p=heatmapplot(E_Orientation_rsquare,limit)
  # ggsave(p, file=paste("E_Orientation_Rsquare_test_",threshold,".png",sep=''),
  #        width=4, height=4, dpi=150, bg="transparent",path=pa)
  # write.table(E_Orientation_rsquare_01,paste(pa,'/','E_Orientation_01_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(E_Orientation_rsquare,paste(pa,'/','Cycle10_E_Orientation_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
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
  data.hidden.act=data.BlueCannon_1[,16:115]#10*10
  B_Orientation_rsquare_1=rsquare(input_pattern,data.hidden.act)
  B_Orientation_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
  BOri_Num_1=sum(B_Orientation_rsquare_01_1)
  # limit=c(0,1)
  # p=heatmapplot(B_Orientation_rsquare,limit)
  # ggsave(p, file=paste("B_Orientation_Rsquare_test_",threshold,".png",sep=''),
  #        width=4, height=4, dpi=150, bg="transparent",path=pa)
  # write.table(B_Orientation_rsquare_01,paste(pa,'/','Cycle10_B_Orientation_01_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(B_Orientation_rsquare_1,paste(pa,'/','Cycle10_B_Orientation_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
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
  data.hidden.act=data.RedCannon_1[,16:115]#10*10
  R_Orientation_rsquare_1=rsquare(input_pattern,data.hidden.act)
  R_Orientation_rsquare_01_1=Rsquare(input_pattern,data.hidden.act)
  ROri_Num_1=sum(R_Orientation_rsquare_01_1)
  # limit=c(0,1)
  # p=heatmapplot(R_Orientation_rsquare,limit)
  # ggsave(p, file=paste("R_Orientation_Rsquare_test_",threshold,".png",sep=''),
  #        width=4, height=4, dpi=150, bg="transparent",path=pa)
  # write.table(R_Orientation_rsquare_01,paste(pa,'/','Cycle10_R_Orientation_01_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(R_Orientation_rsquare_1,paste(pa,'/','Cycle10_R_Orientation_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  
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
  

  #####Blue:Red=2:6
  
  data.BlueCannon_1_26=subset(data.raw,B_Ori!='')
  data.BlueCannon_1_26=subset(data.BlueCannon_1_26,Ratio=='2:6')
  a=c(data.BlueCannon_1_26$B_Ori=='B_UR')
  data.BlueCannon_1_26$BlueCannon[a]=1
  a=c(data.BlueCannon_1_26$B_Ori=='B_R')
  data.BlueCannon_1_26$BlueCannon[a]=2
  a=c(data.BlueCannon_1_26$B_Ori=='B_DR')
  data.BlueCannon_1_26$BlueCannon[a]=3
  a=c(data.BlueCannon_1_26$B_Ori=='B_D')
  data.BlueCannon_1_26$BlueCannon[a]=4
  a=c(data.BlueCannon_1_26$B_Ori=='B_DL')
  data.BlueCannon_1_26$BlueCannon[a]=5
  a=c(data.BlueCannon_1_26$B_Ori=='B_L')
  data.BlueCannon_1_26$BlueCannon[a]=6
  a=c(data.BlueCannon_1_26$B_Ori=='B_UL')
  data.BlueCannon_1_26$BlueCannon[a]=7
  a=c(data.BlueCannon_1_26$B_Ori=='B_U')
  data.BlueCannon_1_26$BlueCannon[a]=8
  ##BlueCannon_Orientation
  input_pattern=data.BlueCannon_1_26$BlueCannon
  data.hidden.act=data.BlueCannon_1_26[,16:115]#10*10
  B_Orientation_rsquare_1_26=rsquare(input_pattern,data.hidden.act)
  B_Orientation_rsquare_01_1_26=Rsquare(input_pattern,data.hidden.act)
  BOri_Num_1_26=sum(B_Orientation_rsquare_01_1_26)
#   limit=c(0,1)
#   p=heatmapplot(B_Orientation_rsquare_1_26,limit)
#   ggsave(p, file=paste("B_Orientation_Rsquare_26_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(B_Orientation_rsquare_01_26,paste(pa,'/','Cycle10_B_Orientation_01_26_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(B_Orientation_rsquare_1_26,paste(pa,'/','Cycle10_B_Orientation_26_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  
  
  
  data.RedCannon_1_26=subset(data.raw,R_Ori!='')
  data.RedCannon_1_26=subset(data.RedCannon_1_26,Ratio=='2:6')
  a=c(data.RedCannon_1_26$R_Ori=='R_UR')
  data.RedCannon_1_26$RedCannon[a]=1
  a=c(data.RedCannon_1_26$R_Ori=='R_R')
  data.RedCannon_1_26$RedCannon[a]=2
  a=c(data.RedCannon_1_26$R_Ori=='R_DR')
  data.RedCannon_1_26$RedCannon[a]=3
  a=c(data.RedCannon_1_26$R_Ori=='R_D')
  data.RedCannon_1_26$RedCannon[a]=4
  a=c(data.RedCannon_1_26$R_Ori=='R_DL')
  data.RedCannon_1_26$RedCannon[a]=5
  a=c(data.RedCannon_1_26$R_Ori=='R_L')
  data.RedCannon_1_26$RedCannon[a]=6
  a=c(data.RedCannon_1_26$R_Ori=='R_UL')
  data.RedCannon_1_26$RedCannon[a]=7
  a=c(data.RedCannon_1_26$R_Ori=='R_U')
  data.RedCannon_1_26$RedCannon[a]=8
  ##RedCannon_Orientation
  ##ordre data according to variable E_Ori
  input_pattern=data.RedCannon_1_26$RedCannon
  data.hidden.act=data.RedCannon_1_26[,16:115]#10*10
  R_Orientation_rsquare_1_26=rsquare(input_pattern,data.hidden.act)
  R_Orientation_rsquare_01_1_26=Rsquare(input_pattern,data.hidden.act)
  ROri_Num_1_26=sum(R_Orientation_rsquare_01_1_26)
#   limit=c(0,1)
#   p=heatmapplot(R_Orientation_rsquare_1_26,limit)
#   ggsave(p, file=paste("R_Orientation_Rsquare_26_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(R_Orientation_rsquare_01_26,paste(pa,'/','Cycle10_R_Orientation_01_26_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(R_Orientation_rsquare_1_26,paste(pa,'/','Cycle10_R_Orientation_26_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
 
  
   ##compute share neuron number of two color Cannons
  share_Cannon_1_26=B_Orientation_rsquare_01_1_26+R_Orientation_rsquare_01_1_26
  a=c(share_Cannon_1_26==2)
  share_BlueRed_Cannon_num_1_26=sum(a) 
  
  #####Blue:Red=6:2
  
  data.BlueCannon_1_62=subset(data.raw,B_Ori!='')
  data.BlueCannon_1_62=subset(data.BlueCannon_1_62,Ratio=='6:2')
  a=c(data.BlueCannon_1_62$B_Ori=='B_UR')
  data.BlueCannon_1_62$BlueCannon[a]=1
  a=c(data.BlueCannon_1_62$B_Ori=='B_R')
  data.BlueCannon_1_62$BlueCannon[a]=2
  a=c(data.BlueCannon_1_62$B_Ori=='B_DR')
  data.BlueCannon_1_62$BlueCannon[a]=3
  a=c(data.BlueCannon_1_62$B_Ori=='B_D')
  data.BlueCannon_1_62$BlueCannon[a]=4
  a=c(data.BlueCannon_1_62$B_Ori=='B_DL')
  data.BlueCannon_1_62$BlueCannon[a]=5
  a=c(data.BlueCannon_1_62$B_Ori=='B_L')
  data.BlueCannon_1_62$BlueCannon[a]=6
  a=c(data.BlueCannon_1_62$B_Ori=='B_UL')
  data.BlueCannon_1_62$BlueCannon[a]=7
  a=c(data.BlueCannon_1_62$B_Ori=='B_U')
  data.BlueCannon_1_62$BlueCannon[a]=8
  ##BlueCannon_Orientation
  input_pattern=data.BlueCannon_1_62$BlueCannon
  data.hidden.act=data.BlueCannon_1_62[,16:115]#10*10
  B_Orientation_rsquare_1_62=rsquare(input_pattern,data.hidden.act)
  B_Orientation_rsquare_01_1_62=Rsquare(input_pattern,data.hidden.act)
  BOri_Num_1_62=sum(B_Orientation_rsquare_01_1_62)
#   limit=c(0,1)
#   p=heatmapplot(B_Orientation_rsquare_1_62,limit)
#   ggsave(p, file=paste("B_Orientation_Rsquare_26_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(B_Orientation_rsquare_01_62,paste(pa,'/','Cycle10_B_Orientation_01_62_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(B_Orientation_rsquare_1_62,paste(pa,'/','Cycle10_B_Orientation_62_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  
  
   
  data.RedCannon_1_62=subset(data.raw,R_Ori!='')
  data.RedCannon_1_62=subset(data.RedCannon_1_62,Ratio=='6:2')
  a=c(data.RedCannon_1_62$R_Ori=='R_UR')
  data.RedCannon_1_62$RedCannon[a]=1
  a=c(data.RedCannon_1_62$R_Ori=='R_R')
  data.RedCannon_1_62$RedCannon[a]=2
  a=c(data.RedCannon_1_62$R_Ori=='R_DR')
  data.RedCannon_1_62$RedCannon[a]=3
  a=c(data.RedCannon_1_62$R_Ori=='R_D')
  data.RedCannon_1_62$RedCannon[a]=4
  a=c(data.RedCannon_1_62$R_Ori=='R_DL')
  data.RedCannon_1_62$RedCannon[a]=5
  a=c(data.RedCannon_1_62$R_Ori=='R_L')
  data.RedCannon_1_62$RedCannon[a]=6
  a=c(data.RedCannon_1_62$R_Ori=='R_UL')
  data.RedCannon_1_62$RedCannon[a]=7
  a=c(data.RedCannon_1_62$R_Ori=='R_U')
  data.RedCannon_1_62$RedCannon[a]=8
  ##RedCannon_Orientation
  ##ordre data according to variable E_Ori
  input_pattern=data.RedCannon_1_62$RedCannon
  data.hidden.act=data.RedCannon_1_62[,16:115]#10*10
  R_Orientation_rsquare_1_62=rsquare(input_pattern,data.hidden.act)
  R_Orientation_rsquare_01_1_62=Rsquare(input_pattern,data.hidden.act)
  ROri_Num_1_62=sum(R_Orientation_rsquare_01_1_62)
#   limit=c(0,1)
#   p=heatmapplot(R_Orientation_rsquare_1_62,limit)
#   ggsave(p, file=paste("R_Orientation_Rsquare_62_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(R_Orientation_rsquare_01_62,paste(pa,'/','Cycle10_R_Orientation_01_62_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(R_Orientation_rsquare_1_62,paste(pa,'/','Cycle10_R_Orientation_62_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")

  
    ##compute share neuron number of two color Cannons
  share_Cannon_1_62=B_Orientation_rsquare_01_1_62+R_Orientation_rsquare_01_1_62
  a=c(share_Cannon_1_62==2)
  share_BlueRed_Cannon_num_1_62=sum(a)  
  
  #####Blue:Red=4:4
  
  data.BlueCannon_1_44=subset(data.raw,B_Ori!='')
  data.BlueCannon_1_44=subset(data.BlueCannon_1_44,Ratio=='4:4')
  a=c(data.BlueCannon_1_44$B_Ori=='B_UR')
  data.BlueCannon_1_44$BlueCannon[a]=1
  a=c(data.BlueCannon_1_44$B_Ori=='B_R')
  data.BlueCannon_1_44$BlueCannon[a]=2
  a=c(data.BlueCannon_1_44$B_Ori=='B_DR')
  data.BlueCannon_1_44$BlueCannon[a]=3
  a=c(data.BlueCannon_1_44$B_Ori=='B_D')
  data.BlueCannon_1_44$BlueCannon[a]=4
  a=c(data.BlueCannon_1_44$B_Ori=='B_DL')
  data.BlueCannon_1_44$BlueCannon[a]=5
  a=c(data.BlueCannon_1_44$B_Ori=='B_L')
  data.BlueCannon_1_44$BlueCannon[a]=6
  a=c(data.BlueCannon_1_44$B_Ori=='B_UL')
  data.BlueCannon_1_44$BlueCannon[a]=7
  a=c(data.BlueCannon_1_44$B_Ori=='B_U')
  data.BlueCannon_1_44$BlueCannon[a]=8
  ##BlueCannon_Orientation
  input_pattern=data.BlueCannon_1_44$BlueCannon
  data.hidden.act=data.BlueCannon_1_44[,16:115]#10*10
  B_Orientation_rsquare_1_44=rsquare(input_pattern,data.hidden.act)
  B_Orientation_rsquare_01_1_44=Rsquare(input_pattern,data.hidden.act)
  BOri_Num_1_44=sum(B_Orientation_rsquare_01_1_44)
#   limit=c(0,1)
#   p=heatmapplot(B_Orientation_rsquare_1_44,limit)
#   ggsave(p, file=paste("B_Orientation_Rsquare_26_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(B_Orientation_rsquare_01_44,paste(pa,'/','Cycle10_B_Orientation_01_44_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(B_Orientation_rsquare_1_44,paste(pa,'/','Cycle10_B_Orientation_44_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  data.RedCannon_1_44=subset(data.raw,R_Ori!='')
  data.RedCannon_1_44=subset(data.RedCannon_1_44,Ratio=='4:4')
  a=c(data.RedCannon_1_44$R_Ori=='R_UR')
  data.RedCannon_1_44$RedCannon[a]=1
  a=c(data.RedCannon_1_44$R_Ori=='R_R')
  data.RedCannon_1_44$RedCannon[a]=2
  a=c(data.RedCannon_1_44$R_Ori=='R_DR')
  data.RedCannon_1_44$RedCannon[a]=3
  a=c(data.RedCannon_1_44$R_Ori=='R_D')
  data.RedCannon_1_44$RedCannon[a]=4
  a=c(data.RedCannon_1_44$R_Ori=='R_DL')
  data.RedCannon_1_44$RedCannon[a]=5
  a=c(data.RedCannon_1_44$R_Ori=='R_L')
  data.RedCannon_1_44$RedCannon[a]=6
  a=c(data.RedCannon_1_44$R_Ori=='R_UL')
  data.RedCannon_1_44$RedCannon[a]=7
  a=c(data.RedCannon_1_44$R_Ori=='R_U')
  data.RedCannon_1_44$RedCannon[a]=8
  ##RedCannon_Orientation
  ##ordre data according to variable E_Ori
  input_pattern=data.RedCannon_1_44$RedCannon
  data.hidden.act=data.RedCannon_1_44[,16:115]#10*10
  R_Orientation_rsquare_1_44=rsquare(input_pattern,data.hidden.act)
  R_Orientation_rsquare_01_1_44=Rsquare(input_pattern,data.hidden.act)
  ROri_Num_1_44=sum(R_Orientation_rsquare_01_1_44) 
#   limit=c(0,1)
#   p=heatmapplot(R_Orientation_rsquare_1_44,limit)
#   ggsave(p, file=paste("R_Orientation_Rsquare_44_test_",threshold,".png",sep=''),
#          width=4, height=4, dpi=150, bg="transparent",path=pa)
#   write.table(R_Orientation_rsquare_01_44,paste(pa,'/','Cycle10_R_Orientation_01_44_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  write.table(R_Orientation_rsquare_1_44,paste(pa,'/','Cycle10_R_Orientation_44_test',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  ##compute share neuron number of two color Cannons
  share_Cannon_1_44=B_Orientation_rsquare_01_1_44+R_Orientation_rsquare_01_1_44
  a=c(share_Cannon_1_44==2)
  share_BlueRed_Cannon_num_1_44=sum(a)  
  
  
  
  EOri_Num
  BOri_Num_1
  ROri_Num_1
  share_BlueRed_Cannon_num_1
  share_BlueEFOR_num
  share_RedEFOR_num
  share_BlueRedEFOR_num
  BOri_Num_1_26
  ROri_Num_1_26
  share_BlueRed_Cannon_num_1_26
  BOri_Num_1_44
  ROri_Num_1_44
  share_BlueRed_Cannon_num_1_44
  BOri_Num_1_62
  ROri_Num_1_62
  share_BlueRed_Cannon_num_1_62

  
  totalnumber=data.frame(
    EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
    BOri_Num_1_26,ROri_Num_1_26,share_BlueRed_Cannon_num_1_26,
    BOri_Num_1_44,ROri_Num_1_44,share_BlueRed_Cannon_num_1_44,
    BOri_Num_1_62,ROri_Num_1_62,share_BlueRed_Cannon_num_1_62
  )
  write.table(totalnumber,paste(pa,'/','Ratio_cycle10_Total Number_',threshold,'_',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
  
  ##add each subject neuron number into one data.frame
  tt[sub,]=totalnumber[1,]
  print(sub)
}












##Rsquare function
Rsquare_01=function(rsquare,thr) {
  r=matrix(c(0),nrow=10,ncol=10) 
  R.square=r
  for (i in c(1:10)){
    for (j in c(2:11)){   
      if (rsquare[i,j]>thr)(R.square[i,j-1]=1)
    }
  }
  R.square
}


for (threshold in c('0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50',
                    '0.55','0.60','0.65','0.70','0.75','0.80','0.85','0.90','0.95')){
  # threshold='0.20'
  ##inite tt, the data frame combining all conditions
  ##Different FOR Representations
  EOri_Num=c(100)
  BOri_Num_1=c(100)
  ROri_Num_1=c(100)
  share_BlueRed_Cannon_num_1=c(100)
  share_BlueEFOR_num=c(100)
  share_RedEFOR_num=c(100)
  share_BlueRedEFOR_num=c(100)  
  BOri_Num_1_26=c(100)
  ROri_Num_1_26=c(100)
  share_BlueRed_Cannon_num_1_26=c(100)
  BOri_Num_1_44=c(100)
  ROri_Num_1_44=c(100)
  share_BlueRed_Cannon_num_1_44=c(100)
  BOri_Num_1_62=c(100)
  ROri_Num_1_62=c(100)
  share_BlueRed_Cannon_num_1_62=c(100)
  
  
  tt=data.frame(
    EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
    BOri_Num_1_26,ROri_Num_1_26,share_BlueRed_Cannon_num_1_26,
    BOri_Num_1_44,ROri_Num_1_44,share_BlueRed_Cannon_num_1_44,
    BOri_Num_1_62,ROri_Num_1_62,share_BlueRed_Cannon_num_1_62
  )
  
  
  thr=as.numeric(threshold)
  for( sub in c(1:30)){
    # sub=17
    ##Set directory 
    pa0=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/',sub,sep='')
    pa=paste(pa0,'test_results',sep='/')
    EOri_rsquare<-read.table(paste(pa,'/','Cycle10_E_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    BOri_rsquare<-read.table(paste(pa,'/','Cycle10_B_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    ROri_rsquare<-read.table(paste(pa,'/','Cycle10_R_Orientation_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    BOri_rsquare_26<-read.table(paste(pa,'/','Cycle10_B_Orientation_26_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    ROri_rsquare_26<-read.table(paste(pa,'/','Cycle10_R_Orientation_26_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    BOri_rsquare_44<-read.table(paste(pa,'/','Cycle10_B_Orientation_44_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    ROri_rsquare_44<-read.table(paste(pa,'/','Cycle10_R_Orientation_44_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    BOri_rsquare_62<-read.table(paste(pa,'/','Cycle10_B_Orientation_62_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 
    ROri_rsquare_62<-read.table(paste(pa,'/','Cycle10_R_Orientation_62_test',sub,'.csv',sep=''),header=TRUE,sep=",",na.string=NULL) 

    EOri_rsquare_01<-Rsquare_01(EOri_rsquare,thr)
    BOri_rsquare_01<-Rsquare_01(BOri_rsquare,thr) 
    ROri_rsquare_01<-Rsquare_01(ROri_rsquare,thr) 
    BOri_rsquare_01_26<-Rsquare_01(BOri_rsquare_26,thr) 
    ROri_rsquare_01_26<-Rsquare_01(ROri_rsquare_26,thr)  
    BOri_rsquare_01_44<-Rsquare_01(BOri_rsquare_44,thr) 
    ROri_rsquare_01_44<-Rsquare_01(ROri_rsquare_44,thr)  
    BOri_rsquare_01_62<-Rsquare_01(BOri_rsquare_62,thr) 
    ROri_rsquare_01_62<-Rsquare_01(ROri_rsquare_62,thr) 
    
    
    EOri_Num=sum(EOri_rsquare_01)
    BOri_Num_1=sum(BOri_rsquare_01)
    ROri_Num_1=sum(ROri_rsquare_01)
    ##compute share neuron number of two color Cannons
    share_Cannon_1=BOri_rsquare_01+ROri_rsquare_01
    a=c(share_Cannon_1==2)
    share_BlueRed_Cannon_num_1=sum(a)
    ##compute share neuron number of blue Cannon and EFOR
    share_blueEFOR=BOri_rsquare_01+EOri_rsquare_01
    a=c(share_blueEFOR==2)
    share_BlueEFOR_num=sum(a)
    ##compute share neuron number of red Cannon and EFOR
    share_redEFOR=ROri_rsquare_01+EOri_rsquare_01
    a=c(share_redEFOR==2)
    share_RedEFOR_num=sum(a)
    ##compute share neuron number of three FORs
    share_threeFORs=ROri_rsquare_01+EOri_rsquare_01+BOri_rsquare_01
    a=c(share_threeFORs==3)
    share_BlueRedEFOR_num=sum(a)
    
    BOri_Num_1_26=sum(BOri_rsquare_01_26)
    ROri_Num_1_26=sum(ROri_rsquare_01_26) 
    ##compute share neuron number of two color Cannons
    share_Cannon_1_26=BOri_rsquare_01_26+ROri_rsquare_01_26
    a=c(share_Cannon_1_26==2)
    share_BlueRed_Cannon_num_1_26=sum(a)  
    
    BOri_Num_1_44=sum(BOri_rsquare_01_44)
    ROri_Num_1_44=sum(ROri_rsquare_01_44) 
    ##compute share neuron number of two color Cannons
    share_Cannon_1_44=BOri_rsquare_01_44+ROri_rsquare_01_44
    a=c(share_Cannon_1_44==2)
    share_BlueRed_Cannon_num_1_44=sum(a)  
    
    
    BOri_Num_1_62=sum(BOri_rsquare_01_62)
    ROri_Num_1_62=sum(ROri_rsquare_01_62)
    ##compute share neuron number of two color Cannons
    share_Cannon_1_62=BOri_rsquare_01_62+ROri_rsquare_01_62
    a=c(share_Cannon_1_62==2)
    share_BlueRed_Cannon_num_1_62=sum(a) 
    
    totalnumber=data.frame(
      EOri_Num,BOri_Num_1,ROri_Num_1,share_BlueRed_Cannon_num_1,share_BlueEFOR_num,share_RedEFOR_num,share_BlueRedEFOR_num,
      BOri_Num_1_26,ROri_Num_1_26,share_BlueRed_Cannon_num_1_26,
      BOri_Num_1_44,ROri_Num_1_44,share_BlueRed_Cannon_num_1_44,
      BOri_Num_1_62,ROri_Num_1_62,share_BlueRed_Cannon_num_1_62
      )
    write.table(totalnumber,paste(pa,'/','Ratio_cycle10_Total Number_',threshold,'_',sub,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
    
    ##add each subject neuron number into one data.frame
    tt[sub,]=totalnumber[1,]
    print(sub)
  }
  
  
  col=ncol(tt)
  row=nrow(tt)
  for(ii in c(1:col)) 
  {
    tt[sub+1,ii]=mean(tt[1:row,ii])
  }
  
  pa1=paste('C:/Users/nan/Emergent/learning networks/nan network/8Neuron3Salience_Cue_Target/trainseperately/test_results',sep='')
  ##wirte all subjects neuron number out
  write.table(tt,paste(pa1,'/','Ratio_cycle10_Total Number_',threshold,'.csv',sep=''),sep=',',col.names=NA,qmethod="double")
}   





