##### using bfast to decompose time series
##### csv.  row: records;   col:observations in each temporal interval

rm(list=ls())
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.7.0_75')
library(bfast)
library(xlsx)
#intial parameters
h=0.15
frequency1=12
max_iter=3
path1="D:\\SchoolWorks\\Research\\Long term Gaussian\\TEST\\Seasonal Effect\\BFAST_out test\\Hangzhou_bfast.csv"
#path1="D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\seasonal Test.csv"

#input and preprocessing
system.time({

  data1=as.matrix(read.csv(file=path1,header = FALSE,sep = ","))
  colnames(data1)=NULL
  size1=dim(data1)
  num_obs=dim(data1)[1]
  num_time=dim(data1)[2]
  trend_cp=matrix(0,num_obs,num_time)
  season_cp=matrix(0,num_obs,num_time)
  residual_cp=matrix(0,num_obs,num_time)
  tr_bp=matrix(0,num_obs,10)
  sn_bp=matrix(0,num_obs,10)
  
  for(i in 1:num_obs)
  {
    col1=data1[i,]
    test_col=sum((col1>0)*1) 
    if (test_col>num_time*h*2)
    {

      col1_ts=ts(data = col1,start = c(2012,4),frequency = 12)
      fit=bfast(Yt=col1_ts,h=0.15,season= "harmonic",max.iter = max_iter,breaks =2)
      len_fit=length(fit[["output"]])
      trend_cp[i,]=as.vector(fit[["output"]][[len_fit]][["Tt"]])
      season_cp[i,]=as.vector(fit[["output"]][[len_fit]][["St"]])
      residual_cp[i,]=as.vector(fit[["output"]][[len_fit]][["Nt"]])
      
      #trend bp
      if (!is.na(fit[["output"]][[len_fit]][["bp.Vt"]])[1])
      {
        num_vt_bp=length(fit[["output"]][[len_fit]][["bp.Vt"]][["breakpoints"]])
        tr_bp[i,1:num_vt_bp]=fit[["output"]][[len_fit]][["Vt.bp"]]
      }
      #season bp
      if (!is.na(fit[["output"]][[len_fit]][["bp.Wt"]])[1])
      {
        num_wt_bp=length(fit[["output"]][[len_fit]][["bp.Wt"]][["breakpoints"]])
        sn_bp[i,1:num_wt_bp]=fit[["output"]][[len_fit]][["Wt.bp"]]
      }
    }
  }
})
#write.csv(trend_cp,"D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\trend_cp.csv")
#write.csv(season_cp,"D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\season_cp.csv")
#write.csv(residual_cp,"D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\residual_cp.csv")
#write.csv(tr_bp,"D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\tr_bp.csv")
#write.csv(sn_bp,"D:\\SchoolWorks\\Research\\Long term Gaussian\\出图数据\\sensitivity\\sn_bp.csv")

