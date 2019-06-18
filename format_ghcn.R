# format.ghcn()
# re-format GHCN weather station data downloaded from GHCND ftp site
# last updated: 2016-08-16

library(chron)

###########################################################
# read GHCN data format
format.ghcn<-function(path, 
                    file.name,
                    out.path
                    ){
  data.txt = readLines(con=(paste(path, file.name, sep="")), 
                       n=-1, 
                       ok=T, 
                       warn=T)
  
  # matrix for data
  data.mat = data.frame(matrix(nr=length(data.txt), nc=(4+31)))
  
  # matrix for flags
  mflag.mat = data.frame(matrix(nr=length(data.txt), nc=(4+31)))
  qflag.mat = data.frame(matrix(nr=length(data.txt), nc=(4+31)))
  sflag.mat = data.frame(matrix(nr=length(data.txt), nc=(4+31)))
  
  # station name
  stnm.mat  = data.frame(matrix(nr=length(data.txt), nc=(4+31)))
  
  for(i in 1:length(data.txt)){
    data.mat[i,1] = mflag.mat[i,1] = qflag.mat[i,1] = sflag.mat[i,1] <- substr(data.txt[i],  start=1,  stop=11)  # ID
    data.mat[i,2] = mflag.mat[i,2] = qflag.mat[i,2] = sflag.mat[i,2] <- substr(data.txt[i],  start=12, stop=15)  # year
    data.mat[i,3] = mflag.mat[i,3] = qflag.mat[i,3] = sflag.mat[i,3] <- substr(data.txt[i],  start=16, stop=17)  # month
    data.mat[i,4] = mflag.mat[i,4] = qflag.mat[i,4] = sflag.mat[i,4] <- substr(data.txt[i],  start=18, stop=21)  # element
    
    for(k in 1:31){
      s = 22+(k-1)*8
      data.mat[i,(4+k)]  <-substr(data.txt[i],  start=s,   stop=s+4)  # daily value
      mflag.mat[i,(4+k)] <-substr(data.txt[i],  start=s+5, stop=s+5)  # mflag for daily value
      qflag.mat[i,(4+k)] <-substr(data.txt[i],  start=s+6, stop=s+6)  # qflag for daily value
      sflag.mat[i,(4+k)] <-substr(data.txt[i],  start=s+7, stop=s+7)  # sflag for daily value
    }
  }
  
  n.data=unlist(unique(data.mat[4]))
  for(n in 1:length(n.data)){
    s.data<-subset(data.mat,   data.mat[4] == as.character(n.data[n]))
    mflag <-subset(mflag.mat, mflag.mat[4] == as.character(n.data[n]))
    qflag <-subset(qflag.mat, qflag.mat[4] == as.character(n.data[n]))
    sflag <-subset(sflag.mat, sflag.mat[4] == as.character(n.data[n]))
    
    month.lengths<-c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    month.lengths.leap<-c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    
    dts <- mat.or.vec(nr=nrow(s.data)*31,nc=1)
    vals <- mat.or.vec(nr=nrow(s.data)*31,nc=1)
    mflag.vals <- mat.or.vec(nr=nrow(s.data)*31,nc=1)
    qflag.vals <- mat.or.vec(nr=nrow(s.data)*31,nc=1)
    sflag.vals <- mat.or.vec(nr=nrow(s.data)*31,nc=1)
    
    for(m in 1:nrow(s.data)){
      for(d in 1:31){
        if(d < 10){
          day <- as.character(paste("0",d,sep=""))
        }else{
          day = as.character(d)
        }
        dt <- paste(as.character(s.data[m,2]), as.character(s.data[m,3]), day, sep="-")
        dts[(m-1)*31+d]<-dt
        
        vals[(m-1)*31+d]<-as.numeric(s.data[m,d+4])
        
        mflag.vals[(m-1)*31+d]<-as.numeric(mflag[m,d+4])
        qflag.vals[(m-1)*31+d]<-as.numeric(qflag[m,d+4])
        sflag.vals[(m-1)*31+d]<-as.numeric(sflag[m,d+4])
      }
    }
    
    data.ts<-as.data.frame(cbind(dts, vals, 
                                 mflag.vals, qflag.vals, sflag.vals))
    data.ts<-subset(data.ts, data.ts[,1]!="NA")
    data.ts[data.ts==-9999]<-c(NA)
    colnames(data.ts)<-c("date", n.data[n], "mflag", "qflag", "sflag")
    out.nm<-paste(out.path, data.mat[i,1], "_", n.data[n], ".csv", sep="")
    write.table(data.ts, out.nm, row.names=F, sep=",")
  }
  
}
###########################################################



