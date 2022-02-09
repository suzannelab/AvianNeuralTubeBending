### Initialisation
# working directory is defined (optional but avoids specifying for evey function)
# setwd("D:/Current_work/tests/test_r_daniela")
# setwd("U:/DanielaR/Suzanne Lab data/images/LSM880/Immuno/20171208-Cas3-Phallo/4ss/measurements")
# setwd("D:/images/LSM880/Immuno/20171208-Cas3-Phallo/3ss/measurements_point")
setwd("/home/admin-suz/Documents/Code/Daniela/Daniela/")
library(stringr)

### Input
# all files of the directory ending with {"_" then a number then either 'r' or 'l' then ".txt"} are imported into an array called files
# (replace "[rl]" with either "r" or "l" for stricter selection)
files<-list.files(recursive=TRUE, full.names=FALSE, pattern="*_[0-9]+-[rl].txt")

### Main
# the data is reformatted into a list of arrays of equal length per embryo

# user parameter
nbins<-100 # expected length of array (number of points of curve), change as required

xdata=(1:nbins)*100/nbins # xdata is the abscissa array, in percentage of distance from midline

# dataframe result for several embryos ('need a loop to fill this table for different embryos)
df_embryos = data.frame(row.names=xdata)
embryo_name = 'embryo1'

embryos<-list()

df_one_embryo = data.frame(row.names=xdata) # Contains data for one embryo

i=0
for (filename in files) {
  data<-read.table(filename,header=TRUE)
  
  xy_norm<-array(0*c(1:nbins),dim=c(nbins,1))
  for (a in data[[1]]) {
    p<-ceiling(a*nbins/100) # index depending on the binning
    xy_norm[p]<-xy_norm[p]+1 # adding 1 for each event at relative length a
  }
  name=str_sub(filename,0,regexpr("_[0-9]+-[rl].txt",filename)[1]-1) # extracting embryo name (replace "[rl]" with either "r" or "l" for stricter selection)
  aux<-names(embryos)
  
  embryos[[name]][1+length(embryos[[name]])]<-list(xy_norm) # storing per embryo
  df_one_embryo[, as.character(i)]<-xy_norm
  i<-i+1
}

# combine all slice in one columns
df_embryos[, as.character(embryo_name)] <- rowSums(df_one_embryo)
format(df_embryos[, as.character(embryo_name)], trim = F)

# Saving dataframe
savefile<-file(paste("all_embryo",nbins,"bins.csv",sep=''))
write.csv(df_one_embryo, savefile, row.names = TRUE)

savefile<-file(paste("sum_all_embryo",nbins,"bins.csv",sep=''))
write.csv(df_embryos, savefile, row.names = TRUE)


### Calculations and saving
# the data is saved and averaged for each embryo
embryos_avg<-list()
for (e in names(embryos)) {
  #savefile<-file(paste("agreg",nbins,"bins_",e,".txt",sep="")) # embryo name (e) and nbins appear in the file name
  aux<-NULL
  for (v in embryos[[e]]) {
    aux<-cbind(aux,v)
  }
  write(t(cbind(xdata,aux)),file=savefile,sep="\t",ncolumns=1+length(embryos[[e]])) # saving
  embryos_avg[[e]]<-list(rowMeans(aux)) # averaging
  close(savefile)
}

### Plot
# data from embryo no.i is plotted, with average
# (plot creates the graph window and axes, lines adds data to the same plot)
i <- 1
plot(xdata,embryos_avg[[i]][[1]],col="black",xlab="distance from midline (%)",ylab="nb of events","l",ylim=c(0,2*max(embryos_avg[[i]][[1]])))
#for (v in embryos[[i]]) {
#	lines(xdata,v,col="red")
#}
#lines(xdata,embryos_avg[[i]][[1]],col="black")

