library(zoo)
library(dplyr)
library(plyr)
##################################################################################################
################ DATA IMPORT AND PROCESSING BEFORE APPLYING THE DAG-IS METHODOLOGY ###############
##################################################################################################

                             # Participant VS SIP Timestamps #

############################## Natural Time 1-sec resolution ###################################
nbboPart<-read.csv('./csvData/cq20161003nbboPart.csv', sep = ',', header = TRUE)
nbboSIP<-read.csv('./csvData/cq20161003nbboSIP.csv', sep = ',', header = TRUE)

IBM.part<-nbboPart[which(nbboPart[,2]=="IBM"),] # participant timestamps for IBM 
array.IBMpart<-matrix(replicate(23405*3,0), nrow=23405, ncol = 3)
colnames(array.IBMpart)<-c("timeMid","bBid","bOfr")
array.IBMpart[,1]<-c(34200:57604)  ### participant timestamps for IBM from 9.30am to 16.30pm

bBid.IBM<-IBM.part[which(!is.na(IBM.part[,4])),c(3,4)]
bOfr.IBM<-IBM.part[which(!is.na(IBM.part[,5])),c(3,5)]

# take median values in each second interval (in alternative, take the last available obs. in a given second interval)
for (i in 1:23404) {
  array.IBMpart[i+1,2]<-median(bBid.IBM[which(bBid.IBM[,1]>= array.IBMpart[i,1] & bBid.IBM[,1]<= array.IBMpart[i+1,1]),2])
  array.IBMpart[i+1,3]<-median(bOfr.IBM[which(bOfr.IBM[,1]>= array.IBMpart[i,1] & bOfr.IBM[,1]<= array.IBMpart[i+1,1]),2]) 
  
}
array.IBMpart<-array.IBMpart[-1,]

IBM.sip<-nbboSIP[which(nbboSIP[,2]=="IBM"),] ### SIP timestamps for IBM
array.IBMsip<-matrix(replicate(23405*3,0), nrow=23405, ncol = 3)
colnames(array.IBMsip)<-c("timeMid","SIP.bBid","SIP.bOfr")
array.IBMsip[,1]<-c(34200:57604)

sip.bBid.IBM<-IBM.sip[which(!is.na(IBM.sip[,4])),c(3,4)]
sip.bOfr.IBM<-IBM.sip[which(!is.na(IBM.sip[,5])),c(3,5)]

for (i in 1:23404) {
  array.IBMsip[i+1,2]<-median(sip.bBid.IBM[which(sip.bBid.IBM[,1]>= array.IBMsip[i,1] & sip.bBid.IBM[,1]<= array.IBMsip[i+1,1]),2])
  array.IBMsip[i+1,3]<-median(sip.bOfr.IBM[which(sip.bOfr.IBM[,1]>= array.IBMsip[i,1] & sip.bOfr.IBM[,1]<= array.IBMsip[i+1,1]),2]) 
  
}
array.IBMsip<-array.IBMsip[-1,]

part.sip.IBMdata<-cbind.data.frame(array.IBMpart,array.IBMsip[,c(2,3)])
df.tsIBM<-na.locf(part.sip.IBMdata) ## DATASET FINALE PER ANALISI
colnames(df.tsIBM)<-c("timeMid","NBBpart","NBOpart","NBBsip","NBOsip")
write.csv(df.tsIBM, file = 'df.tsIBM.csv', row.names = FALSE)

############################################ Event-time ##########################################
#the time-counter t is incremented whenever there is an update to any variable 
#involved in the system.I increase the time counter whenever there is an update to ANY variable,
#and I propagate simultaneously the prices of the other variables which have no update. 
#Thus missing values are not excluded but replaced by previous prices. 

events.IBM<-full_join(IBM.part, IBM.sip, by = "timeMid")
events.IBM<-events.IBM[order(events.IBM$timeMid),]
events.IBM<-events.IBM[,c(3,4,5,8,9)]
events.IBM<-events.IBM[-1,]
events.IBM[1,2]<-153.58
events.IBM[c(1:6),4]<-153.58
events.IBM[c(1:5),5]<-166
lastevents.IBM<-na.locf(events.IBM)
colnames(lastevents.IBM)<-c("timeMid","NBBpart","NBOpart","NBBsip","NBOsip")
lastevents.IBM <- lastevents.IBM[105:nrow(lastevents.IBM),]
rownames(lastevents.IBM) <- c(1:nrow(lastevents.IBM))

write.csv(lastevents.IBM, file='lastevents.IBM.csv', row.names = FALSE)
###########################################################################################################

                                 # Primary Listing VS Others #

######################################## Event-Time ##########################################
IBM20161003CQLexPart<-read.csv('./csvData/IBM20161003CQLexPart.csv', sep = ',', header = TRUE)
IBMlistVSother<-na.locf(IBM20161003CQLexPart)
write.csv(IBMlistVSother, file = 'IBMlistVSother.csv', row.names = FALSE)

####################################Natural Time 1-sec ########################################
array.IBMex<-matrix(replicate(25913*5,0), nrow=25913, ncol = 5)
colnames(array.IBMex)<-c("timeMid","NBBother","NBOother","NBBlist","NBOlist")
array.IBMex[,1]<-c(34200:60112)
IBM<-IBM20161003CQLexPart
NBBother<-IBM[which(!is.na(IBM[,4])),c(3,4)]
NBOother<-IBM[which(!is.na(IBM[,5])),c(3,5)]
NBBlist<-IBM[which(!is.na(IBM[,6])),c(3,6)]
NBOlist<-IBM[which(!is.na(IBM[,7])),c(3,7)]
for (i in 1:25912) {
  array.IBMex[i+1,2]<-median(NBBother[which(NBBother[,1]>= array.IBMex[i,1] & NBBother[,1]<= array.IBMex[i+1,1]),2])
  array.IBMex[i+1,3]<-median(NBOother[which(NBOother[,1]>= array.IBMex[i,1] & NBOother[,1]<= array.IBMex[i+1,1]),2]) 
  array.IBMex[i+1,4]<-median(NBBlist[which(NBBlist[,1]>= array.IBMex[i,1] & NBBlist[,1]<= array.IBMex[i+1,1]),2])
  array.IBMex[i+1,5]<-median(NBOlist[which(NBOlist[,1]>= array.IBMex[i,1] & NBOlist[,1]<= array.IBMex[i+1,1]),2]) 
}
array.IBMex<-array.IBMex[-1,]
df.exIBM<-na.locf(array.IBMex)

write.csv(df.exIBM, file = 'df.exIBM.csv', row.names = FALSE)


                                # Quotes VS Lit/Dark Trades #
ct20161003Part<-read.csv('./csvData/ct20161003Part.csv', sep = ',', header = TRUE)

################################# Event-Time ##################################################
IBM.part<-nbboPart[which(nbboPart[,2]=="IBM"),] ### participant timestamps for IBM
events.IBMpart<-IBM.part[,c(3,4,5)]
part.data<-na.locf(events.IBMpart)
part.data<-part.data[-1,]

darktrades<-ct20161003Part[which(ct20161003Part[,2]=="D" & ct20161003Part[,3]=="IBM"),c(4,5)]
colnames(darktrades)<-c("D","timeMid")

littrades<-ct20161003Part[which(ct20161003Part[,2]!="D" & ct20161003Part[,3]=="IBM"),c(4,5)]
colnames(littrades)<-c("Lit","timeMid")

trades<-full_join(littrades, darktrades, by = "timeMid")
qt<-full_join(trades, part.data, by = "timeMid")
qt<-qt[order(qt$timeMid),]
rownames(qt)<-c(1:nrow(qt))
qt<-qt[72:39560,c(2,1,3,4,5)]
qt<-na.locf(qt)


write.csv(qt, file = 'qt.csv', row.names = FALSE)

############################### Natural time (1-sec resolution) ###############################
array.IBMqt <- matrix(replicate(24902*5,0), nrow=24902, ncol = 5)
colnames(array.IBMqt)<-c("timeMid","Lit","Dark","bBid","bOfr")
array.IBMqt[,1]<-c(34200:59101)

for (i in 1:24901) {
  array.IBMqt[i+1,4]<-median(bBid.IBM[which(bBid.IBM[,1]>= array.IBMqt[i,1] & bBid.IBM[,1]<= array.IBMqt[i+1,1]),2])
  array.IBMqt[i+1,5]<-median(bOfr.IBM[which(bOfr.IBM[,1]>= array.IBMqt[i,1] & bOfr.IBM[,1]<= array.IBMqt[i+1,1]),2])  
  array.IBMqt[i+1,2]<-median(littrades[which(littrades[,2]>= array.IBMqt[i,1] & littrades[,2]<= array.IBMqt[i+1,1]),1])
  array.IBMqt[i+1,3]<-median(darktrades[which(darktrades[,2]>= array.IBMqt[i,1] & darktrades[,2]<= array.IBMqt[i+1,1]),1]) 
}
array.IBMqt<-array.IBMqt[-1,]
df.qtIBM<-na.locf(array.IBMqt)

write.csv(df.qtIBM, file = 'df.qtIBM.csv', row.names = FALSE)
