# HEADER ####

## Script purpose: PREPROCESS BOSCA DATA - loads data and merges events with eye-tracking data
##
##
## Author: Nico Bast
##
## Date Created: `r paste(Sys.Date())`
##
## Copyright (c) Nico Bast, `r paste(format(Sys.Date(), "%Y"))`
## Email: nico.bast@kgu.de
##
##  SYSTEM REQUIREMENTS: at least 64GB of RAM to read data of 314 sessions (13.03.2024)
##
##  line 1-170 is independent of the task and describes generel preprocessing of BOSCA data


#-----------LOAD AND MERGE---------------------------------------------------------------####
# SETUP ####

#check for OS --> define home path (script independent of OS)
ifelse(Sys.info()['sysname']=='Linux',
       home_path<-'~',
       home_path<-'C:/Users/Nico')

#project path
project_path<-'/PowerFolders/project_visualsearch'

#data path
data_path<-'/PowerFolders/data_AFFIP/data'

#user-defined functions
fun_rename<-function(x,variable_position,new_name){
  names(x)[variable_position]<-new_name
  return(x)}


# Load Packages ####
require(zoo) #na.approx
require(data.table) #fread uses parallelization and thus mucd faster than read.csv

#analysis, visualization
require(ggplot2)
require(gridExtra) #panel figures
require(pbapply) #lapply with progress bar

#--> loaded packages
sessionInfo()

# Load Data ####
start_time <- Sys.time()

datapath<-paste0(home_path,data_path) #preprocessed on LINUX machine

#datapath<-"G:/BACKUP_Polzer_ETBattery/data_AFFIP/data"

#read data from datapath and store in according objects
data.files<-list.files(path=datapath,full.names=T)
data.time<-data.files[grep('_timestamps',data.files)]
data.events<-data.files[grep('_event',data.files)]
data.files<-data.files[grep('_gazedata',data.files)]

#reduce datafile to relevant datasets
data.files<-data.files[-grep('RECOVER',data.files)] #exclude recover files
data.time<-data.time[-grep('RECOVER',data.time)] #exclude recover files
data.events<-data.events[-grep('RECOVER',data.events)] #exclude recover files
data.files<-data.files[-grep('test',data.files)] #exclude recover files
data.time<-data.time[-grep('test',data.time)] #exclude recover files
data.events<-data.events[-grep('test',data.events)] #exclude recover files

#remove double entry of 094_t6
#data.events<-data.events[-134]
data.events<-data.events[data.events!="C:/Users/Nico/PowerFolders/data_AFFIP/data/094_t6__event.csv"]


##remove participants that have not all data (events,gazedata,time)
id_names_time<-substr(data.time,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.time)-nchar("_timestamps.csv"))
id_names_files<-substr(data.files,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.files)-nchar("_gazedata.csv"))
id_names_events<-substr(data.events,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.events)-nchar("_event.csv"))

id_names_files[!(id_names_files %in% id_names_time)]
id_names_events[!(id_names_events %in% id_names_time)]
##--> empty

data.files<-data.files[data.files!="C:/Users/Nico/PowerFolders/data_AFFIP/data/066_fu2_gazedata.csv"]
# data.events<-data.events[data.events!="C:/Users/Nico/PowerFolders/data_AFFIP/data/066_fu2_event.csv"]

#sorting - that files names match (k_fu introduced different sorting in data.files versus data.events)
###--> sort according to participant id
order_data<-order(substr(data.files,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.files)-nchar("_gazedata.csv")))
order_events<-order(substr(data.events,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.events)-nchar("_event.csv")))
order_time<-order(substr(data.time,nchar("C:/Users/Nico/PowerFolders/data_AFFIP/data/")+1,nchar(data.time)-nchar("_timestamps.csv")))

data.files<-data.files[order_data]
data.events<-data.events[order_events]
data.time<-data.time[order_time]

#do files match? --> visual inspection
cbind(substr(data.files,44,57),substr(data.events,44,54),substr(data.time,44,54))

#READ
#read csv is very slow
df.list.data<-list(0)
for(i in 1:length(data.files)){
  df.list.data[[i]]<-fread(data.files[i])
  print(paste0('read: ',i))
}
###16 gb fails after 118 entries --> requires at least 32GB (June 2022)
###--> fine for 64gb of memory (March 2024 --> 314 data entries)
### for loop is slower, but does not kill memory

#df.list.data<-lapply(data.files,read.csv)
df.list.time<-pblapply(data.time,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
df.list.events<-pblapply(data.events,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
###--> may takes some time

end_time <- Sys.time()
end_time - start_time

## --> RELABEL + CONCATENATE data ####
start_time <- Sys.time()

#relabel data
name.labels<-c('eyepos.X_L','eyepos.Y_L','eyepos.Z_L','releyepos.X_L','releyepos.Y_L','eyepos.Z_L','gazepos2D.X_L','gazepos2D.Y_L','gazepos3D.X_L','gazepos3D.Y_L','gazepos3D.Z_L','pupildil_L','validcode_L','eyepos.X_R','eyepos.Y_R','eyepos.Z_R','releyepos.X_R','releyepos.Y_R','eyepos.Z_R','gazepos2D.X_R','gazepos2D.Y_R','gazepos3D.X_R','gazepos3D.Y_R','gazepos3D.Z_R','pupildil_R','validcode_R')
for(i in 1:length(df.list.data)){names(df.list.data[[i]])<-name.labels}
for(i in 1:length(df.list.time)){names(df.list.time[[i]])<-'timestamp'}
for(i in 1:length(df.list.events)){names(df.list.events[[i]])<-c('event','ev.ts')}

#add timestamp to gaze data - only possible for same length files
df.list<-pbmapply(data.frame,df.list.time,df.list.data,SIMPLIFY = F)

#Add event data to gaze data
##Function to map events to according timestamps:
eventfunc<-function(x,y,z,i){
  event<-which(x[,1]>=y[i,2])
  z[event]<-y[i,1]
  return(z)}

##apply this function to all list elements of df.list (for loop in for loop)
df<-list()
for(i in 1:length(df.list.data)){
  eventlog<-rep(NA,nrow(df.list.events[[i]]))
  for(j in 1:nrow(df.list.events[[i]])){eventlog<-eventfunc(df.list[[i]],df.list.events[[i]],eventlog,j)}
  eventlog<-as.factor(eventlog) #changes the values
  levels.eventlog<-levels(df.list.events[[i]]$event)[as.numeric(levels(eventlog))] #retrieve those levels that are in eventlog
  levels(eventlog)<-levels.eventlog #apply retrieved events
  df[[i]]<-cbind(eventlog,df.list[[i]])
  print(paste0('matched: ',i))
}

#add names to list
#id.names<-substr(data.files,nchar(datapath)+2,nchar(datapath)+7) #08.07.19: now independent of relative to datapath
id.names<-substr(data.files,nchar(datapath)+2,nchar(data.files)-nchar('_gazedata.csv')) #06.03.23: amended with FUs

names(df)<-id.names

## --> SAVE MERGED DATA (TEMP) ####

#remove lists: clear ram
rm(df.list.data, df.list.events, df.list.time)

#save(df,file="F:/temp_data_AFFIP/all_data_merged_040722.Rdata")
#save(df,file="C:/Users/nico/Desktop/BOSCA_joint_attention_all_data_merged_060323.Rdata")
#save(df,file="C:/Users/nico/Desktop/BOSCA_joint_attention_all_data_merged_170423.Rdata")
#save(df,file="C:/Users/nico/Desktop/BOSCA_all_data_merged_130324.Rdata")
#save(df,file="C:/Users/nico/Desktop/BOSCA_all_data_merged_150524.Rdata")
#save(df,file="C:/Users/nico/Desktop/BOSCA_all_data_merged_280824.Rdata")

#--> now found on NAS

end_time <- Sys.time()
end_time - start_time

# --> ##load("F:\temp_data_AFFIP\BOSCA_all_data_merged_280824.Rdata")
#-----------PREPROCESSING----------------------------------------------------------------####
## -- SELECT TASK SPECIFIC DATA ####

start_time <- Sys.time()

#create long format variable: ts (in seconds format)
ts<-lapply(df,function(x){x<-x$timestamp})
ts<-lapply(ts,function(x){abs(head(x,n=1)-x)/1000000})

#add ts and change name of the variable
df<-mapply(cbind,df,ts,SIMPLIFY = FALSE) #simplify needs to be in capital letters
df<-lapply(df,fun_rename,variable_position=29,new_name='ts')


#SELECT VISUAL SEARCH DATA
#each trial: 2 seconds fixationcross + 1.5 seconds trial presentation (in reality fixcross 2.33s - stimulus 1.6s)
#---> select blocks - visualsearch_x -- testblockB_1
#---> which data timestamp > visualsearch_1 & < testblockB_1

#FIND ONSET AND OFFSET of visual search blocks
onset_block1<-sapply(df,function(x){x$ts[which(x$eventlog=='visualsearch_1')[1]]})
offset_block1_v1<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('testblockB_ver07_1','testblockB_1'))[1]]})
offset_block1_v2<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('emotionexpblock_1'))[1]]})
offset_block1_v3<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('natorientmov_1'))[1]]})

onset_block2<-sapply(df,function(x){x$ts[which(x$eventlog=='visualsearch_2')[1]]})
offset_block2_v1<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('testblockB_ver07_2','testblockB_2'))[1]]})
offset_block2_v2<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('emotionexpblock_2'))[1]]})
offset_block2_v3<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('natorientmov_2'))[1]]})

onset_block3<-sapply(df,function(x){x$ts[which(x$eventlog=='visualsearch_3')[1]]})
offset_block3_v1<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('testblockB_ver07_3','testblockB_3'))[1]]})
offset_block3_v2<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('emotionexpblock_3'))[1]]})
offset_block3_v3<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('natorientmov_3'))[1]]})

onset_block4<-sapply(df,function(x){x$ts[which(x$eventlog=='visualsearch_4')[1]]})
offset_block4_v1<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('testblockB_ver07_4','testblockB_4'))[1]]})
offset_block4_v2<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('emotionexpblock_4'))[1]]})
offset_block4_v3<-sapply(df,function(x){x$ts[which(x$eventlog %in% c('natorientmov_4'))[1]]})

#select the correct offset variable (natorient, next block, or emotionexpression)
func_select_timestamp<-function(x,y,z,i){
  offsets<-c(x,y,z)
  greater<-which(offsets>i)
  min(offsets[greater])}

#find oset in each data block
offset_block1<-mapply(func_select_timestamp,x=offset_block1_v1,y=offset_block1_v2,z=offset_block1_v3,i=onset_block1)
offset_block2<-mapply(func_select_timestamp,x=offset_block2_v1,y=offset_block2_v2,z=offset_block2_v3,i=onset_block2)
offset_block3<-mapply(func_select_timestamp,x=offset_block3_v1,y=offset_block3_v2,z=offset_block3_v3,i=onset_block3)
offset_block4<-mapply(func_select_timestamp,x=offset_block4_v1,y=offset_block4_v2,z=offset_block4_v3,i=onset_block4)
###--> INFO: PRODUCES WARNING FOR TRIALS WITH ONLY MISSING DATA

#remove infinite values
offset_block1<-ifelse(is.infinite(offset_block1),NA,offset_block1)
offset_block2<-ifelse(is.infinite(offset_block2),NA,offset_block2)
offset_block3<-ifelse(is.infinite(offset_block3),NA,offset_block3)
offset_block4<-ifelse(is.infinite(offset_block4),NA,offset_block4)

#retrieve visual search data for each block
data_block1<-mapply(function(x,y,z){x[x$ts>=y & x$ts<z,]},x=df,y=onset_block1,z=offset_block1, SIMPLIFY=F)
data_block2<-mapply(function(x,y,z){x[x$ts>=y & x$ts<z,]},x=df,y=onset_block2,z=offset_block2, SIMPLIFY=F)
data_block3<-mapply(function(x,y,z){x[x$ts>=y & x$ts<z,]},x=df,y=onset_block3,z=offset_block3, SIMPLIFY=F)
data_block4<-mapply(function(x,y,z){x[x$ts>=y & x$ts<z,]},x=df,y=onset_block4,z=offset_block4, SIMPLIFY=F)

#select only data of the first 80 seconds
cutoff<-80
data_block1<-mapply(function(x,y){x[(x$ts-y)<cutoff,]},x=data_block1,y=onset_block1, SIMPLIFY=F)
data_block2<-mapply(function(x,y){x[(x$ts-y)<cutoff,]},x=data_block2,y=onset_block2, SIMPLIFY=F)
data_block3<-mapply(function(x,y){x[(x$ts-y)<cutoff,]},x=data_block3,y=onset_block3, SIMPLIFY=F)
data_block4<-mapply(function(x,y){x[(x$ts-y)<cutoff,]},x=data_block4,y=onset_block4, SIMPLIFY=F)

end_time <- Sys.time()
end_time - start_time


## -- CREATE VARIABLES: phase, trial, ts.trial and block variable ####
###--> applied as function for every trial

start_time <- Sys.time()


### create a phase variable (target, fixcross, intro, outro)
fun_phase<-function(x){ifelse(grepl('target',x[,'eventlog']),'target',
                              ifelse(grepl('colscreen_col:1_dur:0.2',x[,'eventlog']),'fixcross',
                                     ifelse(grepl('fixationcross_col:0_dur:0.2',x[,'eventlog']),'fixcross',
                                            ifelse(grepl('colscreen_col:1_dur:2',x[,'eventlog']),'outro','intro'))))}


#create phase variable data of a block
phase_block1<-sapply(data_block1,fun_phase)
phase_block2<-sapply(data_block2,fun_phase)
phase_block3<-sapply(data_block3,fun_phase)
phase_block4<-sapply(data_block4,fun_phase)

#change to factor
phase_block1<-sapply(phase_block1,as.factor)
phase_block2<-sapply(phase_block2,as.factor)
phase_block3<-sapply(phase_block3,as.factor)
phase_block4<-sapply(phase_block4,as.factor)

#add phase variable
data_block1<-mapply(data.frame,data_block1,phase_block1, SIMPLIFY=F) #add
data_block2<-mapply(data.frame,data_block2,phase_block2, SIMPLIFY=F) #add
data_block3<-mapply(data.frame,data_block3,phase_block3, SIMPLIFY=F) #add
data_block4<-mapply(data.frame,data_block4,phase_block4, SIMPLIFY=F) #add

rm(phase_block1,phase_block2,phase_block3,phase_block4)

data_block1<-lapply(data_block1,fun_rename,variable_position=30,new_name='trial_phase') #rename
data_block2<-lapply(data_block2,fun_rename,variable_position=30,new_name='trial_phase') #rename
data_block3<-lapply(data_block3,fun_rename,variable_position=30,new_name='trial_phase') #rename
data_block4<-lapply(data_block4,fun_rename,variable_position=30,new_name='trial_phase') #rename

#--> use trial phase to create a trial variable

#define function that uses event change to define a trial (fixcross + stimuluspresentation)
###--> numbers have been chosen based on initial raw data inspection
fun_ident_trial<-function(x){
  for(i in 1:length(x)){
    if(i==length(x)){break} #stop loop i last element - otherwise next command will cause problems
    x[i]<-ifelse(x[i]>630 & x[i]<890 & x[i+1]>450 & x[i+1]<500,x[i]+x[i+1],x[i]) #longer event followed by shorter event
    x[i]<-ifelse(x[i]>450 & x[i]<500,NA,x[i]) #remove those that have been added

  }
  x<-x[!is.na(x)]
  return(x)
}

#identify trials --> function that based on change to identify trials and add trial sequence variable
fun_define_trials<-function(block_data){

  #create index variable - indicates when event changes #
  event.change<-lapply(block_data,function(x){which(diff(as.numeric(x$trial_phase))!=0)}) #index event change per participant

  length.datasets<-sapply(block_data,nrow)
  event.change<-mapply(function(x,y){x<-c(0,x,y)},event.change,length.datasets) #add start and end event to index
  event.change<-lapply(event.change,diff) #create a difference value

  #apply function to identify trials (combine fixcross + stimulus sections, see function definition above)
  event.change<-lapply(event.change,fun_ident_trial)

  #create index for these new trials
  index.trial<-lapply(event.change,function(x){rep(seq_along(x),times=x)})
  #sequence over each new trial
  ts.event<-lapply(index.trial,function(x){as.numeric(do.call(c,by(x,x,seq_along, simplify=F)))}) #tts over each trial

  #add index and sequence data to list
  block_data<-mapply(function(x,y,z){data.frame(x,y,z)},block_data,index.trial,ts.event,SIMPLIFY = F)
  block_data<-lapply(block_data,fun_rename,variable_position=31,new_name='index_trial')
  block_data<-lapply(block_data,fun_rename,variable_position=32,new_name='ts_event')
  return(block_data)
}


##remove participant with messed up data
data_block3<-data_block3[names(data_block3)!='052_FU2']

#apply functions that add trial and trial sequence data
data_block1<-fun_define_trials(data_block1)
data_block2<-fun_define_trials(data_block2)
data_block3<-fun_define_trials(data_block3)
data_block4<-fun_define_trials(data_block4)

#add a block variable
fun_block<-function(x,block){
  k<-nrow(x)
  block_nr<-rep(block,k)
  x<-data.frame(x,block_nr)
  return(x)
}

#add a block variable
data_block1<-lapply(data_block1,fun_block,block=1)
data_block2<-lapply(data_block2,fun_block,block=2)
data_block3<-lapply(data_block3,fun_block,block=3)
data_block4<-lapply(data_block4,fun_block,block=4)


#rename index trial - and crate trial variable independent of block
fun_rename_trial<-function(x){
  k<-x[,'index_trial']-1 #correct trial number in blocks
  k<-ifelse(k==0|k>16,NA,k) #correct trial number in blocks
  k<-k+((x[,'block_nr']-1)*16) #add trial number independent of blocks
  x[,'trial']<-k
  return(x)
}

#rename index trial - and crate trial variable independent of block
data_block1<-lapply(data_block1,fun_rename_trial)
data_block2<-lapply(data_block2,fun_rename_trial)
data_block3<-lapply(data_block3,fun_rename_trial)
data_block4<-lapply(data_block4,fun_rename_trial)

#create a new ts and trial variable that considers first second of trial as postphase of the preceding trial (ts_event_new, ts_trial_new)
cut_trial_cutoff<-300
fun_modify_trials_ts<-function(x){

  #value required for FOR LOOP
  length_trials<-table(x$trial)
  k<-x$ts_event
  j<-x$trial

  for(i in 1:length(length_trials)){

    #select events for a trial i that are not NA
    events_of_trial<-k[!is.na(x$trial) & x$trial==i]

    #change trial variable accordingly
    new_trial_variable<-ifelse(events_of_trial<=cut_trial_cutoff & i!=1,
                               j[!is.na(x$trial) & x$trial==i]-1,
                               j[!is.na(x$trial) & x$trial==i])

    #if int he first second (j<300) adapt timestamp to preceding trial, else subtract cutoff
    events_of_trial<-ifelse(events_of_trial<=cut_trial_cutoff & i!=1,
                            events_of_trial+length_trials[i-1]-cut_trial_cutoff,
                            events_of_trial-cut_trial_cutoff)

    #set value smaller zero to NA
    events_of_trial[events_of_trial<0]<-NA

    #apply data for each trial
    k[which(!is.na(x$trial) & x$trial==i)]<-events_of_trial
    j[which(!is.na(x$trial) & x$trial==i)]<-new_trial_variable

  }

  x$ts_event_new<-k
  x$trial_new<-j
  return(x)
}

data_block1<-pblapply(data_block1,fun_modify_trials_ts)
data_block2<-pblapply(data_block2,fun_modify_trials_ts)
data_block3<-pblapply(data_block3,fun_modify_trials_ts)
data_block4<-pblapply(data_block4,fun_modify_trials_ts)

rm(df)

end_time <- Sys.time()
end_time - start_time

#---------------------------------------------------------------------------------------------------#

## -- control screen attention, retrieve gaze position + center deviation ####

rm(ts)

start_time <- Sys.time()

fun_screen_att<-function(x){

  attach(x)
  #exclude implausible values
  xl <- ifelse((gazepos2D.X_L<0|gazepos2D.X_L>1), NA, gazepos2D.X_L)
  xr <- ifelse((gazepos2D.X_R<0|gazepos2D.X_R>1), NA, gazepos2D.X_R)
  yl <- ifelse((gazepos2D.Y_L<0|gazepos2D.Y_L>1), NA, gazepos2D.Y_L)
  yr <- ifelse((gazepos2D.Y_R<0|gazepos2D.Y_R>1), NA, gazepos2D.Y_R)
  #take offset between left and right into account
  x.offset<-xl-xr
  x.offset<-na.approx(x.offset,rule=2)
  y.offset<-yl-yr
  y.offset<-na.approx(y.offset,rule=2)
  #mean gaze across both eyes
  xl <- ifelse(is.na(xl)==FALSE, xl, xr+x.offset)
  xr <- ifelse(is.na(xr)==FALSE, xr, xl-x.offset)
  yl <- ifelse(is.na(yl)==FALSE, yl, yr+y.offset)
  yr <- ifelse(is.na(yr)==FALSE, yr, yl-y.offset)
  gazepos.x<-(xl+xr)/2
  gazepos.y<-(yl+yr)/2
  #add the gaze position data to df

  #remove outside screen
  gazepos.x<-ifelse(gazepos.x>1 | gazepos.x<0,NA,gazepos.x)
  gazepos.y<-ifelse(gazepos.y>1 | gazepos.y<0,NA,gazepos.y)

  #estimate center deviation
  center_deviation<-sqrt((gazepos.x-0.5)^2 + (gazepos.y-0.5)^2)

  x[,'gazepos.x']<-gazepos.x
  x[,'gazepos.y']<-gazepos.y
  x[,'center_dev']<-center_deviation

  # #exclude data, for which gaze position or PD = NA (TO DO: check if there are PD values for which gaze position = NA)
  # df<-df[!is.na(df$gazepos.x) & !is.na(df$gazepos.y),]
  detach(x)

  return(x)
}

#control screen attention
data_block1<-pblapply(data_block1,fun_screen_att)
data_block2<-pblapply(data_block2,fun_screen_att)
data_block3<-pblapply(data_block3,fun_screen_att)
data_block4<-pblapply(data_block4,fun_screen_att)

end_time <- Sys.time()
end_time - start_time


## -- retrieve screen distance ####
start_time <- Sys.time()

fun_screen_dist<-function(x){

  dist_L<-x$eyepos.Z_L #in mm from tracker
  dist_R<-x$eyepos.Z_R #in mm from tracker

  #exclude implausible values (smaller 500mm and larger 800mm is outside track box)
  dist_L<-ifelse(dist_L > 800 | dist_L < 500, NA, dist_L)
  dist_R<-ifelse(dist_R > 800 | dist_R < 500, NA, dist_R)

  #take offset between left and right into account
  offset<-dist_L-dist_R
  offset<-na.approx(offset,rule=2)

  #mean gaze across both eyes
  dist_L <- ifelse(is.na(dist_L)==FALSE, dist_L, dist_R+offset)
  dist_R <- ifelse(is.na(dist_R)==FALSE, dist_R, dist_R-offset)

  screen_dist<-(dist_L+dist_R)/2
  x[,'screen_dist']<-screen_dist

  return(x)

}

data_block1<-pblapply(data_block1,fun_screen_dist)
data_block2<-pblapply(data_block2,fun_screen_dist)
data_block3<-pblapply(data_block3,fun_screen_dist)
data_block4<-pblapply(data_block4,fun_screen_dist)


## -- drop unecessary data + particiapnts without data --> subsequent preprocessing requires far less RAM####

fun_required_necessary_data<-function(x){

  #drop raw eye tracking data
  x<-x[,!(grepl('gazepos2D',names(x)) | grepl('gazepos3D',names(x)) | grepl('eyepos',names(x)))]
  return(x)

}

data_block1<-pblapply(data_block1,fun_required_necessary_data)
data_block2<-pblapply(data_block2,fun_required_necessary_data)
data_block3<-pblapply(data_block3,fun_required_necessary_data)
data_block4<-pblapply(data_block4,fun_required_necessary_data)

## remove long trials (implausible)

# fun_valid_trials<-function(x,sample_cutoff=1300){
#
#
#   #preprocess based on per trial level
#   x$trial_new<-ifelse(is.na(x$trial_new),0,x$trial_new) #consider NA as trial=0
#   split_by_trial<-split(x,as.factor(x$trial_new))
#
#   #valid trial legth
#   valid_trials<-which(sapply(split_by_trial,nrow)<sample_cutoff)
#   split_by_trials<-split_by_trial[valid_trials]
#
#   #put back together
#   unsplitting_factor<-as.factor(x$trial_new)
#   unsplit_by_trials<-unsplit(split_by_trial,f=unsplitting_factor)
#   x<-unsplit_by_trials
#   x$trial_new[x$trial_new==0]<-NA
#   return(x)
#
# }
#
# data_block1<-lapply(data_block1,fun_valid_trials)
# data_block2<-lapply(data_block2,fun_valid_trials)
# data_block3<-lapply(data_block3,fun_valid_trials)
# data_block4<-lapply(data_block4,fun_valid_trials)


##--> participants with long duration have no data - drop these

sample_cutoff<-50000

dropped_participants<-c(which(sapply(data_block1,nrow)>sample_cutoff),
                        which(sapply(data_block2,nrow)>sample_cutoff),
                        which(sapply(data_block3,nrow)>sample_cutoff),
                        which(sapply(data_block4,nrow)>sample_cutoff))

data_block1<-data_block1[which(sapply(data_block1,nrow)<sample_cutoff)]
data_block2<-data_block2[which(sapply(data_block2,nrow)<sample_cutoff)]
data_block3<-data_block3[which(sapply(data_block3,nrow)<sample_cutoff)]
data_block4<-data_block4[which(sapply(data_block4,nrow)<sample_cutoff)]

end_time <- Sys.time()
end_time - start_time

## ----------> GAZE PREPROCESSING (Nyström, 2010) ---------------------- ####

start_time <- Sys.time()
##--> Time difference of 57.74925 mins (20.07.2022, Desktop 32GB Ram)


# ###use gazepath module
# require(gazepath)
#
# subsample<-sample(1:length(data_block1), 10, replace=F)
# test<-lapply(data_block1[subsample],function(x){gazepath(data=x,
#   x1='gazepos.x',
#   y1='gazepos.y',
#   d1='screen_dist',
#   trial='trial',
#   height_px=1080,
#   height_mm=290,
#   width_px=1920,
#   width_mm=510,
#   res_x=1920,
#   res_y=1080,
#   samplerate=300,
#   method='Mould.all', #recommended for high sampling data (>250) in low duration trials (<2s)
#   posthoc=T,
#   min_dist=500)
# })
#
# sample_test<-test[[8]]['fixations']

#variables for gaze preprocessing
screen_width<-510 #mm on a 23 inch screen wiht 16:9 aspect ratio (Tobii TX 300 screen)
screen_height<-290 #mm on a 23 inch screen wiht 16:9 aspect ratio (Tobii TX 300 screen)
degrees_by_radian<-57.296 #fixed conversion facor
velocity_cutoff<-1000 #visual degress per second
acceleration_cutoff<-100000 #visual degress per second
initial_velocity_cutoff<-200
median_samples_trial<-1200

#--> Savitksy Golay filter of length 15 ~ 50ms --> see for coefficients: http://www.statistics4u.info/fundstat_eng/cc_savgol_coeff.html
#filter_sg15<-c(-78,-13,42,87,122,147,162,167,162,147,122,87,42,-13,-78)/1105
filter_sg21<-c(-171,-76,9,84,149,204,249,284,309,324,329,324,309,284,249,204,149,84,9,-76,-171)/3059

#required functions for gaze preprocessing:

#A. - blink identification function - within 75ms to 250ms interval
#--> INFO: identifies blinks and NAs 8 samples before and after it (~25ms)
fun_blink_cor <- function(signal,lower_threshold=23,upper_threshold=75,samples_before=8,samples_after=8) {
  #change NA to 999 for rle()-function
  findna <- ifelse(is.na(signal),999,signal)
  #find blinks:
  #output of rle(): how many times values (NA) are repeated
  repets <- rle(findna)
  #stretch to length of PD vector for indexing
  repets <- rep(repets[["lengths"]], times=repets[["lengths"]])
  #difference between two timestamps~3.33ms -> 75/3.333=22.5 -> wenn 23 Reihen PD=NA, dann blink gap
  #if more than 150ms (45 rows) of NA, missing data due to blink unlikely
  #dummy coding of variables (1=at least 23 consecutive repetitions, 0=less than 23 repetitions)
  repets <- ifelse(repets>=lower_threshold & repets<=upper_threshold, 1, 0)
  #exclude cases where other values than NA (999) are repeated >=23 times by changing dummy value to 0:
  repets[findna!=999 & repets==1] <- 0
  #gives out where changes from 0 to 1 (no NA at least 23xNA) or 1 to 0 (23x NA to no NA) appear
  changes <- c(diff(repets),0)
  #define start (interval before blink/missing data)
  changes.start<-which(changes==1) #where NA-sequence starts
  #gives out row numbers of NA (blink) and previous 8 frames
  start.seq<-unlist(lapply(changes.start, function(x) {seq(max(x-(samples_before-1),1), x)}))
  repets[start.seq]<-1
  #define end (interval after blink/missing data)
  changes.end<-which(changes==-1)+1 #where NA.sequence ends
  #gives out row numbers of NA (blink) and subsequent 8 frames
  end.seq<-unlist(lapply(changes.end, function(x) {seq(x, min(x+(samples_before-1),length(repets)))}))
  repets[end.seq]<-1
  #replace PD data in blink interval (start to end) with NA
  signal[repets==1]<-NA
  return(signal)
}

#B. - return velocity
fun_return_speed<-function(a,b,time){
  time<-unlist(time)
  gaze_diff_x<-diff(a)
  gaze_diff_y<-diff(b)
  gaze_diff<-sqrt((gaze_diff_x^2)+(gaze_diff_y^2))
  gaze_speed<-gaze_diff/diff(time)
  gaze_speed<-c(NA,gaze_speed)
  return(gaze_speed)
}

#C. - return acceleration
fun_return_accel<-function(x,time){
  time<-unlist(time)
  diff_speed<-diff(x)
  gaze_accel<-diff_speed/diff(time)
  gaze_accel<-c(NA,gaze_accel)
  return(gaze_accel)
}

#D. - function data driven velocity threshold to identify saccades --> see Nyström et al., 2010
fun_velocity_threshold<-function(x){

  cutoff<-initial_velocity_cutoff
  diff_cutoff<-cutoff
  k<-x

  while(diff_cutoff>1){

    k<-k[k<cutoff]
    mean_speed<-median(k,na.rm=T)
    sd_speed<-mean(k,na.rm=T)

    cutoff_new<-mean_speed+(3*sd_speed)
    if(is.na(cutoff_new)){
      cutoff<-NA
      break}

    diff_cutoff<-cutoff-cutoff_new
    if(cutoff_new>cutoff){cutoff_new<-cutoff}
    cutoff<-cutoff_new

  }

  # #testing
  # return(cutoff)

  saccade_peak<-ifelse(x>cutoff,T,F)
  return(saccade_peak)
  #also return cutoffs in a list

}

#idnetififes most often values in a sequence
fun_most_values <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  return(ux[tab == max(tab)])

}


#function gaze preprocessing (requires functions above)
#--> inspired by Nyström et al. 2010 - denoising with Savitsky-Golay filter and adaptive velocity threshold filter
fun_gaze_preprocess<-function(x){

  #testing
  #x<-data_block1[[84]]

  #preprocess based on per trial level
  x$trial_new<-ifelse(is.na(x$trial_new),0,x$trial_new) #consider NA as trial=0
  split_by_trial<-split(x,as.factor(x$trial_new))

  #split_by_trial<-split(x,as.factor(x$index_trial))
  timestamp<-sapply(split_by_trial,function(x){x<-x['ts']})
  screen_distance<-sapply(split_by_trial,function(x){x<-x['screen_dist']})

  #1. blink correction --> as noise
  gazepos_x<-sapply(split_by_trial,function(x){fun_blink_cor(x$gazepos.x)})
  gazepos_y<-sapply(split_by_trial,function(x){fun_blink_cor(x$gazepos.y)})

  #3. drop trials with less than 50% of data
  gazepos_x<-sapply(gazepos_x,function(x){if(sum(is.na(x))>0.5*median_samples_trial){x<-as.numeric(rep(NA,length(x)))}else{return(x)}})
  gazepos_y<-sapply(gazepos_y,function(x){if(sum(is.na(x))>0.5*median_samples_trial){x<-as.numeric(rep(NA,length(x)))}else{return(x)}})

  #2. convert relative gaze to degrees visual angle --> degrees from point of origin
  gazepos_x_deg<-mapply(function(x,y){x<-x*atan(screen_width/y)*degrees_by_radian},x=gazepos_x,y=screen_distance,SIMPLIFY = F)
  gazepos_y_deg<-mapply(function(x,y){x<-x*atan(screen_height/y)*degrees_by_radian},x=gazepos_y,y=screen_distance,SIMPLIFY = F)

  #put together
  unsplitting_factor<-as.factor(x$trial_new)
  unsplit_by_trials<-unsplit(split_by_trial,f=unsplitting_factor)
  gazepos_x<-unsplit(gazepos_x,f=unsplitting_factor)
  gazepos_y<-unsplit(gazepos_y,f=unsplitting_factor)
  gazepos_x_deg<-unsplit(gazepos_x_deg,f=unsplitting_factor)
  gazepos_y_deg<-unsplit(gazepos_y_deg,f=unsplitting_factor)

  x<-data.frame(unsplit_by_trials,gazepos_x,gazepos_y,gazepos_x_deg,gazepos_y_deg)
  x$trial_new[x$trial_new==0]<-NA
  return(x)

}

data_block1<-pblapply(data_block1,fun_gaze_preprocess)
data_block2<-pblapply(data_block2,fun_gaze_preprocess)
data_block3<-pblapply(data_block3,fun_gaze_preprocess)
data_block4<-pblapply(data_block4,fun_gaze_preprocess)


#saccade identification based on velocity --> difficult with noisy data
fun_saccade_ident<-function(x){

  #preprocess based on per trial level
  x$trial_new<-ifelse(is.na(x$trial_new),0,x$trial_new) #consider NA as trial=0
  split_by_trial<-split(x,as.factor(x$trial_new))
  timestamp<-sapply(split_by_trial,function(x){x<-x['ts']})
  gazepos_x_deg<-sapply(split_by_trial,function(x){x<-x['gazepos_x_deg']})
  gazepos_y_deg<-sapply(split_by_trial,function(x){x<-x['gazepos_y_deg']})


  # A. VELOCITY: FILTERING, DENOISING, PEAK IDENTIFICATION

  #3. return velocity and acceleration variable --> in degrees per second
  gaze_speed<-mapply(fun_return_speed,a=gazepos_x_deg,b=gazepos_y_deg,time=timestamp,SIMPLIFY = F)
  gaze_accel<-mapply(fun_return_accel,x=gaze_speed,time=timestamp,SIMPLIFY = F)

  #4. remove biologically implausible acceleration and velocity values
  gaze_speed<-sapply(gaze_speed,function(x){ifelse(abs(x)>velocity_cutoff,NA,x)})
  gaze_accel<-sapply(gaze_accel,function(x){ifelse(abs(x)>acceleration_cutoff,NA,x)})

  #5. Denoising by Savitzky-Golay filter (length 21)
  gaze_speed<-lapply(gaze_speed,function(x){
    ifelse(length(x)<length(filter_sg21),
           k<-x,
           k<-as.numeric(stats::filter(x,filter_sg21)))
    return(k)})

  #--> SOME FORM OF DENOISING HAS TO BE APPLIED
  #literature of signal denoise
  #use a Savitzky Golay filter with length 15 ~ 50ms --> has been proven to be superior, see below


  #filter/denoise benchmarking #
  # sg7<-c(-2,3,6,7,6,3,-2)/21 #Savitsky Golay filter of length 7
  # sg5<-c(-3,12,17,12,-3)/35 #Savitsky Golay filter of length 5
  # sg21<-c(-171,-76,9,84,149,204,249,284,309,324,329,324,309,284,249,204,149,84,9,-76,-171)/3059
  # sg15<-c(-78,-13,42,87,122,147,162,167,162,147,122,87,42,-13,-78)/1105
  # ##--> see for coefficients: http://www.statistics4u.info/fundstat_eng/cc_savgol_coeff.html
  #
  # x<-gaze_speed[[15]]
  # par(mfrow=c(3,2))
  # plot(x,main='data')
  # plot(as.numeric(filter(x,sg5)),main='S-G5 filter')
  # plot(as.numeric(filter(x,sg15)),main='S-G15 filter')
  # plot(as.numeric(filter(x,sg21)),main='S-G21 filter')
  # #plot((na.approx(x,maxgap = interpolate_cutoff)),main='linear interpol')
  # plot(pracma::savgol(na.approx(x),fl=21),main='S-G long')
  # plot(spline(x,n=length(x)),main='spline')
  # par(mfrow=c(1,1))
  # ###--> compare different denoise / filtering methods
  # ##--> Savitzky-Golay filter seems to be the winner - use a length of 21

  #6.data-driven velocity threshold and identify peaks (saccades) based on it
  velocity_peak<-lapply(gaze_speed,fun_velocity_threshold)

  # # #  # #testing
  # plot(gaze_speed[[10]],col=as.factor(unlist(velocity_peak[[10]])))
  # plot(unlist(gaze_speed),col=as.factor(unlist(velocity_peak)))

  #B.: DISPERSION

  # fun_dispersion_filter<-function(a,b){
  #
  #   gaze_diff_x<-diff(a)
  #   gaze_diff_y<-diff(b)
  #   gaze_diff<-sqrt((gaze_diff_x^2)+(gaze_diff_y^2))
  #   gaze_diff<-c(NA,gaze_diff)
  # }
  #
  # gaze_diff<-mapply(fun_dispersion_filter,a=gazepos_x,b=gazepos_y,SIMPLIFY=F)

  #C.: DURATION

  #modus in a window of ten samples
  velocity_continues<-frollapply(velocity_peak,n=10,fun_most_values,align='center')
  velocity_continues<-lapply(velocity_continues,as.logical)

  #D.: DEFINE SACCADE

  saccade<-mapply(function(x,y){ifelse(x & y,T,F)},x=velocity_peak,y=velocity_continues,SIMPLIFY=F)

  #return trial-wise list to data.frame
  unsplitting_factor<-as.factor(x$trial_new)
  unsplit_by_trials<-unsplit(split_by_trial,f=unsplitting_factor)
  gaze_speed<-unsplit(gaze_speed,f=unsplitting_factor)
  gaze_accel<-unsplit(gaze_accel,f=unsplitting_factor)
  velocity_peak<-unsplit(velocity_peak,f=unsplitting_factor)
  velocity_continues<-unsplit(velocity_continues,f=unsplitting_factor)
  saccade<-unsplit(saccade,f=unsplitting_factor)


  x<-data.frame(unsplit_by_trials,gaze_speed,gaze_accel,velocity_peak,velocity_continues,saccade)
  x$trial_new[x$trial_new==0]<-NA
  return(x)
}


#remove failing dataset
data_block1<-data_block1[names(data_block1)!='062_fu2']
data_block2<-data_block2[names(data_block2)!='062_fu2']
data_block3<-data_block3[names(data_block3)!='062_fu2']
data_block4<-data_block4[names(data_block4)!='062_fu2']

data_block1<-pblapply(data_block1,fun_saccade_ident)
data_block2<-pblapply(data_block2,fun_saccade_ident)
data_block3<-pblapply(data_block3,fun_saccade_ident)
data_block4<-pblapply(data_block4,fun_saccade_ident)

#fixation identification --> difficult with noisy data
fun_fixation_ident<-function(x,degree_fixation_cutoff=1,duration_fixation_cutoff=30){

  #testing
  # x<-data_block1[[100]]
  # x<-fun_gaze_preprocess(x)
  # x<-fun_saccade_ident(x)

  x$trial_new<-ifelse(is.na(x$trial_new),0,x$trial_new) #consider NA as trial=0
  split_by_trial<-split(x,as.factor(x$trial_new))
  gazepos_x_deg<-sapply(split_by_trial,function(x){x<-x['gazepos_x_deg']})
  gazepos_y_deg<-sapply(split_by_trial,function(x){x<-x['gazepos_y_deg']})


  #gaze difference from sampel to sample
  fun_gaze_diff<-function(a,b){

    gaze_diff_x<-diff(a)
    gaze_diff_y<-diff(b)
    gaze_diff<-sqrt((gaze_diff_x^2)+(gaze_diff_y^2))
    gaze_diff<-c(NA,gaze_diff)

  }

  gaze_diff<-mapply(fun_gaze_diff,a=gazepos_x_deg,b=gazepos_y_deg,SIMPLIFY=FALSE)

  #identify significant movement in subsequent or preceding samples

  fun_movement_ident<-function(x){

    no_movement_next_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(mean(y,na.rm=T)<degree_fixation_cutoff,T,F)},align='left')
    no_movement_last_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(mean(y,na.rm=T)<degree_fixation_cutoff,T,F)},align='right')
    #no_movement_next_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(all(x<degree_fixation_cutoff),T,F)},align='left')
    #no_movement_last_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(all(x<degree_fixation_cutoff),T,F)},align='right')
    no_movement<-ifelse(no_movement_next_samples | no_movement_last_samples,T,F)

  }

  no_movement<-sapply(gaze_diff,fun_movement_ident)

  #identify significant drifts in subsequent or preceding samples

  fun_gaze_diff_abs<-function(x){

    gaze_diff_abs<-diff(x)
    gaze_diff_abs<-c(NA,gaze_diff_abs)

  }

  gaze_diff_abs_x<-sapply(gazepos_x_deg,fun_gaze_diff_abs)
  gaze_diff_abs_y<-sapply(gazepos_y_deg,fun_gaze_diff_abs)

  fun_drift_ident<-function(x){

    no_drift_next_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(sum(y,na.rm=T)<degree_fixation_cutoff,T,F)},align='left')
    no_drift_last_samples<-frollapply(x,n=duration_fixation_cutoff,function(y){ifelse(sum(y,na.rm=T)<degree_fixation_cutoff,T,F)},align='right')
    no_drift<-ifelse(no_drift_next_samples | no_drift_last_samples,T,F)

  }

  no_drift_x<-sapply(gaze_diff_abs_x,fun_drift_ident)
  no_drift_y<-sapply(gaze_diff_abs_y,fun_drift_ident)


  #--> define a fixation sample if no movement or drift in the last or next 100ms ~ 30 samples
  fixation<-mapply(function(x,y,z){ifelse(x & y & z,T,F)},x=no_movement,y=no_drift_x,z=no_drift_y)

  #add to data
  unsplitting_factor<-as.factor(x$trial_new)
  unsplit_by_trials<-unsplit(split_by_trial,f=unsplitting_factor)
  fixation<-unsplit(fixation,f=unsplitting_factor)
  x<-data.frame(unsplit_by_trials,fixation)
  x$trial_new[x$trial_new==0]<-NA
  return(x)

}

data_block1<-pblapply(data_block1,fun_fixation_ident)
data_block2<-pblapply(data_block2,fun_fixation_ident)
data_block3<-pblapply(data_block3,fun_fixation_ident)
data_block4<-pblapply(data_block4,fun_fixation_ident)

end_time <- Sys.time()
end_time - start_time

## ----------> PUPIL Preprocessing (Kret, 2018) -------------- ####

start_time <- Sys.time()

func_pd_preprocess<-function(x){

  #define variables
  Left_Diameter<-x$pupildil_L
  Right_Diameter<-x$pupildil_R
  RemoteTime<-x$timestamp

  #constant for MAD caluclation
  constant<-3 ##--> if change speed is higher than constant * median change --> values are excluded
  #constant<-3 #default value

  # STEP 1 - exclude invalid data ####
  pl <- ifelse((Left_Diameter<2|Left_Diameter>8), NA, Left_Diameter)
  pr <- ifelse((Right_Diameter<2|Right_Diameter>8), NA, Right_Diameter)
  #table(is.na(pl))
  #table(is.na(pr))

  # STEP 2 - filtering ####
  ## A) normalized dilation speed, take into account time jumps with Remotetimestamps: ####
  #maximum change in pd compared to last and next pd measurement
  #Left
  pl.speed1<-diff(pl)/diff(RemoteTime) #compared to last
  pl.speed2<-diff(rev(pl))/diff(rev(RemoteTime)) #compared to next
  pl.speed1<-c(NA,pl.speed1)
  pl.speed2<-c(rev(pl.speed2),NA)
  pl.speed<-pmax(pl.speed1,pl.speed2,na.rm=T)
  rm(pl.speed1,pl.speed2)
  #Right
  pr.speed1<-diff(pr)/diff(RemoteTime)
  pr.speed2<-diff(rev(pr))/diff(rev(RemoteTime))
  pr.speed1<-c(NA,pr.speed1)
  pr.speed2<-c(rev(pr.speed2),NA)
  pr.speed<-pmax(pr.speed1,pr.speed2,na.rm=T)
  rm(pr.speed1,pr.speed2)
  #median absolute deviation -SPEED
  #constant<-3
  pl.speed.med<-median(pl.speed,na.rm=T)
  pl.mad<-median(abs(pl.speed-pl.speed.med),na.rm = T)
  pl.treshold.speed<-pl.speed.med+constant*pl.mad #treshold.speed units are mm/microsecond
  #plot(abs(pl.speed))+abline(h=pl.treshold.speed)
  pr.speed.med<-median(pr.speed,na.rm=T)
  pr.mad<-median(abs(pr.speed-pr.speed.med),na.rm = T)
  pr.treshold.speed<-pr.speed.med+constant*pr.mad #treshold.speed units are mm/microsecond
  #plot(abs(pr.speed))+abline(h=pr.treshold.speed)
  #correct pupil dilation for speed outliers
  pl<-ifelse(abs(pl.speed)>pl.treshold.speed,NA,pl)
  pr<-ifelse(abs(pr.speed)>pr.treshold.speed,NA,pr)

  ## B) delete data around blinks - not applied ####
  #gaps=missing data sections > 75ms; Leonie: also <=250ms, otherwise not likely to be a blink
  #to be excluded: samples within 50 ms of gaps -> +-25 (8 data points) oder 50?
  pl<-fun_blink_cor(pl)
  pr<-fun_blink_cor(pr)

  ## C) normalized dilation size - median absolute deviation -SIZE ####
  #applies a two pass approach
  #first pass: exclude deviation from trend line derived from all samples
  #second pass: exclude deviation from trend line derived from samples passing first pass
  #-_> reintroduction of sample that might have been falsely excluded due to outliers
  #estimate smooth size based on sampling rate
  smooth.length<-150 #measured in ms
  #take sampling rate into account (300 vs. 120):
  #smooth.size<-round(smooth.length/mean(diff(RemoteTime)/1000)) #timestamp resolution in microseconds
  smooth.size<-round(smooth.length/median(diff(RemoteTime),na.rm=T)) #timestamp resolution in milliseconds
  is.even<-function(x){x%%2==0}
  smooth.size<-ifelse(is.even(smooth.size)==T,smooth.size+1,smooth.size) #make sure to be odd value (see runmed)
  #Left
  pl.smooth<-na.approx(pl,na.rm=F,rule=2) #impute missing values with interpolation
  #pl.smooth<-runmed(pl.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
  if(sum(!is.na(pl.smooth))!=0){pl.smooth<-runmed(pl.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
  pl.mad<-median(abs(pl-pl.smooth),na.rm=T)
  #Right
  pr.smooth<-na.approx(pr,na.rm=F,rule=2) #impute missing values with interpolation
  #pr.smooth<-runmed(pr.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
  if(sum(!is.na(pr.smooth))!=0){pr.smooth<-runmed(pr.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
  pr.mad<-median(abs(pr-pr.smooth),na.rm=T)
  #correct pupil dilation for size outliers - FIRST pass
  pl.pass1<-ifelse((pl>pl.smooth+constant*pl.mad)|(pl<pl.smooth-constant*pl.mad),NA,pl)
  pr.pass1<-ifelse((pr>pr.smooth+constant*pr.mad)|(pr<pr.smooth-constant*pr.mad),NA,pr)
  #Left
  pl.smooth<-na.approx(pl.pass1,na.rm=F,rule=2) #impute missing values with interpolation
  #pl.smooth<-runmed(pl.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
  if(sum(!is.na(pl.smooth))!=0){pl.smooth<-runmed(pl.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
  pl.mad<-median(abs(pl-pl.smooth),na.rm=T)
  #Right
  pr.smooth<-na.approx(pr.pass1,na.rm=F,rule=2) #impute missing values with interpolation
  #pr.smooth<-runmed(pr.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
  if(sum(!is.na(pr.smooth))!=0){pr.smooth<-runmed(pr.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
  pr.mad<-median(abs(pr-pr.smooth),na.rm=T)
  #correct pupil dilation for size outliers - SECOND pass
  pl.pass2<-ifelse((pl>pl.smooth+constant*pl.mad)|(pl<pl.smooth-constant*pl.mad),NA,pl)
  pr.pass2<-ifelse((pr>pr.smooth+constant*pr.mad)|(pr<pr.smooth-constant*pr.mad),NA,pr)
  pl<-pl.pass2
  pr<-pr.pass2

  ## D) sparsity filter - not applied ####
  # STEP 3 - processing valid samples  ####
  #take offset between left and right into account
  pd.offset<-pl-pr
  pd.offset<-na.approx(pd.offset,rule=2)
  #mean pupil dilation across both eyes
  pl <- ifelse(is.na(pl)==FALSE, pl, pr+pd.offset)
  pr <- ifelse(is.na(pr)==FALSE, pr, pl-pd.offset)

  #interpolation of NA (for <=300ms)
  pl<-na.approx(pl, na.rm=F, maxgap=90, rule=2)
  pr<-na.approx(pr, na.rm=F, maxgap=90, rule=2)

  pd <- (pl+pr)/2
  # end of function --> return ####
  #detach(x)

  x[,'pd']<-pd
  return(x)
}

# pd.list<-lapply(df, func_pd_preprocess)
data_block1<-pblapply(data_block1, func_pd_preprocess)
data_block2<-pblapply(data_block2, func_pd_preprocess)
data_block3<-pblapply(data_block3, func_pd_preprocess)
data_block4<-pblapply(data_block4, func_pd_preprocess)

end_time <- Sys.time()
end_time - start_time

### - select relevant data ####

start_time <- Sys.time()

fun_select_data<-function(x){
  x<-x[,names(x) %in% c('eventlog','timestamp','ts','trial_phase','index_trial',
                        'ts_event','block_nr','trial','gazepos.x','gazepos.y',
                        'pd','center_deviation','gazepos_x','gazepos_y','gaze_speed','gaze_accel',
                        'velocity_peak','velocity_continues','saccade','fixation',
                        'ts_event_new','trial_new')]
  return(x)
}

data_block1<-lapply(data_block1,fun_select_data)
data_block2<-lapply(data_block2,fun_select_data)
data_block3<-lapply(data_block3,fun_select_data)
data_block4<-lapply(data_block4,fun_select_data)

end_time <- Sys.time()
end_time - start_time

### - create target + target position variable #####

start_time <- Sys.time()

func_target_var<-function(x){

  split_by_trial<-split(x,as.factor(x$index_trial))
  target_information<-sapply(split_by_trial,function(x){names(table(droplevels(x$eventlog)))[3]})
  target<-substr(target_information,8,8)
  target_position<-substr(target_information,10,nchar(target_information))

  length_trials<-sapply(split_by_trial,nrow)
  target<-rep(target,length_trials)
  target_position<-rep(target_position,length_trials)

  x[,'target']<-target
  x[,'target_position']<-target_position
  return(x)

}

#create target + target position variable
data_block1<-lapply(data_block1,func_target_var)
data_block2<-lapply(data_block2,func_target_var)
data_block3<-lapply(data_block3,func_target_var)
data_block4<-lapply(data_block4,func_target_var)


### - identify HITS ####
fun_ident_hits<-function(df){

  target_position<-df$target_position

  xstart<-as.numeric(rep(NA,length(target_position)))
  ystart<-as.numeric(rep(NA,length(target_position)))
  xend<-as.numeric(rep(NA,length(target_position)))
  yend<-as.numeric(rep(NA,length(target_position)))

  #identify target position
  #-->cope with different string length of target position
  nchar_14<-which(nchar(target_position)==14)
  xstart[nchar_14]<-as.numeric(substr(target_position[nchar_14],1,3))
  ystart[nchar_14]<-as.numeric(substr(target_position[nchar_14],5,6))
  xend[nchar_14]<-as.numeric(substr(target_position[nchar_14],8,10))
  yend[nchar_14]<-as.numeric(substr(target_position[nchar_14],12,14))

  nchar_15a<-which(nchar(target_position)==15 & grepl('_65_1150_',target_position))
  xstart[nchar_15a]<-as.numeric(substr(target_position[nchar_15a],1,3))
  ystart[nchar_15a]<-as.numeric(substr(target_position[nchar_15a],5,6))
  xend[nchar_15a]<-as.numeric(substr(target_position[nchar_15a],8,11))
  yend[nchar_15a]<-as.numeric(substr(target_position[nchar_15a],13,15))

  nchar_15b<-which(nchar(target_position)==15 & (grepl('_315_650_',target_position) | grepl('_565_650_',target_position)))
  xstart[nchar_15b]<-as.numeric(substr(target_position[nchar_15b],1,3))
  ystart[nchar_15b]<-as.numeric(substr(target_position[nchar_15b],5,7))
  xend[nchar_15b]<-as.numeric(substr(target_position[nchar_15b],9,11))
  yend[nchar_15b]<-as.numeric(substr(target_position[nchar_15b],13,15))

  nchar_16<-which(nchar(target_position)==16)
  xstart[nchar_16]<-as.numeric(substr(target_position[nchar_16],1,3))
  ystart[nchar_16]<-as.numeric(substr(target_position[nchar_16],5,7))
  xend[nchar_16]<-as.numeric(substr(target_position[nchar_16],9,12))
  yend[nchar_16]<-as.numeric(substr(target_position[nchar_16],14,16))

  nchar_17<-which(nchar(target_position)==17)
  xstart[nchar_17]<-as.numeric(substr(target_position[nchar_17],1,4))
  ystart[nchar_17]<-as.numeric(substr(target_position[nchar_17],6,8))
  xend[nchar_17]<-as.numeric(substr(target_position[nchar_17],10,13))
  yend[nchar_17]<-as.numeric(substr(target_position[nchar_17],15,17))

  # #convert relative gaze to pixels
  # gaze_x_pix<-df$gazepos.x*1920
  # gaze_y_pix<-df$gazepos.y*1080

  #convert relative gaze to pixels --> uses preprocessed gaze variable
  gaze_x_pix<-df$gazepos_x*1920
  gaze_y_pix<-df$gazepos_y*1080


  #define offset - that allows to identify hit when they are just outside the target
  tol_offset<-15 #15 pixels

  #identify hits - compare gaze position to target position
  hit<-rep(NA,nrow(df))
  hit<-ifelse(gaze_x_pix > (xstart-tol_offset) & gaze_x_pix < (xend+tol_offset) & gaze_y_pix > (ystart-tol_offset) & gaze_y_pix < (yend+tol_offset),T,F)

  df[,'hit']<-hit
  return(df)

}

data_block1<-lapply(data_block1,fun_ident_hits)
data_block2<-lapply(data_block2,fun_ident_hits)
data_block3<-lapply(data_block3,fun_ident_hits)
data_block4<-lapply(data_block4,fun_ident_hits)


### - create ID variable - required before concatenating blocks ####
fun_retrieve_id<-function(x,id){
  k<-nrow(x)
  id<-rep(id,k)
  x[,'id']<-id
  return(x)
}

data_block1<-mapply(fun_retrieve_id,x=data_block1,id=names(data_block1),SIMPLIFY=F)
data_block2<-mapply(fun_retrieve_id,x=data_block2,id=names(data_block2),SIMPLIFY=F)
data_block3<-mapply(fun_retrieve_id,x=data_block3,id=names(data_block3),SIMPLIFY=F)
data_block4<-mapply(fun_retrieve_id,x=data_block4,id=names(data_block4),SIMPLIFY=F)


### - calculate baseline pupil size - corrected pupil size ####
fun_baseline<-function(x){

  split_by_trial<-split(x,as.factor(x$index_trial))
  baseline_data<-sapply(split_by_trial,function(x){mean(x$pd[x$ts_event_new>= 300 & x$ts_event_new<=450],na.rm=T)}) #select between event
  rpd<-unlist(mapply(function(x,y){x$pd-y},x=split_by_trial,y=baseline_data)) #correct for baseline by subtraction
  baseline_pd<-rep(baseline_data,sapply(split_by_trial,nrow)) #calculate baseline pd

  x[,'rpd']<-rpd
  x[,'baseline_pd']<-baseline_pd
  return(x)

}

data_block1<-lapply(data_block1,fun_baseline)
data_block2<-lapply(data_block2,fun_baseline)
data_block3<-lapply(data_block3,fun_baseline)
data_block4<-lapply(data_block4,fun_baseline)

end_time <- Sys.time()
end_time - start_time

### - calculated fixate fixation cross before stimulus onset
stimulus_onset<-450

fun_fixate_fixcross<-function(x){

  split_by_trial<-split(x,as.factor(x$index_trial))

  mean_gazepos_x<-sapply(split_by_trial,function(x){
    mean(x$gazepos_x[x$ts_event_new<stimulus_onset],na.rm=T)})

  mean_gazepos_y<-sapply(split_by_trial,function(x){
    mean(x$gazepos_y[x$ts_event_new<stimulus_onset],na.rm=T)})

  any_fixation<-sapply(split_by_trial,function(x){
    any(x$fixation[x$ts_event_new<stimulus_onset],na.rm=T)})

  fixated_fixcross<-mean_gazepos_x < 0.6 & mean_gazepos_x > 0.4 & mean_gazepos_y < 0.6 & mean_gazepos_y > 0.4 & any_fixation

  fixated_fixcross<-rep(fixated_fixcross,sapply(split_by_trial,nrow))
  x[,'fixated_fixcross']<-fixated_fixcross
  return(x)
}

data_block1<-lapply(data_block1,fun_fixate_fixcross)
data_block2<-lapply(data_block2,fun_fixate_fixcross)
data_block3<-lapply(data_block3,fun_fixate_fixcross)
data_block4<-lapply(data_block4,fun_fixate_fixcross)

###----------> concatenate to single df #####

tmp1<-dplyr::bind_rows(data_block1)
tmp2<-dplyr::bind_rows(data_block2)
tmp3<-dplyr::bind_rows(data_block3)
tmp4<-dplyr::bind_rows(data_block4)
df<-rbind(tmp1,tmp2,tmp3,tmp4)
rm(tmp1,tmp2,tmp3,tmp4)


# #for testing: sleect subsample and do preprocessing
# subsample<-sample(1:length(data_block1), 10, replace=F)
# test<-lapply(data_block1[subsample],fun_gaze_preprocess)
# test<-lapply(test,fun_saccade_ident)
# test<-lapply(test,fun_fixation_ident)
# test<-lapply(test,func_pd_preprocess)
# test<-lapply(test,fun_select_data)
# test<-lapply(test,func_target_var)
# test<-lapply(test,fun_ident_hits)
# test<-mapply(fun_retrieve_id,x=test,id=names(test),SIMPLIFY=F)
# test<-lapply(test,fun_baseline)
# tmp1<-dplyr::bind_rows(test)
# test<-tmp1

# --------------- REMOVE INVALID DATA ---------------- ####

## remove NA in id ####
df<-df[!is.na(df$id),]

## remove FU4 data ####
df<-df[!grepl('fu4',df$id),]

## trimming of implausible data ####

#exclude trial timestamp larger than 1050
hist(df$ts)
df<-df[df$ts_event_new<1050,]
hist(df$ts_event) #remove ts_events > 1050
hist(df$ts_event_new)


# -------------- CREATE PIC; GROUP, and TIMEPOINT VARIABLE ----------------- ####

## create group variable ####
df$group<-ifelse(grepl('_K',df$id)|grepl('_k',df$id),'TD','ASD')

## create ID variable ####

#create individual id variable
df$pic<-substr(df$id,1,3)

## create timepoint variable ####
unique(substr(df$id,4,7))

table(is.na(df$id))

df$timepoint<-ifelse(grepl('_t2',df$id) | grepl('_T2',df$id),'T2',
                     ifelse(grepl('_t4',df$id) | grepl('_T4',df$id),'T4',
                            ifelse(grepl('_t6',df$id) | grepl('_T6',df$id),'T6',
                                   ifelse(grepl('_fu2',df$id) | grepl('_FU2',df$id),'FU2',
                                          ifelse(grepl('_fu3',df$id) | grepl('_FU3',df$id),'FU3',
                                                 ifelse(grepl('K_fu',df$id) | grepl('K_FU',df$id) | grepl('k_FU',df$id) | grepl('k_fu',df$id),'K_FU','K'))))))

with(df,table(group,timepoint))




# --------- DATA AGGREGATION----------------------------------################
### --> create ID AGGREGATED DATA set (df_agg) ####

#only trial where participants fixated the fixation cross
#df_agg<-df[df$fixated_fixcross,] #df is already selected for participants that are in all.match
df_agg<-df

split_by_trial<-split(df_agg,df_agg$id)
#hist(sapply(split_by_trial,nrow)) #only 4000 trials with data

#time to target
#time_to_target<-pbsapply(split_by_trial,function(x,stimulus_onset=450,frequency=300){
time_to_target<-pbsapply(split_by_trial,function(x,stimulus_onset=600,frequency=300){

  relevant_data<-x$ts_event_new[x$hit & x$fixation & x$ts_event_new>stimulus_onset]

  ifelse(all(is.na(relevant_data)),
         NA,
         (min(relevant_data,na.rm=T)-stimulus_onset)/frequency)

})

hist(time_to_target,50)
table(is.na(time_to_target))

#pupillary response
pupillary_response<-pbsapply(split_by_trial,function(x){

  before_stimulus_onset<-mean(x$pd[x$ts_event_new>300 & x$ts_event_new<450],na.rm=T)
  one_second_after_stimulus_onset<-mean(x$pd[x$ts_event_new>675 & x$ts_event_new<825],na.rm=T)
  one_second_after_stimulus_onset-before_stimulus_onset


})

hist(pupillary_response,50)

#hit
hit_duration<-pbsapply(split_by_trial,function(x){table(x$hit)[2]*0.00333})
missed_target<-ifelse(hit_duration<0.1,T,F)
hit_duration<-ifelse(hit_duration<0.1,NA,hit_duration)

#gaze speed
gaze_speed<-sapply(split_by_trial,function(x){mean(x$gaze_speed,na.rm=T)})
hist(gaze_speed)

#saccade_duration
saccade_duration<-sapply(split_by_trial,function(x){table(x$saccade)[2]*0.00333})
fixation_duration<-sapply(split_by_trial,function(x){table(x$fixation)[2]*0.00333})

#aggregate factor data (processing with R3.6 does not copy level labels)
id<-as.factor(with(df_agg,by(id,id,unique)))
pic<-as.factor(with(df_agg,by(pic,id,head,n=1)))
group<-as.factor(with(df_agg,by(group,id,head,n=1)))
timepoint<-as.factor(with(df_agg,by(timepoint,id,head,n=1)))

#aggregated mean numeric data
baseline_pd<-as.numeric(with(df_agg,by(baseline_pd,id,mean,na.rm=T)))

df_agg<-data.frame(id,pic,group,timepoint,
                   pupillary_response,time_to_target,baseline_pd,hit_duration,missed_target,saccade_duration,fixation_duration)

### --> create TRIAL AGGREGATED DATA set (df_trial) #####

#only trial where participants fixated the fixation cross
#df_trial<-df[df$fixated_fixcross,]
df_trial<-df

split_by_trial<-split(df_trial,interaction(df_trial$id,df_trial$trial))

#time to target
time_to_target<-pbsapply(split_by_trial,function(x,stimulus_onset=600,frequency=300){

  relevant_data<-x$ts_event_new[x$hit & x$fixation & x$ts_event_new>stimulus_onset]

  ifelse(all(is.na(relevant_data)),
         NA,
         (min(relevant_data,na.rm=T)-stimulus_onset)/frequency)

})

hist(time_to_target,50)
table(is.na(time_to_target))

#pupillary response
pupillary_response<-pbsapply(split_by_trial,function(x){

  before_stimulus_onset<-mean(x$pd[x$ts_event_new>450 & x$ts_event_new<600],na.rm=T)
  one_second_after_stimulus_onset<-mean(x$pd[x$ts_event_new>825 & x$ts_event_new<975],na.rm=T)
  one_second_after_stimulus_onset-before_stimulus_onset


})

#hit
hit_duration<-pbsapply(split_by_trial,function(x){table(x$hit)[2]*0.00333})
missed_target<-ifelse(hit_duration<0.1,T,F)
hit_duration<-ifelse(hit_duration<0.1,NA,hit_duration)

#gaze speed
gaze_speed<-sapply(split_by_trial,function(x){mean(x$gaze_speed,na.rm=T)})
hist(gaze_speed)

#saccade_duration
saccade_duration<-pbsapply(split_by_trial,function(x){table(x$saccade)[2]*0.00333})
fixation_duration<-pbsapply(split_by_trial,function(x){table(x$fixation)[2]*0.00333})

#aggregate factor data (processing with R3.6 does not copy level labels)
id<-as.factor(with(df_trial,by(id,interaction(id,trial),unique)))
pic<-as.factor(with(df_trial,by(pic,interaction(id,trial),head,n=1)))
group<-as.factor(with(df_trial,by(group,interaction(id,trial),head,n=1)))
timepoint<-as.factor(with(df_trial,by(timepoint,interaction(id,trial),head,n=1)))
block_nr<-as.factor(with(df_trial,by(block_nr,interaction(id,trial),head,n=1)))
trial<-as.factor(with(df_trial,by(trial,interaction(id,trial),head,n=1)))
target<-as.factor(with(df_trial,by(target,interaction(id,trial),head,n=1)))
target_position<-as.factor(with(df_trial,by(target_position,interaction(id,trial),head,n=1)))

#aggregated mean numeric data
baseline_pd<-as.numeric(with(df_trial,by(baseline_pd,interaction(id,trial),mean,na.rm=T)))

df_trial<-data.frame(id,pic,group,timepoint,block_nr,trial,target,target_position,
                     pupillary_response,time_to_target,baseline_pd,hit_duration,missed_target,saccade_duration,fixation_duration)

# ---> SAVE PREPROCESSED DATA ####

#save(df,df_trial,df_agg,file=paste0(home_path,project_path,"/data/all_data_preprocessed_160524.Rdata"))
save(df,df_trial,df_agg,file=paste0(home_path,project_path,"/data/all_data_preprocessed_280824.Rdata"))

test<-df[grepl('target',df$eventlog),]

ggplot(df,aes(ts_event_new,fill=trial_phase))+geom_histogram()+theme_bw()

