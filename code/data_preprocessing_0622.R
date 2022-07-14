#COMMENT: Preprocessing has to be done with R-version < 4!

# SETUP ####

#check for OS --> define home path (script independent of OS)
ifelse(Sys.info()['sysname']=='Linux',
       home_path<-'~',
       home_path<-'C:/Users/Nico')

#project path
project_path<-'/PowerFolders/project_visualsearch'

#data path
data_path<-'/PowerFolders/data_AFFIP/data' #preprocessed on LINUX machine

fun_rename<-function(x,variable_position,new_name){
  names(x)[variable_position]<-new_name
  return(x)}


# Load Packages ####
require(zoo) #na.approx
require(data.table) #fread uses parallelization and thus mucd faster than read.csv

# Load Data ####
datapath<-paste0(home_path,data_path) #preprocessed on LINUX machine

datapath<-"G:/BACKUP_Polzer_ETBattery/data_AFFIP/data"

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

#data.files<-data.files[c(grep('t2_gazedata.csv',data.files),grep('K_gazedata.csv',data.files))] #exclude other test points
#data.time<-data.time[c(grep('t2_timestamps.csv',data.time),grep('K_timestamps.csv',data.time))] #exclude other test points
#data.events<-data.events[c(grep('t2_event.csv',data.events),grep('K_event.csv',data.events))] #exclude other test points
# -> all three file lists (gazedata, time, events) should have the same length

#remove double entry of 094_t6
data.events<-data.events[-134]

# #read all gaze data into list class
# df.list.data<-lapply(data.files,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
# df.list.time<-lapply(data.time,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
# df.list.events<-lapply(data.events,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)

#read csv is very slow
test_list<-list(0)
for(i in 1:length(data.files)){
    test_list[[i]]<-fread(data.files[i])
    print(paste0('read: ',i))
}
###16 gb fails after 118 entries --> requires at least 32GB (June 2022)
### for loop is slower, but does not kill memory

df.list.data<-test_list
#df.list.data<-lapply(data.files,read.csv)
df.list.time<-lapply(data.time,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
df.list.events<-lapply(data.events,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)


## --> RELABEL + CONCATENATE data ####

#relabel data
name.labels<-c('eyepos.X_L','eyepos.Y_L','eyepos.Z_L','releyepos.X_L','releyepos.Y_L','eyepos.Z_L','gazepos2D.X_L','gazepos2D.Y_L','gazepos3D.X_L','gazepos3D.Y_L','gazepos3D.Z_L','pupildil_L','validcode_L','eyepos.X_R','eyepos.Y_R','eyepos.Z_R','releyepos.X_R','releyepos.Y_R','eyepos.Z_R','gazepos2D.X_R','gazepos2D.Y_R','gazepos3D.X_R','gazepos3D.Y_R','gazepos3D.Z_R','pupildil_R','validcode_R')
for(i in 1:length(df.list.data)){names(df.list.data[[i]])<-name.labels}
for(i in 1:length(df.list.time)){names(df.list.time[[i]])<-'timestamp'}
for(i in 1:length(df.list.events)){names(df.list.events[[i]])<-c('event','ev.ts')}

#add timestamp to gaze data - only possible for same length files
df.list<-mapply(data.frame,df.list.time,df.list.data,SIMPLIFY = F)


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
}


#add names to list
id.names<-substr(data.files,nchar(datapath)+2,nchar(datapath)+7) #08.07.19: now independent of relative to datapath
names(df)<-id.names


#remove lists: clear ram
rm(df.list.data, df.list.events, df.list.time)

save(df,file="F:/temp_data_AFFIP/all_data_merged_040722.Rdata")

#---------------------------------------------------------------------------------------#
## --> SELECT TASK SPECIFIC DATA ####

#create long format variable: ts (in seconds format)
ts<-lapply(df,function(x){x<-x$timestamp})
ts<-lapply(ts,function(x){abs(head(x,n=1)-x)/1000000})

# fun_rename<- function(x){
#   names(x)[29]<-c('ts')
#   return(x)}

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


## --> CREATE VARIABLES: phase, trial, ts.trial and block variable ####
  ###--> applied as function for every trial

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


#create a new ts and trial variable that considers first second of trial as postphase of prreceidng trial (ts_event_new, ts_trial_new)
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

data_block1<-lapply(data_block1,fun_modify_trials_ts)
data_block2<-lapply(data_block2,fun_modify_trials_ts)
data_block3<-lapply(data_block3,fun_modify_trials_ts)
data_block4<-lapply(data_block4,fun_modify_trials_ts)

#---------------------------------------------------------------------------------------------------#

# DATA PREPROCESSING ####

  rm(ts)

  ## -- control screen attention, retrieve gaze position + center deviation ####

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
data_block1<-lapply(data_block1,fun_screen_att)
data_block2<-lapply(data_block2,fun_screen_att)
data_block3<-lapply(data_block3,fun_screen_att)
data_block4<-lapply(data_block4,fun_screen_att)


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

data_block1<-lapply(data_block1,fun_screen_dist)
data_block2<-lapply(data_block2,fun_screen_dist)
data_block3<-lapply(data_block3,fun_screen_dist)
data_block4<-lapply(data_block4,fun_screen_dist)

  ## ----------------- GAZE PREPROCESSING  ---------------------- ####

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

data_block1<-lapply(data_block1,fun_gaze_preprocess)
data_block2<-lapply(data_block2,fun_gaze_preprocess)
data_block3<-lapply(data_block3,fun_gaze_preprocess)
data_block4<-lapply(data_block4,fun_gaze_preprocess)

#saccade identification based on velocity --> difficutl with noisy data
fun_saccade_ident<-function(x){

  #x<-fun_gaze_preprocess(data_block1[[100]])

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

data_block1<-lapply(data_block1,fun_saccade_ident)
data_block2<-lapply(data_block2,fun_saccade_ident)
data_block3<-lapply(data_block3,fun_saccade_ident)
data_block4<-lapply(data_block4,fun_saccade_ident)

#fixation identification

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


#testing #####

 ###--> returns too many saccades - either smoothing of gazepos before or also consider distance
    ###--> check literature

  #return distance variable
  #return gaze or fixation category
  #post-hoc validation


  ## ----------- PUPIL Preprocessing (Kret, 2018) -------------- ####
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
data_block1<-lapply(data_block1, func_pd_preprocess)
data_block2<-lapply(data_block2, func_pd_preprocess)
data_block3<-lapply(data_block3, func_pd_preprocess)
data_block4<-lapply(data_block4, func_pd_preprocess)


  ### - select relevant data ####
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



  ### - create target + target position variable #####
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


  #create ID variable - required before concatenating blocks
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



  #calculate baseline pupil size - correted pupil size
  fun_baseline<-function(x){

    split_by_trial<-split(x,as.factor(x$index_trial))
    baseline_data<-sapply(split_by_trial,function(x){mean(x$pd[x$ts_event>= 250 & x$ts_event<=400],na.rm=T)}) #select between event
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



  #TODO:
    #--> data to noisy to identify saccades per trial

    #--> identify fixation

    #--> create trialwise data frame
      #- with time_to_hit data --> based on hits
      #- with pupillary response

  ##--> develop analysis idea


  #--> DATA PLAUSIBILITY####
  require(gridExtra)
  require(ggplot2)

    #for testing: sleect subsample and do preprocessing
    subsample<-sample(1:length(data_block1), 10, replace=F)
    test<-lapply(data_block1[subsample],fun_gaze_preprocess)
    test<-lapply(test,fun_saccade_ident)
    test<-lapply(test,fun_fixation_ident)
    test<-lapply(test,func_pd_preprocess)
    test<-lapply(test,fun_select_data)
    test<-lapply(test,func_target_var)
    test<-lapply(test,fun_ident_hits)
    test<-mapply(fun_retrieve_id,x=test,id=names(test),SIMPLIFY=F)
    test<-lapply(test,fun_baseline)
    tmp1<-dplyr::bind_rows(test)
    test<-tmp1


  tmp1<-dplyr::bind_rows(data_block1)
  tmp2<-dplyr::bind_rows(data_block2)
  tmp3<-dplyr::bind_rows(data_block3)
  tmp4<-dplyr::bind_rows(data_block4)
  test<-rbind(tmp1,tmp2,tmp3,tmp4)
  rm(tmp1,tmp2,tmp3,tmp4)

  #create group variable
  group<-ifelse(grepl('_K',test$id),'TD','ASD')
  test<-data.frame(test,group)

  #create timepoint variable
  timepoint<-ifelse(grepl('_t2',test$id) | grepl('_T2',test$id),'T2',
                    ifelse(grepl('_t4',test$id) | grepl('_T4',test$id),'T4',
                           ifelse(grepl('_t6',test$id) | grepl('_T6',test$id),'T6','K')))

  test<-data.frame(test,timepoint)

  table(test$timepoint)
  table(test$hit)
  table(test$hit[test$trial_phase=='target'])[2]/sum(table(test$hit[test$trial_phase=='target']))
  #--> 15.6% hits during target presentation

  require(ggplot2)


  #hits over progression of a trial
  ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,fill=hit))+geom_bar(position = "fill")+
    labs(x='sample (1/300s)',y='proportion of all samples')

  #saccade peak also shows no signal
  ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,fill=saccade))+geom_bar(position = "fill")+
    labs(x='sample (1/300s)',y='proportion of all samples')

  #fixation over progression of a trial
  ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,fill=fixation))+geom_bar(position = "fill")+
    labs(x='sample (1/300s)',y='proportion of all samples')



  table(test$saccade_peak)

  #descriptive hit differences
  with(test[test$trial_phase=='target',],by(hit,group,function(x){table(x)[2]/sum(table(x))}))

  #gaze behavior
  ggplot(test[test$trial_phase=='target',],aes(x=gazepos.x,y=gazepos.y))+
    geom_hex(bins=30)+
    scale_fill_gradientn(colours=rev(rainbow(3)))+
    ylim(1,0)+xlim(0,1)+coord_fixed(ratio = 9/16)

  #pupil size progression - between groups

        hist(test$ts_event[test$trial_phase=='fixcross']) #presentation of fixcross
        hist(test$ts_event[test$trial_phase=='target']) #presentation of fixcross


  ggplot(test[test$ts_event<1200,],aes(x=ts_event,y=pd,group=as.factor(group),color=as.factor(group)))+
    geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')+
    geom_vline(xintercept=750)


  #presentation of fixcross versus target in duration of scene
  hist(test$ts_event_new[test$trial_phase=='fixcross']) #presentation of fixcross
  hist(test$ts_event_new[test$trial_phase=='target']) #presentation of fixcross


  #pupil size progression - between groups
  ggplot(test[test$ts_event_new<1100,],aes(x=ts_event_new,y=pd,group=as.factor(group),color=as.factor(group)))+
    geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')+
    geom_vline(xintercept=450)+geom_vline(xintercept=900)

  #gaze velocity - between groups
  ggplot(test[test$ts_event_new<1100,],aes(x=ts_event_new,y=gaze_speed,group=as.factor(group),color=as.factor(group)))+
    geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='gaze velocity (degrees/s)')+
    geom_vline(xintercept=450)+geom_vline(xintercept=900)

 #gaze acceleration --> does not make much sense
  ggplot(test[test$ts_event_new<1100 & test$trial_phase=='target',],aes(x=ts_event_new,y=gaze_accel,group=as.factor(group),color=as.factor(group)))+
    geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='gaze acceleration')+
    geom_vline(xintercept=450)+geom_vline(xintercept=900)


            ggplot(test[test$ts_event<1000,],aes(x=ts_event,y=rpd,group=as.factor(group),color=as.factor(group)))+
              geom_smooth(method='lm', formula = y ~ x + poly(x,7))+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')

            ggplot(test[test$ts_event<1000,],aes(x=ts_event,y=rpd,group=as.factor(timepoint),color=as.factor(timepoint)))+
              geom_smooth(method='lm', formula = y ~ x + poly(x,3))+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')

            ggplot(test[test$ts_event<1000,],aes(x=ts_event,y=rpd,group=as.factor(group),color=as.factor(group)))+
              geom_smooth(method='lm', formula = y ~ x + poly(x,3))+theme_bw()+labs(title='pupil response within trial',x='time (samples)',y='pupil size (mm)')



  #pupil size progression
  g1<-ggplot(test[test$ts_event<1000,],aes(x=ts_event,y=pd))+
  geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')



  #gaze position
      hist(test$gazepos_x)
      hist(test$gazepos_y)

  ggplot(test[test$ts_event<1200 & !is.na(test$target_position),],aes(x=ts_event_new,y=gazepos_x,group=as.factor(target_position),color=as.factor(target_position)))+
    geom_smooth()+theme_bw()+ylim(10,30)+labs(title='eye movement: x-axis',x='time (samples)',y='screen space (0-1)')

  ggplot(test[test$ts_event<1200 & !is.na(test$target_position),],aes(x=ts_event_new,y=gazepos_y,group=as.factor(target_position),color=as.factor(target_position)))+
    geom_smooth()+theme_bw()+ylim(10,30)+labs(title='eye movement: y-axis',x='time (samples)',y='screen space (0-1)')
    ##--> most target hits are on top half



#figure on preprocessed data

  #gaze behavior
  g1<-ggplot(test[test$trial_phase=='target',],aes(x=gazepos.x,y=gazepos.y))+
    geom_hex(bins=30)+scale_fill_gradientn(colours=rev(rainbow(3)))+
    ylim(1,0)+xlim(0,1)+coord_fixed(ratio = 9/16)+theme_bw()+labs(title='heatmap of gaze behavior',x='x-axis',y='y-axis')

  #hits over progression of a trial
  g2<-ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,fill=hit))+geom_bar(position = "fill")+
    labs(title='tagret hit within trial progression',x='sample (1/300s)',y='proportion of all samples')+theme_bw()

  #pupil size progression
  g3<-ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,y=pd))+
    geom_smooth()+theme_bw()+labs(title='pupil size within trial',x='time (1/300s)',y='pupil size (mm)')+
    geom_vline(xintercept=450)+geom_vline(xintercept=900)

  #gaze velocity
  g4<-ggplot(test[test$ts_event_new<1200,],aes(x=ts_event_new,y=gaze_speed))+
    geom_smooth()+theme_bw()+labs(title='gaze velocity within trial',x='time (1/300s)',y='gaze velocity (degrees/s)')+
    geom_vline(xintercept=450)+geom_vline(xintercept=900)

  #gaze position
  g5<-ggplot(test[test$ts_event<1200 & !is.na(test$target_position),],aes(x=ts_event_new,y=gazepos_x,group=as.factor(target_position),color=as.factor(target_position)))+
    geom_smooth()+theme_bw()+ylim(10,30)+labs(title='eye movement: x-axis',x='time (samples)',y='screen space (0-1)')

  g6<-ggplot(test[test$ts_event<1200 & !is.na(test$target_position),],aes(x=ts_event_new,y=gazepos_y,group=as.factor(target_position),color=as.factor(target_position)))+
    geom_smooth()+theme_bw()+ylim(10,30)+labs(title='eye movement: y-axis',x='time (samples)',y='screen space (0-1)')


  #create figure scene duration
  tiff(file=paste0(home_path,project_path,"/output/figure_preprocessed_data.tiff"), # create a file in tiff format in current working directory
       width=10, height=15, units="in", res=300, compression='lzw') #define size and resolution of the resulting figure

  grid.arrange(g1,g2,g3,g4,g5,g6,ncol=2,top='preprocessed data in subsample')

  dev.off()










  #gaze velocity
  ggplot(test[test$ts_event<1200,],aes(x=ts_event,y=gaze_speed))+
    geom_smooth()+theme_bw()+labs(title='gaze velocity within trial',x='time (samples)',y='gaze velocity (degrees/s)')



  ggplot(test[test$ts_event<1200 & test$saccade_peak==F,],aes(x=ts_event_new,y=gaze_speed,group=as.factor(group),color=as.factor(group)))+
    geom_smooth(method='lm', formula = y ~ x + poly(x,7))+theme_bw()+labs(title='gaze velocity within trial',x='time (samples)',y='gaze velocity (degrees/s)')


  ggplot(test[test$ts_event<1000 & test$saccade_peak==F,],aes(x=ts_event_new,y=gaze_speed,group=as.factor(group),color=as.factor(group)))+
    geom_smooth(method='lm', formula = y ~ x + poly(x,8))+theme_bw()+labs(title='gaze velocity within trial',x='time (samples)',y='gaze velocity (degrees/s)')



  #gaze acceleration
  ggplot(test[test$ts_event<1100 & test$saccade_peak==F,],aes(x=ts_event,y=gaze_accel,group=as.factor(group),color=as.factor(group)))+
    geom_smooth(method='lm', formula = y ~ x + poly(x,7))+theme_bw()+labs(title='gaze acceleration within trial',x='time (samples)',y='gaze velocity (degrees/s)')


  ggplot(test[test$ts_event<1100 & test$saccade_peak==F,],aes(x=ts_event,y=gazepos_x,group=as.factor(group),color=as.factor(group)))+
    geom_smooth(method='lm', formula = y ~ x + poly(x,7))+theme_bw()

  ggplot(test[test$ts_event<1100 & test$saccade_peak==F,],aes(x=ts_event,y=gazepos_y,group=as.factor(group),color=as.factor(group)))+
    geom_smooth(method='lm', formula = y ~ x + poly(x,7))+theme_bw()



  hist(test$ts_event[test$saccade_peak==T])

?na.approx

# SAVE PREPROCESSED DATA ####

object.size(df)

save(df,file=paste0(home_path,'/PowerFolders/data_AFFIP/data_preprocessed_290622.Rdata'))


