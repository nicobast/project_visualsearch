# HEADER ####

## Script purpose: DATA analysis visual search task - BOSCA battery - eye-tracking data
##
## Author: Nico Bast
##
## Date Created: `r paste(Sys.Date())`
##
## Copyright (c) Nico Bast, `r paste(format(Sys.Date(), "%Y"))`
## Email: nico.bast@kgu.de
##

# SETUP (load data anad packages) ####

load("C:/Users/Nico/PowerFolders/project_visualsearch/data/all_data_preprocessed_160524.Rdata")

# define required packages
require(ggplot2)
require(gridExtra) #panel figures
require(pbapply) #lapply with progress bar
require(lme4) #linear mixed models
require(emmeans) #linear mixed models
require(lmerTest) #linear mixed models
require(ggpubr) #significance bars in figures


# DATA PLAUSIBILITY (in preprocessed per-sample data) ####

#trials
hist(df$trial_new) #new as this uses the new timestamp as reference, where trial starts with last ISI second
hist(df$trial) #--> this is more clear - should be used in analysis

#individual data sets
length(unique(df$id)) #data of 313 measurement timepoints May 2024
length(unique(df$pic)) #data of 147 participants (102 after matching)
by(df$id,df$timepoint,function(x){length(unique(x))})
# K = 76, K_FU = 26, t2 = 67, t4 = 55, t6 = 51, FU2 = 23, FU3 = 12 (May 2024)

#missing data
table(is.na(df$rpd))[2]/sum(table(is.na(df$rpd))) #--> 41.1 missing data in pupil data
table(is.na(df$gazepos_x))[2]/sum(table(is.na(df$gazepos_x))) #--> 52.4% missing data in gaze x
table(is.na(df$gazepos_y))[2]/sum(table(is.na(df$gazepos_y))) #--> 55.8% missing data in gaze y

table(df$saccade) #roughly 10% saccade data is plausible
table(df$fixation) #roughly 90% fixation data is plausible

#data distribution --> exclude implausible
par(mfrow = c(3, 2))
hist(df$pd,30,main='pupil size (mm)')
#hist(df$baseline_pd,30,main='baseline pupil size (mm)') #pupil size in first 500ms of a trial
hist(df$rpd[df$rpd<2 & df$rpd>-2],30,main='pupillary response') #--> implausible outlier #pupil response (baseline pd - pupil size)
hist(df$gaze_speed,30,main='gaze velocity (degrees/s)')
hist(df$gaze_accel,30,main='gaze acceleration (degrees/s²)')
hist(df$gazepos_y,30,main='gaze location on x-axis (0-1)')
hist(df$gazepos_x,30,main='gaze location on y-axis (0-1)')
par(mfrow = c(1, 1))

#VALID TRIALS by participants
hist(as.numeric(with(df[!is.na(df$rpd),],
                     by(trial_new,id,function(x){length(unique(x))}))),xlab='trials',main='trials with PD data by participant')

hist(as.numeric(with(df[!is.na(df$rpd) & df$fixated_fixcross,],
                     by(trial_new,id,function(x){length(unique(x))}))),xlab='trials',main='trials with PD data by participant')

#HITS
table(df$hit)
table(df$hit[df$trial_phase=='target'])[2]/sum(table(df$hit[df$trial_phase=='target']))
#--> 18.4% hits during target presentation

#descriptive hit differences
with(df[df$trial_phase=='target',],by(hit,group,function(x){table(x)[2]/sum(table(x))}))
#--> total hit ratio: ASD = 17.4, TD = 20.2

#fixate fixation cross
with(df,by(fixated_fixcross,group,function(x){table(x)[2]/sum(table(x))}))
#--> total fixated fixation cross ratio: ASD = 25.6, TD = 35.4

table(df$target)
table(df$target_position)

# DATA VISUALIZATION (in preprocessed per-sample data) ####

#hits over progression of a trial
ggplot(df,aes(x=ts_event_new,fill=hit))+geom_bar(position = "fill")+
  labs(x='sample (1/300s)',y='proportion of all samples')

#saccade peak also shows no signal
ggplot(df,aes(x=ts_event_new,fill=saccade))+geom_bar(position = "fill")+
  labs(x='sample (1/300s)',y='proportion of all samples')

#fixation over progression of a trial
ggplot(df,aes(x=ts_event_new,fill=fixation))+geom_bar(position = "fill")+
  labs(x='sample (1/300s)',y='proportion of all samples')


#gaze behavior
ggplot(df[df$trial_phase=='target',],aes(x=gazepos.x,y=gazepos.y))+
  geom_hex(bins=30)+
  scale_fill_gradientn(colours=rev(rainbow(3)))+
  ylim(1,0)+xlim(0,1)+coord_fixed(ratio = 9/16)

#pupil size progression - between groups

hist(df$ts_event[df$trial_phase=='fixcross']) #presentation of fixcross --> between 0-750 = 2.5 seconds
hist(df$ts_event[df$trial_phase=='target']) #presentation of fixcross --> between 750-1200 = 1.5 seconds

ggplot(df[sample(nrow(df),nrow(df)/10),],aes(x=ts_event_new,y=pd,group=as.factor(group),color=as.factor(group)))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,8))+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')+
  geom_vline(xintercept=c(450,900))
##--> use figures as in pupillary response paper


#presentation of fixcross versus target in duration of scene
hist(df$ts_event_new[df$trial_phase=='fixcross']) #presentation of fixcross
hist(df$ts_event_new[df$trial_phase=='target']) #presentation of fixcross

#pupil size progression - between groups
ggplot(df,aes(x=ts_event_new,y=pd,group=as.factor(group),color=as.factor(group)))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,6))+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')+
  geom_vline(xintercept=450)+geom_vline(xintercept=900)

ggplot(df,aes(x=ts_event_new,y=rpd,group=as.factor(group),color=as.factor(group)))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,6))+theme_bw()+labs(title='pupil size within trial',x='time (samples)',y='pupil size (mm)')+
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


## -->  figure on preprocessed per-sample data ####

#gaze behavior
g1<-ggplot(df[df$trial_phase=='target',],aes(x=gazepos.x,y=gazepos.y))+
  geom_hex(bins=30)+scale_fill_gradientn(colours=rev(rainbow(3)))+
  ylim(1,0)+xlim(0,1)+coord_fixed(ratio = 9/16)+theme_bw()+labs(title='heatmap of gaze behavior',x='x-axis',y='y-axis')

#hits over progression of a trial
g2<-ggplot(df,aes(x=ts_event_new,fill=hit))+geom_bar(position = "fill")+
  labs(title='tagret hit within trial progression',x='sample (1/300s)',y='proportion of all samples')+theme_bw()

#pupil size progression
g3<-ggplot(df,aes(x=ts_event_new,y=pd))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,6))+theme_bw()+labs(title='pupil size within trial',x='time (1/300s)',y='pupil size (mm)')+
  geom_vline(xintercept=450)+geom_vline(xintercept=900)

#gaze velocity
g4<-ggplot(df,aes(x=ts_event_new,y=gaze_speed))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,6))+theme_bw()+labs(title='gaze velocity within trial',x='time (1/300s)',y='gaze velocity (degrees/s)')+
  geom_vline(xintercept=450)+geom_vline(xintercept=900)

#gaze position
g5<-ggplot(df[!is.na(df$target_position),],aes(x=ts_event_new,y=gazepos_x,group=as.factor(target_position),color=as.factor(target_position)))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,5))+theme_bw()+labs(title='eye movement: x-axis',x='time (samples)',y='screen space (0-1)')

g6<-ggplot(df[!is.na(df$target_position),],aes(x=ts_event_new,y=gazepos_y,group=as.factor(target_position),color=as.factor(target_position)))+
  geom_smooth(method='lm', formula = y ~ x + poly(x,5))+theme_bw()+labs(title='eye movement: y-axis',x='time (samples)',y='screen space (0-1)')

#create figure scene duration
tiff(file=paste0(home_path,project_path,"/output/figure_preprocessed_data.tiff"), # create a file in tiff format in current working directory
     width=12, height=15, units="in", res=300, compression='lzw') #define size and resolution of the resulting figure

grid.arrange(g1,g2,g3,g4,g5,g6,ncol=2,top='preprocessed data')

dev.off()


#-----------------------------------------------------------------------------####
##- DATA ANALYSIS - sample descriptives - between timepoints ####

n_sample<-with(df_match_timepoint,table(timepoint))

gender_table<-with(df_match_timepoint,sapply(by(Geschlecht_Index,timepoint,table),c))  #-->more female in TD
gender<-paste(gender_table[1,],gender_table[2,],sep='/')

age_m<-with(df_match_timepoint,as.numeric(by(age_months,timepoint,function(x){round(mean(x),2)})))
test_age_m<-with(df_match_timepoint,as.numeric(by(test_age,timepoint,function(x){round(mean(x),2)})))
iq_m<-with(df_match_timepoint,as.numeric(by(IQ,timepoint,function(x){round(mean(x),2)})))
srs_m<-with(df_match_timepoint,as.numeric(by(srs_sum,timepoint,function(x){round(mean(x,na.rm=T),2)})))
rbsr_m<-with(df_match_timepoint,as.numeric(by(RBSR_ges,timepoint,function(x){round(mean(x,na.rm=T),2)})))
cbcl_ges_m<-with(df_match_timepoint,as.numeric(by(CBCL_T_GES,timepoint,function(x){round(mean(x,na.rm=T),2)})))
cbcl_int_m<-with(df_match_timepoint,as.numeric(by(CBCL_T_INT,timepoint,function(x){round(mean(x,na.rm=T),2)})))
cbcl_ext_m<-with(df_match_timepoint,as.numeric(by(CBCL_T_EXT,timepoint,function(x){round(mean(x,na.rm=T),2)})))
BOSCC_m<-with(df_match_timepoint,as.numeric(by(BOSCC_Average_Total,timepoint,function(x){round(mean(x),2)})))
adi_m<-with(df_match_timepoint,as.numeric(by(adi.sum.algo,timepoint,function(x){round(mean(x,na.rm=T),2)})))
adi_toddler_m<-with(df_match_timepoint,as.numeric(by(adi.sum.toddler,timepoint,function(x){round(mean(x,na.rm=T),2)})))

age_sd<-with(df_match_timepoint,as.numeric(by(age_months,timepoint,function(x){round(sd(x),2)})))
test_age_sd<-with(df_match_timepoint,as.numeric(by(test_age,timepoint,function(x){round(sd(x),2)})))
iq_sd<-with(df_match_timepoint,as.numeric(by(IQ,timepoint,function(x){round(sd(x),2)})))
srs_sd<-with(df_match_timepoint,as.numeric(by(srs_sum,timepoint,function(x){round(sd(x,na.rm=T),2)})))
rbsr_sd<-with(df_match_timepoint,as.numeric(by(RBSR_ges,timepoint,function(x){round(sd(x,na.rm=T),2)})))
cbcl_ges_sd<-with(df_match_timepoint,as.numeric(by(CBCL_T_GES,timepoint,function(x){round(sd(x,na.rm=T),2)})))
cbcl_int_sd<-with(df_match_timepoint,as.numeric(by(CBCL_T_INT,timepoint,function(x){round(sd(x,na.rm=T),2)})))
cbcl_ext_sd<-with(df_match_timepoint,as.numeric(by(CBCL_T_EXT,timepoint,function(x){round(sd(x,na.rm=T),2)})))
BOSCC_sd<-with(df_match_timepoint,as.numeric(by(BOSCC_Average_Total,timepoint,function(x){round(sd(x),2)})))
adi_sd<-with(df_match_timepoint,as.numeric(by(adi.sum.algo,timepoint,function(x){round(sd(x,na.rm=T),2)})))
adi_toddler_sd<-with(df_match_timepoint,as.numeric(by(adi.sum.toddler,timepoint,function(x){round(sd(x,na.rm=T),2)})))

age<-paste(age_m,age_sd,sep='/')
test_age<-paste(test_age_m,test_age_sd,sep='/')
iq<-paste(iq_m,iq_sd,sep='/')
srs<-paste(srs_m,srs_sd,sep='/')
rbsr<-paste(rbsr_m,rbsr_sd,sep='/')
cbcl_ges<-paste(cbcl_ges_m,cbcl_ges_sd,sep='/')
cbcl_int<-paste(cbcl_int_m,cbcl_int_sd,sep='/')
cbcl_ext<-paste(cbcl_ext_m,cbcl_ext_sd,sep='/')
BOSCC<-paste(BOSCC_m,BOSCC_sd,sep='/')
adi<-paste(adi_m,adi_sd,sep='/')
adi_toddler<-paste(adi_toddler_m,adi_toddler_sd,sep='/')


#group comparisons
groupdiff_p<-c(NA,
               round(with(df_match_timepoint,summary(aov(test_age~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(age_months~timepoint))[[1]][1,'Pr(>F)']),3),
               round(as.numeric(with(df_match_timepoint,chisq.test(Geschlecht_Index,timepoint))['p.value']),3),
               round(with(df_match_timepoint,summary(aov(IQ~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(srs_sum~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(RBSR_ges~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(CBCL_T_GES~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(CBCL_T_INT~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(CBCL_T_EXT~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(BOSCC_Average_Total~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(adi.sum.algo~timepoint))[[1]][1,'Pr(>F)']),3),
               round(with(df_match_timepoint,summary(aov(adi.sum.toddler~timepoint))[[1]][1,'Pr(>F)']),3)
)

groupdiff_p[which(groupdiff_p==0)]<-'<0.001'
groupdiff_p[which(is.na(groupdiff_p))]<-'-'

df_sampledescribe<-cbind(
  row_names<-c('n','test age','age','gender','IQ',
               'SRS','RBSR','CBCL total (t)','CBCL int (t)','CBCL ext (t)',
               'BOSCC total','ADI sum','ADI toddler sum'),
  rbind(n_sample,test_age,age,gender,iq,srs,rbsr,cbcl_ges,cbcl_int,cbcl_ext,BOSCC,adi,adi_toddler),
  groupdiff_p
)

df_sampledescribe

require(kableExtra)
#TABLE
table_sample<-df_sampledescribe %>%
  kbl(caption = "Sample description",
      col.names = c('','TD (T2)','ASD (T2)','ASD (T4)','ASD (T6)','p-value'),
      row.names = F) %>%
  kable_classic(full_width = F, html_font = "Cambria")

#df_sampledescribe
table_sample

save_kable(table_sample, file= 'manuscript/tables/table_sampledescription.html')

##- DATA ANALYSIS - id aggregated data ####

hist(df_agg$pupillary_response,20)
hist(df_agg$time_to_target,20)
hist(df_agg$baseline_pd,20)
hist(df_agg$hit_duration,20) #aggregated for all trials

lmodel<-lm(scale(time_to_target)~timepoint+test_age+Geschlecht_Index+age_months,data=df_agg)
anova(lmodel)

lmodel<-lm(scale(baseline_pd)~timepoint+test_age+Geschlecht_Index+age_months,data=df_agg)
anova(lmodel)

lmodel<-lm(scale(pupillary_response)~timepoint+test_age+Geschlecht_Index+age_months,data=df_agg)
anova(lmodel)
TukeyHSD(lmodel,which = 'timepoint')

lmodel<-lm(scale(hit_duration)~timepoint+test_age+Geschlecht_Index+age_months,data=df_agg)
anova(lmodel)

##--> correlation tables ####
cor_matrix<-cor(df_agg[,c('pupillary_response','time_to_target','baseline_pd',
                          'hit_duration','fixation_duration','saccade_duration',
                          'BOSCC_Average_Total','test_age','age_months','CBCL_T_GES',
                          'CBCL_T_INT','CBCL_T_EXT','srs_sum','RBSR_ges')],use='complete.obs')[]

cor_matrix_format<-cor_matrix
cor_matrix_format<-round(cor_matrix_format,2)
cor_matrix_format[upper.tri(cor_matrix)]<-''
cor_matrix_format<-as.data.frame(cor_matrix_format)

cor_matrix_format
##--> nothing striking

##- DATA ANALYSIS - trial wise data quality ####

#trial data by participants / timepoints
sum(with(df_trial,by(trial,id,function(x){length(unique(x))}))) #data of trials
with(df_trial,by(trial,timepoint,function(x){sum(table(x))})) #data of trials per timepoint
summary(with(df_trial,by(trial,id,function(x){length(unique(x))}))) #mean of 18 trials per participant

with(df_trial,by(time_to_target,timepoint,function(x){table(is.na(x))}))
with(df_trial,by(pupillary_response,timepoint,function(x){table(is.na(x))}))
with(df_trial,by(baseline_pd,timepoint,function(x){table(is.na(x))}))
with(df_trial,by(hit_duration,timepoint,function(x){table(is.na(x))}))

##- DATA ANALYSIS - linear mixed models on trial data ####

require(lme4)
require(lmerTest)
require(emmeans)
require(sjPlot) #plot LMM
require(sjmisc) #plot LMM
require(glmmTMB) #plot LMM

hist(df_trial$pupillary_response,50)
hist(df_trial$baseline_pd,50)

hist(df_trial$time_to_target,50)
###--> seems like a bi-modal response (fast responses, slow responses)

lmm<-lmer(scale(time_to_target)~timepoint+
            target+target_position+
            test_age+Geschlecht_Index+age_months+
            (1|id)+(1|trial),data=df_trial[df_trial$time_to_target<1.1,])

lmm<-lmer(scale(time_to_target)~timepoint+
            target+target_position+
            test_age+Geschlecht_Index+age_months+
            (1|id)+(1|trial),data=df_trial[df_trial$time_to_target>1.1,])

anova(lmm)
plot(confint(contrast(emmeans(lmm,~timepoint),'eff')))
confint(contrast(emmeans(lmm,~timepoint),'pairwise'))
#timepoint: 1 = K, 2 = t2, 3 = t4, 4 = t6
#--> post intervention trial timepoint shows the fastest time to target

lmm<-lmer(scale(pupillary_response)~timepoint+
            target+target_position+
            test_age+Geschlecht_Index+age_months+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)
plot(confint(contrast(emmeans(lmm,~timepoint),'eff')))
confint(contrast(emmeans(lmm,~timepoint),'pairwise'))

lmm<-lmer(scale(baseline_pd)~timepoint+
            target+target_position+
            test_age+Geschlecht_Index+age_months+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)
fixef(lmm)

plot_model(lmm,type = "diag") #shown random effect structure

confint(contrast(emmeans(lmm,~group),'pairwise'))
contrast(emmeans(lmm,~target),'pairwise')
contrast(emmeans(lmm,~target_position),'eff')


lmm<-lmer(scale(hit_duration)~timepoint+
            target+target_position+
            test_age+Geschlecht_Index+age_months+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)




lmm<-lmer(scale(time_to_target)~scale(CBCL_T_GES)+scale(srs_sum)+scale(RBSR_ges)+target+target_position+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)
confint(emmeans(lmm,~scale(srs_sum)))


lmm<-lmer(scale(pupillary_response)~scale(CBCL_T_GES)+scale(srs_sum)+scale(RBSR_ges)+target+target_position+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)
confint(emmeans(lmm,~scale(srs_sum)))

lmm<-lmer(scale(baseline_pd)~scale(CBCL_T_GES)+scale(srs_sum)+scale(RBSR_ges)+target+target_position+
            (1|id)+(1|trial),data=df_trial)
anova(lmm)
cbind(fixef(lmm)['scale(RBSR_ges)'],confint(lmm,parm='scale(RBSR_ges)'))
#--> across groups a higher RBSR is associated with higher baseline PD


lmm<-lmer(scale(BOSCC_Average_Total)~timepoint+scale(CBCL_T_GES)+scale(srs_sum)+scale(RBSR_ges)+(1|id),data=df_trial[df_trial$timepoint!='K',])
anova(lmm)

### ------------------------- #########
#### current preliminary findings #########
# - t6 is associated with faster time to target
# - RBSR is associated with higher baseline PD across groups

