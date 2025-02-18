

##### ------- Data exploration -------- #########

Sys.setenv(LANG = "en")
options(scipen=10)
require(dplyr)


### load files

load(file="data/all_data_preprocessed_280824.Rdata") # load preprocessed data (see code/data_preprocessing_visualsearch_0524.R)
## save(df, file = "data/df") # save dfs separately to reduce loading time
# save(df_agg, file = "data/df_agg")
# save(df_trial, file = "data/df_trial")
# load("data/df") # takes super long to load, enable only when needed
load("data/df_agg")
load("data/df_trial")

load("data/demo_df.Rdata") # demographic data

######################################
### EXPLORATION VISUAL SEARCH DATA ###
######################################

table(df_trial$id)
table(df_agg$id)

unique(df_trial$pic)
unique(df_agg$pic) # IDs are ok

str(df_trial) # structure
str(df_agg)

length(unique(df_trial$pic))
length(unique(df_agg$pic)) # total n

df_trial %>% group_by(group, timepoint) %>% summarise(n = n())
df_agg %>% group_by(group, timepoint) %>% summarise(n = n()) # n per timepoint and group
# # change order of timepoint factor levels
# df_trial$timepoint <- factor(df_trial$timepoint, levels = c("T2", "K", "T4", "T6", "FU2", "K_FU", "FU3"))
# df_agg$timepoint <- factor(df_agg$timepoint, levels = c("T2", "K", "T4", "T6", "FU2", "K_FU", "FU3"))

# missing data
colSums(is.na(df_trial)) # 3081 rows with only NA >> why??
df_trial <- df_trial[!is.na(df_trial$id),] # remove those rows

colSums(is.na(df_agg)) # ok

# IDs
table(df_trial$pic)
table(df_agg$pic)
df_agg %>%
  dplyr::group_by(pic, timepoint) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) # one duplicate
df_agg[df_agg$pic == "025",] # inspect duplicate
df_agg <- df_agg[!df_agg$id == '025_t2',] # drop false row (025_t2)



#df_trial$id <- trimws(df_trial$id) # removes whitespace

# trials
length(unique(df_trial$trial))
table(df_trial$trial) # 64 trials

# targets
table(df_trial$target) # 4 targets (H, O, S, V)

# target postitions
table(df_trial$target_position) # 8 positions

# number of hits
table(df_trial$missed_target)
prop.table(table(df_trial$missed_target)) # 20 % hits (overall)
table(df_trial$missed_target, df_trial$timepoint)# split per timepoint
prop.table(table(df_trial %>% filter(timepoint == "T2") %>% pull(missed_target))) # 25 % hits at T2
prop.table(table(df_trial %>% filter(timepoint == "T4") %>% pull(missed_target))) # 19 % hits at T4
prop.table(table(df_trial %>% filter(timepoint == "T6") %>% pull(missed_target))) # 19 % hits at T6
prop.table(table(df_trial %>% filter(timepoint == "FU2") %>% pull(missed_target))) # 20 % hits at FU2
prop.table(table(df_trial %>% filter(timepoint == "K") %>% pull(missed_target))) # controls: 21 % hits at baseline
prop.table(table(df_trial %>% filter(timepoint == "K_FU") %>% pull(missed_target))) # controls: 14 % hits at FU

# timepoints
table(df_trial$timepoint)
table(df_agg$timepoint)

# change timepoints to match demo data
levels(df_trial$timepoint)
levels(df_trial$timepoint) <- c(levels(df_trial$timepoint), 'FU') # add FU as level otherwise changing the level will not work
df_trial$timepoint[df_trial$group == 'TD' & df_trial$timepoint == 'K'] <- 'T2'
df_trial$timepoint[df_trial$group == 'TD' & df_trial$timepoint == 'K_FU'] <- 'FU'
df_trial$timepoint[df_trial$group == 'ASD' & df_trial$timepoint == 'FU2'] <- 'FU'
df_trial <- df_trial[!df_trial$timepoint == 'FU3',] # drop FU3
df_trial$timepoint <- droplevels(df_trial$timepoint, exclude = c("FU2", "FU3", "K", "K_FU"))

levels(df_agg$timepoint)
levels(df_agg$timepoint) <- c(levels(df_agg$timepoint), 'FU') # add FU as level otherwise changing the level will not work
df_agg$timepoint[df_agg$group == 'TD' & df_agg$timepoint == 'K'] <- 'T2'
df_agg$timepoint[df_agg$group == 'TD' & df_agg$timepoint == 'K_FU'] <- 'FU'
df_agg$timepoint[df_agg$group == 'ASD' & df_agg$timepoint == 'FU2'] <- 'FU'
df_agg <- df_agg[!df_agg$timepoint == 'FU3',] # drop FU3
df_agg$timepoint <- droplevels(df_agg$timepoint, exclude = c("FU2", "FU3", "K", "K_FU"))

#######################
### SAVE CLEAN DATA ###
#######################

save(df_agg, file = "data/df_agg")
save(df_trial, file = "data/df_trial")