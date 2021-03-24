library(tidyverse)
library(RcppRoll)

#### functions ####
detect_artifacts <- function(df, prefixes, win.sec, flag.threshold) {
  
  roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)
  
  sample.duration <- diff(df$stimTime.sec[1:2])
  nSamples <- win.sec/sample.duration
  for (muscleName in prefixes) {
    rawVar <- names(df) %>% str_subset(muscleName)
    name.z <- paste0(muscleName,'.z')
    name.zrange <- paste0(name.z,'range')
    name.flagged <- paste0(muscleName, '.flagged')
    name.rawfixed <- paste0(muscleName, '.fixed')
    name.zfixed <- paste0(muscleName, '.zfixed')
    df <- df %>% 
      mutate(!!name.z := scale(.[[rawVar]])[,1]) %>% 
      mutate(!!name.zrange := roll_range(.[[name.z]], n = nSamples, fill = NA)) %>% 
      mutate(!!name.flagged := abs(.[[name.zrange]]) > flag.threshold) %>% 
      mutate(!!name.flagged := replace_na(.[[name.flagged]], FALSE)) %>% 
      
      mutate(!!name.rawfixed := if_else(.[[name.flagged]], 
                                        as.numeric(NA), 
                                        .[[rawVar]])) %>% 
      
      mutate(!!name.zfixed := if_else(.[[name.flagged]], 
                                      as.numeric(NA), 
                                      .[[name.z]]))
  }
  return(df)
}

####

#### main ####

output.file <- 'data/expt1_femg-02_cleaned-all.csv'

femg.files <- list.files('data/expt1_femg-01_aligned', full.names = TRUE) 

prefixes <- c('t.zyg', 't.cor', 'r.zyg', 'r.cor')

##### artifact detection #####

femg.z <- femg.files %>% 
  map_dfr(read_csv, col_types = 'ddiicddddccccccicic') %>% 
  # cut anything after the last stimulus (where trilalNum is 0)
  filter(trialNo >= 1) %>%
  group_by(session) %>% 
  do(detect_artifacts(., prefixes, win.sec = 0.05, flag.threshold = 3))

##### trial durations #####

trial.durations <- femg.z %>% 
  group_by(session,cued,trialNo) %>% 
  summarise(stim.duration = max(stimTime.sec, na.rm = TRUE),
            nonstim.duration = min(stimTime.sec, na.rm = TRUE)) %>% 
  ungroup()

trial.durations %>%
  ggplot(aes(x=stim.duration)) +facet_grid(cued ~.) + geom_histogram(binwidth = 1)

trial.durations %>% 
  filter(stim.duration < 1) %>% 
  arrange(stim.duration) 

trial.durations %>%
  ggplot(aes(x=-nonstim.duration)) +facet_grid(cued ~.) + geom_histogram(binwidth = 1)

trial.durations %>% 
  filter(nonstim.duration > -10) %>% 
  arrange(nonstim.duration) 

# count trials
# per session
trial.durations %>% group_by(session) %>% tally() 

# per cue
trial.durations %>% group_by(cued) %>% tally() 

# total
trial.durations %>% tally() 

##### trim pre-stim data and save ####
femg.z %>% 
  filter(stimTime.sec >= -2) %>% 
write_csv(output.file)
