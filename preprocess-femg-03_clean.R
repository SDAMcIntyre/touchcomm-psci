library(tidyverse)

#### functions ####
summarise_flagged_trials <- function(df, prefixes) {
  sort_unique <- function(x) {
    y <- unique(x)
    y[order(y)] }
  
  output <- list()
  output[['flaggedOnAnyMuscle']] <- c()
  for (muscleName in prefixes) {
    
    name.flagged <- paste0(muscleName, '.flagged')
    
    allFlaggedTrials <- df %>% 
      filter( .[[name.flagged]] ) %>% 
      distinct(expt.trialNo) %>% 
      pull(expt.trialNo) 
    
    nTrials <- n_distinct(df$expt.trialNo)
    nFlaggedTrials <- length(allFlaggedTrials)
    
    output[[muscleName]] <- list('allFlaggedTrials' = allFlaggedTrials,
                                 'nTrials' = nTrials,
                                 'nFlaggedTrials' = nFlaggedTrials,
                                 'pcFlaggedTrials' = 100*nFlaggedTrials/nTrials)
    
    
    output[['flaggedOnAnyMuscle']] <- output[['flaggedOnAnyMuscle']] %>% 
      c(allFlaggedTrials)
  }
  output[['flaggedOnAnyMuscle']] <- output[['flaggedOnAnyMuscle']] %>% sort_unique()
  return(output)
}


#### main ####

output.folder <- 'data/expt1_femg-03_cleaned-time-selected'
if (!dir.exists(output.folder)) dir.create(output.folder)

femg.z <- read_csv('data/expt1_femg-02_cleaned-all.csv', col_types = 'ddiicddddccccccicicddlddddlddddlddddldd')


##### clean and export #####

# trim to just baseline and stimulus
baseline.duration.sec <- 0.2
baseline.offset.sec <- 1.0

femg.binned <- femg.z %>% 
  mutate(phase = replace(phase, # values to replace
                         stimTime.sec >= -(baseline.duration.sec + baseline.offset.sec) & 
                           stimTime.sec < -baseline.offset.sec, # conditions
                         'baseline')) %>% 
  mutate(
    bin.n = cut(stimTime.sec, 
                breaks = seq(min(stimTime.sec), max(stimTime.sec), by = bin.sec), 
                labels = FALSE),
    time = (bin.n-1) * bin.sec + min(stimTime.sec) 
  ) %>% 
  group_by(session,cued,trialNo,phase,time) %>% 
  summarise(
    across(.cols = starts_with(prefixes), 
           .fns = ~mean(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  do(clean_bins(., prefixes)) %>% 
  filter(phase == 'stimulus' | phase == 'baseline')

# reject trials with artifacts and
# export data for classifier (python)

window.subsets <- list(c(0,4))
# window.subsets <- list(c(0,1),
#                        c(1,2),
#                        c(2,3),
#                        c(3,4))

# 100 ms windows
bin.sec <- 0.1

femg.out <- list()

for (slice in seq_along(window.subsets)) {
  
  subwindow <- window.subsets[[slice]]
  
  femg.subset <- femg.binned %>% 
    mutate(expt.trialNo = paste(session,trialNo) %>% as.factor %>% as.numeric) %>% 
    filter( (phase == 'baseline') | (time >= subwindow[1] & time < subwindow[2]) )
  
  # trials <- summarise_flagged_trials(femg.subset, prefixes)
  
  femg.out[[slice]] <- list()
  
  for (muscleName in prefixes) {
    
    femg.out[[slice]][[muscleName]] <- list()
    
    # names of variables depending on the muscle
    name_clean = paste0(str_remove(muscleName,'\\.'),'_clean')
    name_norm = paste0(str_remove(muscleName,'\\.'),'_norm')
    
    femg.out[[slice]][[muscleName]] <- femg.subset %>% 
      # filter( !(expt.trialNo %in% trials[[muscleName]]$allFlaggedTrials) ) %>% 
      select(time,
             trial = trialNo,
             session,
             phase,
             cue = cued,
             !!name_clean := !!paste0(muscleName,'.fixed'),
             !!name_norm := !!paste0(muscleName,'.zfixed')) %>% 
      na.omit()
    
    nByCue <- femg.out[[slice]][[muscleName]] %>% 
      group_by(cue, session, trial) %>%
      tally() %>% tally() %>% summarise(n = sum(n)) 
    
    N <- nByCue %>% summarise(n = sum(n))
    
    print(
      paste('slice:', subwindow[1], '-', subwindow[2],'s', 
            'muscle:', muscleName, 'N =', N)
    )
    print(nByCue)
    
    write_csv(femg.out[[slice]][[muscleName]],
              paste0(output.folder,'/fEMG_clean_', 
                     str_remove(muscleName,'\\.'),
                     '_',subwindow[1],
                     'to',subwindow[2],'sec_100ms-binned_25Mar2021.csv')
    )
  }
}

#### checks #### 

m <- 'rzyg'
x <- read_csv(paste0(output.folder,'/fEMG_clean_',m,'_0to4sec_100ms-binned_25Mar2021.csv'), 
              col_types = cols(trial = col_integer()))
x %>%
  ggplot(aes(x = time, y = .[[paste0(m,'_norm')]],
             colour = cue, fill = cue, linetype = cue, shape = cue)) +
  geom_vline(xintercept = 0) +
  stat_summary(fun = 'mean', geom = 'line') +
  stat_summary(fun = 'mean', geom = 'point') +
  theme_bw() +
  labs(x = 'Time (seconds)', y = 'Muscle Activity (z)')

x[!complete.cases(x),]

# m <- 'tzyg'
# new <- read_csv(paste0('data/femg cleaned/fEMG_clean_',m,'_0to1sec_15Mar2021.csv'), 
#                 col_types = cols(trial = col_integer()))
# 
# old <- read_csv(paste0('/Users/sarmc72/OneDrive - Linköpings universitet/sarmc72/SarahWorking/',
#                        'language of social touch/PsychScience Revision 2/femg preprocessing/',
#                        'fEMG_clean_',m,'.csv'),
#                 col_types = cols(trial = col_integer()))
# 
# new[!complete.cases(new),]
# old[!complete.cases(old),]
# 
# new.trials <- new %>% group_by(session,trial) %>% summarise(dataset = 'new')
# old.trials <- old %>% group_by(session,trial) %>% summarise(dataset = 'old')
# 
# # number of trials per session in each dataset
# new.trials %>% 
#   group_by(session) %>% 
#   tally() %>% View()
# 
# old.trials %>% 
#   group_by(session) %>% 
#   tally() %>% View()
# 
# compare <- full_join(new.trials,old.trials, by = c('session', 'trial'))
# compare[!complete.cases(compare),] %>% View()
# 
# # in old dataset, not in new
# compare[!complete.cases(compare),] %>% 
#   filter(!is.na(dataset.y)) %>% 
#   group_by(session) %>% 
#   tally(sort = TRUE) #%>% summarise(sum(n))
# 
# # in new dataset, not in old
# compare[!complete.cases(compare),] %>% 
#   filter(!is.na(dataset.x)) %>% 
#   group_by(session) %>% 
#   tally(sort = TRUE) #%>% summarise(sum(n))


