library(dplyr)
library(readr)
library(stringr)

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

output.folder <- 'data/processed/femg-04_cleaned-time-selected'
if (!dir.exists(output.folder)) dir.create(output.folder)


# trim to just baseline and stimulus
# chosen based on time plot
baseline.duration.sec <- 0.2
baseline.offset.sec <- 1.0

femg.binned <- read_csv('data/processed/femg-03_binned-100ms-clean.csv') %>% 
    mutate(phase = replace(phase, # values to replace
                           time >= -(baseline.duration.sec + baseline.offset.sec) &
                             time < -baseline.offset.sec, # conditions
                           'baseline')) %>% 
   filter(phase == 'stimulus' | phase == 'baseline')


#### export data for classifier (python) ####

# #femg channels
prefixes <- c('t.zyg', 't.cor', 'r.zyg', 'r.cor')

window.subsets <- list(c(0,4))

femg.out <- list()

for (slice in seq_along(window.subsets)) {
  
  subwindow <- window.subsets[[slice]]
  
  femg.subset <- femg.binned %>% 
    mutate(expt.trialNo = paste(session,trialNo) %>% as.factor %>% as.numeric) %>% 
    filter( (phase == 'baseline') | (time >= subwindow[1] & time < subwindow[2]) )
  
  femg.out[[slice]] <- list()
  
  for (muscleName in prefixes) {
    
    femg.out[[slice]][[muscleName]] <- list()
    
    # names of variables depending on the muscle
    name_clean = paste0(str_remove(muscleName,'\\.'),'_clean')
    name_norm = paste0(str_remove(muscleName,'\\.'),'_norm')
    
    femg.out[[slice]][[muscleName]] <- femg.subset %>% 
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
                     'to',subwindow[2],'sec_100ms-binned.csv')
    )
  }
}
