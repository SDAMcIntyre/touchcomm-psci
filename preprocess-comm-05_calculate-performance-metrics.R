library(readr)
library(dplyr)

#### functions ####

tally_by <- function(.data, ...) {
  byVars <- enquos(..., .named = TRUE)
  xformula <- reformulate(termlabels = c(names(byVars)))
  .data %>%
    xtabs(formula = xformula) %>%
    as_tibble() %>%
    arrange(...)
}

performance_data <- function(.data, item, response, ...) {
  .data %>% 
    # count up responses for all combos of item and response
    tally_by(..., {{item}}, {{response}}) %>% 
    # calculate total number of trials/presentations
    group_by(...) %>% mutate(Total = sum(n)) %>% 
    # number of times each cue appeared
    group_by(...,{{item}}) %>% mutate(Present = sum(n)) %>%
    # number of times each response was made
    group_by(..., {{response}}) %>% mutate(Selected = sum(n)) %>%
    # get rid of unneeded rows (we're summarising, one line for each cue)
    filter({{item}} == {{response}}) %>% rename('Hits' = 'n') %>% # now counts are just hits
    # calculate performance variables
    ungroup() %>% 
    mutate(Misses = Present - Hits,
           FalseAlarms = Selected - Hits,
           CorrectRejections = Total - Present - FalseAlarms,
           Recall = Hits/Present, # p correct when cue present
           Precision = Hits/Selected, # p correct with this response
           Precision = if_else(is.na(Precision), 0, Precision), 
           F1 = 2*( (Precision*Recall) / (Precision+Recall)), 
           F1 = if_else(is.na(F1), 0, F1),
           F1chance = 2*( (Present/Total) / ((Present/Total)+1) ) # always give same answer
    )
}


#### read data ####
comm.e1 <- read_csv('data/processed/expt1_comm-01_combined.csv') %>%
  mutate(exptPID = paste('E1', pair, sep = '.'),
         roles = if_else(SessionNo == 'Session 1', 'Initial roles', 'Swapped roles'))

comm.e2 <- read_csv('data/processed/expt2_comm-01_combined.csv') %>%
  mutate(exptPID = paste('E2',pair, sep = '.'))

comm.e3 <- read_csv('data/processed/expt3_comm-01_combined.csv') %>%
  mutate(toucher = 1,
         exptPID = paste('E3',PID, sep = '.'))

comm.e4 <- read_csv('data/processed/expt4_comm-01_combined.csv') %>%
  mutate(exptPID = paste('E4',PID, sep = '.'))

#### calculate performance metrics and save ####

comm.e1 %>%
  filter(response != 'open') %>% 
  performance_data(cued, response, exptPID, roles) %>% 
  write_csv('data/processed/expt1_comm-02_performance.csv')

comm.e2 %>%
  performance_data(cued, response, exptPID) %>% 
  write_csv('data/processed/expt2_comm-02_performance.csv')

comm.e3 %>%
  performance_data(cued, response, exptPID) %>% 
  write_csv('data/processed/expt3_comm-02_performance.csv')

comm.e4 %>%
  performance_data(cued, response, exptPID) %>% 
  write_csv('data/processed/expt4_comm-02_performance.csv')

full_join(comm.e3,comm.e4) %>%
  performance_data(cued, response, presentationNumber) %>% 
  write_csv('data/processed/expts3-4_comm-02_performance-over-time.csv')
