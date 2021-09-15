library(stringr)
library(dplyr)
library(readr)

#### functions ####

reconstruct_comm_file <- function(behDataDir, missingCommData) {
  for (f in 1:length(missingCommData)) {
    logFile <- paste0(behDataDir,missingCommData[f],'_log.csv')
    logData <- read.csv(logFile, stringsAsFactors = FALSE)
    t.tag <- 'toucher cue '
    cued <- str_replace(logData$event[grepl(t.tag, logData$event)], t.tag,'')
    r.tag <- 'receiver responded '
    response.correct <- str_replace(logData$event[grepl(r.tag, logData$event)], r.tag,'')
    response <- str_split(response.correct, ' - ', simplify = TRUE)[,1]
    correct <- as.numeric(str_split(response.correct, ' - ', simplify = TRUE)[,2] == 'correct')
    nCompleteTrials <- length(response.correct)
    commData <- data.frame( trial = 1:nCompleteTrials,
                            cued, response, other.text = '', correct)
    write.csv(commData,paste0(behDataDir,missingCommData[f],'_communication-data_RECONSTRUCTED.csv'), row.names = FALSE)
  }
}

read_comm_data <- function(fpath,fname,infoDataFiles) {
  rawData <- read.csv(paste0(fpath,fname),stringsAsFactors = FALSE)
  
  prefix <- strsplit(fname,'_communication-data')[[1]][1]
  infoFile <- infoDataFiles[grepl(prefix,infoDataFiles)]
  info <- read.csv(paste0(fpath,infoFile), 
                   stringsAsFactors = FALSE, header = FALSE, 
                   col.names = c('prompt','input','input2'))
  
  dateTime <- info$input[grepl('Date and time',info$prompt)]
  PID <- paste0('P',info$input[grepl('Participant Code',info$prompt)])
  
  return(data.frame(rawData, PID, dateTime, stringsAsFactors = FALSE))
}

read_all_comm_data <- function(fpath,fnames,infoDataFiles){
  commData <- c()
  for (f in 1:length(fnames)) {
    print(paste(f,'of',length(fnames),fnames[f]))
    commData <- rbind(commData,
                      read_comm_data(fpath,fnames[f],infoDataFiles))
  }
  commData <- commData %>% 
    group_by(PID) %>% 
    mutate(
      sessionNo = paste('Session', dateTime %>% as.factor %>% as.numeric),
      totalTrial = (dateTime %>% as.factor %>% as.numeric -1) * max(trial) + trial
    ) %>% 
    group_by(PID, cued) %>% 
    mutate(presentationNumber = totalTrial %>% as.factor %>%  as.numeric)
  return(subset(commData, select = -dateTime))
  return(commData)
}


#### reconstruct missing files ####
behDataDir <- 'data/private/expt4_comm_raw/'
missingCommData <- c('touch-comm-Behav_2018-04-06_11-24-35_P009',
                     'touch-comm-Behav_2018-04-06_15-00-56_P010')
reconstruct_comm_file(behDataDir, missingCommData)

#### read in and combine data ####
commDataFiles <- dir(path = behDataDir, pattern = 'communication-data')
infoDataFiles <- dir(path = behDataDir, pattern = 'info')

commData <- read_all_comm_data(behDataDir,commDataFiles,infoDataFiles)
write_csv(commData, 'data/processed/expt4_comm-01_combined.csv')
