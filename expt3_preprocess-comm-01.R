library(stringr)
library(dplyr)
library(readr)

#### functions #####

read_comm_data <- function(fpath,fname,infoDataFiles) {
  rawData <- read.csv(paste0(fpath,fname),stringsAsFactors = FALSE)
  
  prefix <- strsplit(fname,'_communication-data')[[1]][1]
  infoFile <- infoDataFiles[grepl(prefix,infoDataFiles)]
  info <- read.csv(paste0(fpath,infoFile), 
                   stringsAsFactors = FALSE, header = FALSE, 
                   col.names = c('prompt','input','input2'))
  
  dateTime <- info$input[grepl('Date and time',info$prompt)]
  rawParticipant <- info$input[grepl('Participant Code',info$prompt)]
  PID <- paste0('P',str_extract(rawParticipant,'[:digit:].'))
  
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
    group_by(dateTime, cued) %>% 
    mutate(presentationNumber = trial %>% as.factor %>% as.numeric)
  return(subset(commData, select = -dateTime))
}

#### read in the data ####
behDataDir <- 'data/private/expt3_comm-00_raw/'
commDataFiles <- dir(path = behDataDir, pattern = 'communication-data')
infoDataFiles <- dir(path = behDataDir, pattern = 'info')

commData <- read_all_comm_data(behDataDir,commDataFiles,infoDataFiles)
write_csv(commData, 'data/processed/expt3_comm-01_combined.csv')
