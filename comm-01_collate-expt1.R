library(readr)
library(dplyr)

# This script takes raw data files from the communication task with potentially identifying 
# information (timestamps for participation) and removes/obscures these details, and 
# combines all sessions into one file for convenience. The raw files with identifying 
# information (input for this script) are available on request, the combined file (output
# for this script) is published on the public repository

##### FUNCTIONS #####

read_comm_data <- function(fpath,fname,infoDataFiles) {
  rawData <- read.csv(paste0(fpath,fname),stringsAsFactors = FALSE)
  
  prefix <- strsplit(fname,'_communication-data')[[1]][1]
  infoFile <- infoDataFiles[grepl(prefix,infoDataFiles)]
  info <- read.csv(paste0(fpath,infoFile), 
                   stringsAsFactors = FALSE, header = FALSE, 
                   col.names = c('prompt','input','input2'))
  
  dateTime <- info$input[grepl('Date and time',info$prompt)]
  toucher <- paste0('P',info$input[grepl('Toucher Participant Code',info$prompt)])
  receiver <- paste0('P',info$input[grepl('Receiver Participant Code',info$prompt)])
  tr <- sort(c(toucher,receiver))
  pair = paste(tr[1], tr[2], sep = '-')
  
  return(data.frame(rawData, toucher, receiver, pair, dateTime, stringsAsFactors = FALSE))
}

read_all_comm_data <- function(fpath,fnames,infoDataFiles){
  commData <- c()
  for (f in 1:length(fnames)) {
    print(paste(f,'of',length(fnames),fnames[f]))
    commData <- rbind(commData,
                      read_comm_data(fpath,fnames[f],infoDataFiles))
  }

  commData <- commData %>% 
    group_by(pair) %>% 
    mutate(SessionNo = paste('Session', (dateTime %>% as.factor %>% as.numeric) )) %>% 
    group_by(dateTime, cued) %>% 
    mutate(presentationNumber = trial %>% as.factor %>% as.numeric )
  return(subset(commData, select = -dateTime))
}


##### COMBINE DATA #####
# locate the data files
behDataDir <- 'data/private/comm_expt1-raw/' 
commDataFiles <- dir(path = behDataDir, pattern = 'communication-data')
infoDataFiles <- dir(path = behDataDir, pattern = 'info')

# read in communication data
commData <- read_all_comm_data(behDataDir,commDataFiles,infoDataFiles)

# rearrange and write to file
commData %>% 
  select(toucher, receiver, pair, everything()) %>% 
  arrange(toucher,SessionNo,cued,presentationNumber) %>% 
  write_csv('data/primary/comm_expt1-collated.csv')
