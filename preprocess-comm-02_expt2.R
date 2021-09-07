library(stringr)
library(dplyr)
library(readr)

##### functions #####

read_comm_data <- function(fpath,fname,infoDataFiles) {
  rawData <- read.csv(paste0(fpath,fname),stringsAsFactors = FALSE)
  
  prefix <- strsplit(fname,'_communication-data')[[1]][1]
  infoFile <- infoDataFiles[grepl(prefix,infoDataFiles)]
  info <- read.csv(paste0(fpath,infoFile), 
                   stringsAsFactors = FALSE, header = FALSE, 
                   col.names = c('prompt','input','input2'))
  
  dateTime <- info$input[grepl('Date and time',info$prompt)]
  rawPair <- info$input[grepl('Participant Pair Code',info$prompt)]
  pair <- strsplit(rawPair,'-')[[1]][1] # remove session number
  if (grepl('AA',pair)) { # remove session number in this one case
    pair <- 'PAA'
  } else { # just get the number and put P in front
    pair <- paste0('P',str_extract(pair,'[:digit:].'))
  }
  
  return(data.frame(rawData, pair, dateTime, stringsAsFactors = FALSE))
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
    mutate(sessionNo = dateTime %>% as.factor %>% as.numeric ) %>% 
    ungroup() %>% 
    mutate(pairTrial = trial+(sessionNo-1)*100) %>% 
    group_by(pair, cued) %>%
    mutate( presentationNumber = pairTrial %>% as.factor %>% as.numeric  ) %>% 
    mutate(sessionNo = paste('Session', sessionNo) ) %>% 
    select(-c(dateTime,pairTrial))
  return(commData)
}

remove_exclusions <- function(df,excl) {
  df$exclude <- FALSE
  df[df$pair %in% excl,]$exclude <- TRUE
  df <- subset(df, exclude == FALSE, select = -exclude)
  return(df)
}


##### combine/process data #####

# locate the data files
behDataDir <- 'data/private/expt2_comm-00_raw/'
commDataFiles <- dir(path = behDataDir, pattern = 'communication-data')
infoDataFiles <- dir(path = behDataDir, pattern = 'info')

# read in communication data
commData <- read_all_comm_data(behDataDir,commDataFiles,infoDataFiles)

# participant pairs to exclude
excludePairs <- c( 'P02','P20')
commData <- remove_exclusions(commData,excludePairs)

##### write to file ####
commData %>%
  arrange(pair,sessionNo,trial) %>% 
  write_csv('data/processed/expt2_comm-01_combined.csv')
