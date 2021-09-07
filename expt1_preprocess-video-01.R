library(stringr)
library(dplyr)
library(tidyr)
library(readr)

#### functions ####

align_trial_labels <- function(x) {
  # fix manual trial label misalignment
  for (unlabelled.i in which(is.na(x$trial) | x$trial == '')) {
    labelled.idx <- which(!is.na(x$trial) & x$trial != '')
    earlier.idx <- labelled.idx[which(sign(unlabelled.i - labelled.idx) > 0)]
    prev.idx <- earlier.idx[length(earlier.idx)]
    later.idx <- labelled.idx[which(sign(unlabelled.i - labelled.idx) < 0)]
    nxt.idx <- later.idx[1]
    if (length(earlier.idx)==0) { x$trial[unlabelled.i] <- x$trial[nxt.idx] 
    } else {
      if (length(later.idx)==0) { x$trial[unlabelled.i] <- x$trial[prev.idx]
      } else {
        timeAfterPrevEnd <- x$End.sec[unlabelled.i] - x$End.sec[prev.idx]
        timeBeforeNextStart <- x$Start.sec[nxt.idx] - x$Start.sec[unlabelled.i]
        if (timeBeforeNextStart < timeAfterPrevEnd) {
          x$trial[unlabelled.i] <- x$trial[nxt.idx]
        } else { x$trial[unlabelled.i] <- x$trial[prev.idx] }
      }}
  }
  return(x)
}

read_video_file <- function(fpath,fname) {
  read.table(paste0(fpath,fname), sep = '\t', skip = 1, 
             fill = TRUE, stringsAsFactors = FALSE) %>% 
    setNames(c(
      'Start.sec', 'End.sec', 'Duration.sec', 'trial',
      paste0( 'Touch', 1:(length(.)-4) )
    )) %>% 
    pivot_longer(cols = starts_with('Touch'), values_to = 'Description') %>% 
    arrange(Start.sec, End.sec) %>% 
    align_trial_labels() %>% 
    filter(nchar(Description) > 0) %>% 
    group_by(trial, name) %>% 
    mutate(y = max(Duration.sec)) %>% 
    ungroup() %>% 
    filter(Duration.sec == y) %>% 
    select(-c(name,y)) %>% 
    mutate(
      toucher = str_extract(fname, 'T[0-9]{2}') %>% str_replace('T', 'P'),
      receiver = str_extract(fname, 'R[0-9]{2}') %>% str_replace('R', 'P'),
      trial = trial %>% as.numeric
    )
}

read_all_video_files <- function(fpath, fnames) {
  video.data <- c()
  for (f in 1:length(fnames)) {
    print(paste(f,'of',length(fnames),fnames[f]))
    video.data.1 <- read_video_file(fpath, fnames[f])
    print(paste(max(video.data.1$trial, na.rm = TRUE), 'trials.'))
    video.data <- rbind(video.data, video.data.1)
  }
  return(video.data)
}


#### read all video data ####

video.files.path <- 'data/raw/expt1_video-annotations-00_raw/'
video.files <- dir(path = video.files.path, pattern = 'txt')

video.data.all <- read_all_video_files(video.files.path, video.files)


#### combine with comm data ####

comm.data.all <- read_csv('data/processed/expt1_comm-01_combined.csv')

video.data.combined <- left_join(video.data.all, comm.data.all) %>% 
  select(trial, toucher, receiver, everything())

write.csv(video.data.combined, 
          'data/processed/expt1_video-01_combined.csv', 
          row.names = FALSE)
