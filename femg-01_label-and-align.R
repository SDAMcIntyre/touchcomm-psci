library(tidyverse)
library(RcppRoll)
library(plotly)

# output folder
output.folder <- 'data/processed/expt1_femg-01_aligned'
if (!dir.exists(output.folder)) dir.create(output.folder)

#femg raw
femg.raw.files <- list.files('data/raw/expt1_femg_raw-txt', pattern = '[0-9]+', full.names = TRUE) 
channels <- read.csv('data/raw/expt1_femg_raw-txt/channel-labels.csv')
roles <- read.csv('data/raw/expt1_femg_raw-txt/role-labels.csv')
video.files <- list.files('data/processed/expt1_femg_video-timing-sync', full.names = TRUE)
commForfemg <- read_csv('data/processed/expt1_comm-01_combined.csv', col_types = 'cccicccici') %>%
  mutate(
    session = paste(
    str_replace(toucher,'P','T'),
    str_replace(receiver, 'P','R'), sep = '_')
    ) 

femg.channel.names <- c('t.zyg', 't.cor', 'r.zyg', 'r.cor')

#### functions ####

read_raw_femg <- function(fileName,skiprows = 1) {
  sampRate.Hz <- fileName %>% 
    read_lines(skip = 1, n_max = 1) %>% 
    str_extract('[0-9]+') %>% 
    as.numeric() %>% 
    ( function(x) (1 / (x / 1000)) )
  
  nChannels <- read_lines(fileName, skip = 2, n_max = 1) %>% parse_number()
  
  read_csv(fileName,skip = nChannels*2+3) %>% 
    .[-c(1:skiprows),] %>% 
    mutate(Time.sec = seq(0,n()-1)/sampRate.Hz) 
}

label_channels <- function(df, sessionID, channels, roles) {
  
  channelLabs <- channels %>% 
    filter(filename == sessionID) %>% 
    select(-filename) %>% paste0('CH', .) %>% c(., 'CH40', 'Time.sec')
  
  df <- df %>% 
    select(all_of(channelLabs))
  
  if (roles[roles$tr==sessionID,'personA'] == 'toucher') {
    names(df) <- c('t.zyg','t.cor','r.zyg','r.cor','StimCode', 'Time.sec')
  } else { names(df) <- c('r.zyg','r.cor','t.zyg','t.cor','StimCode', 'Time.sec') }
  
  return(df)
}

diff_from_prev <- function(x) { c(TRUE, diff(x) != 0) }
diff_from_next <- function(x) { c(diff(x) != 0, TRUE) }

add_transitions <- function(femgData, stimChannel) {
  femgData %>%
    ungroup() %>% 
    mutate(
      transition.start = diff_from_prev(.[[stimChannel]]),
      transition.end = diff_from_next(.[[stimChannel]])
    )
}

denoise <- function(x, nsamples = 'auto', ignoreValues = c(0)) {
  if (nsamples == 'auto') {
    # look at the data to find out how long the stimulus signal is
    runs <- rle(x)
    raw.stim.runs <- tibble(lengths = runs$lengths,
                            stimcode = runs$values)
    stimcode.nsamples <- raw.stim.runs %>% 
      filter(stimcode %in% ignoreValues == FALSE) %>% 
      pull(lengths) %>% median()
    nsamples = ceiling(stimcode.nsamples/2)
  }
  
  # look forwards
  #get the median of the last window in the dataset
  last.window.median <- median(x[(length(x)-nsamples):length(x)])
  
  # get the rolling median of the data, left-aligned, pad it with the 
  # median of the last window so that it is the same length as the data
  next.n.med <- roll_medianl(x, nsamples, fill = last.window.median)
  
  # shift the medians back by one position so that each value in the 
  # dataset aligns with the median of the window of data ahead of it ("looking ahead")
  next.n.med <- c(next.n.med[-1], next.n.med[length(next.n.med)])
  
  # look backwards (do the same as above but right aligned medians)
  first.window.median <- median(x[1:nsamples])
  prev.n.med <- roll_medianr(x, nsamples, fill = first.window.median)
  #shift it one forwards so it doesn't include itself
  prev.n.med <- c(prev.n.med[1], prev.n.med[-length(prev.n.med)])
  
  # check if it's both different from the median of the previous n samples and 
  #from the median of the next n samples
  flagged <- x != prev.n.med & x != next.n.med
  
  corrected <- x
  corrected[flagged] <- next.n.med[flagged]
  
  return(list('flagged' = flagged, 'corrected' = corrected))
  
}

clean_acq_stim_codes <- function(femgData, 
                                 stimChannel, 
                                 usedStimCodes, 
                                 knownNoiseCodes = c(), 
                                 offCode = 0, 
                                 nsamples = 'auto') {
  
  denoised <- denoise(femgData[[stimChannel]], nsamples = nsamples, 
                      ignoreValues = c(offCode, knownNoiseCodes))
  stimCodedData <- femgData %>% 
    mutate(
      # try to flag and correct unknown noise/debounce errors:
      Stim.flag.noise = denoised$flagged,
      StimCode.corrected = denoised$corrected,
      #set known noise codes to 0
      StimCode.corrected = replace(StimCode.corrected, # values to replace
                                   StimCode.corrected %in% knownNoiseCodes, # condition
                                   offCode), # replace with
      # is the stim code an unexpected one?
      unexpected = StimCode.corrected %in%  c(offCode, usedStimCodes) == FALSE
    )
  return(stimCodedData)
}  

plot_stim_code_sequence <- function(femgData, stimChannel) {
  unexpectedCodes <- femgData %>% filter(unexpected) %>% 
    .[[stimChannel]] %>% unique()
  femgData %>%
    add_transitions(stimChannel) %>%
    filter(transition.start | transition.end) %>% 
    ggplot(aes(Time.sec, .data[[stimChannel]])) +
    geom_path() +
    geom_point(aes(colour = unexpected)) +
    scale_colour_manual(values = c('grey','red')) +
    labs(title = paste(unexpectedCodes, collapse = ', '), 
         y = 'Stimulus Code',
         x = 'Time (sec)') 
}

get_video_stim <- function(videoFile, femgData, stimChannel, offCode = 0) {
  
  # add stimulus transitions to fEMG data
  femgData <- femgData %>% 
    add_transitions(stimChannel)
  
  # read in stim file
  video.sync <- suppressWarnings(
    read_tsv(videoFile, col_types = 'dddc', skip = 1,
             col_names = c('Begin.sec', 'End.sec', 'Duration.sec', 'TrialNo'))
  )
  
  # audio sync signal in the video file occurred at the same time as 
  # the trial onset signal was sent on the fEMG file for two trials
  
  # get the row numbers with the sync signal in the video file
  syncRows <- grep('signal',video.sync$TrialNo)
  
  # get the sync times in the video file
  syncTimes <- video.sync[syncRows,1] %>% pull(Begin.sec)
  
  # get the trial numbers that the sync signal appeared on
  syncTrials <- video.sync[syncRows,4] %>% 
    mutate(TrialNo = parse_number(TrialNo)) %>% 
    pull(TrialNo)
  
  # get the frames from the femg data with stimulus onset
  femg.start <- femgData %>% 
    filter(transition.start & .[[stimChannel]] != offCode)
  
  # time offset between femg and video files
  offset <- mean(femg.start$Time.sec[syncTrials] - syncTimes)
  
  # align the video to the femg time
  video.aligned <- video.sync[-syncRows,] %>% 
    mutate(begin.sec = Begin.sec + offset, 
           end.sec = End.sec + offset, 
           trial = TrialNo %>% parse_number()) %>% 
    select(begin.sec, end.sec, trial)
  
  # is the sequence in the femg data file the same as in the video  file?
  synced <- nrow(video.aligned) == nrow(femg.start)
  
  output <- list('stimSeqData' = video.aligned, 
                 'nTrials.femg'= nrow(femg.start),
                 'nTrials.video' = nrow(video.aligned),
                 'synced' = synced)
  
  # offsets for all stimuli in femg file
  femg.end <- femgData %>% 
    filter(transition.end & .[[stimChannel]] != offCode) %>% 
    pull(Time.sec) 
  
  output$touchdelay <- tibble(
    onset = video.aligned$begin.sec - femg.start$Time.sec,
    offset = video.aligned$end.sec - femg.end)
  
  return(output) 
}

plot_video_comparison <- function(femgData,stimChannel,videoStimSeq, offCode = 0) {
  # add stimulus transitions to fEMG data
  femgData <- femgData %>% 
    add_transitions(stimChannel)

    ggplot() +
    # video
    geom_rect(data = videoStimSeq,
              aes(xmin = begin.sec, ymin = 0, xmax = end.sec, ymax = 6),
              fill = 'blue', alpha = 0.3) +
    # fEMG
    geom_path(data = filter(femgData, transition.start | transition.end),
              aes(x = Time.sec, y = .data[[stimChannel]])) +
    geom_text(data = filter(femgData, transition.start & .data[[stimChannel]] != offCode),
              aes(x = Time.sec, y = .data[[stimChannel]] + 0.2, label = .data[[stimChannel]]),
              colour = 'black') +
    labs(title = 'Black: stim codes in femg file; Blue: trial periods in video file', y = stimChannel)
}

align_to_video <- function(femgData, stimChannel, videoStim, offCode = 0) {
  
  femg.codes <- femgData %>% 
    add_transitions(stimChannel) %>% 
    filter(transition.start & .[[stimChannel]] != offCode) %>% 
    pull(stimChannel)
  
  femg.synced <- femgData %>% 
    mutate(StimCode.video = offCode)
  
  for (trialNum in videoStim$stimSeqData$trial) {
    tofill <- which(
      femgData$Time.sec >= videoStim$stimSeqData$begin.sec[trialNum] & 
        femgData$Time.sec < videoStim$stimSeqData$end.sec[trialNum]
    )
    femg.synced[tofill,'StimCode.video'] <- femg.codes[trialNum]
  }
  return(femg.synced)
}

add_session_variables <- function(femgData, stimChannel, offCode = 0) {
  
  # is the code different to the previous one? 
  femgData <- femgData %>% 
    add_transitions(stimChannel)
  # get trial numbers from indices of transition points
  newTrials <- c(0, which(femgData$transition.end &
                            femgData[[stimChannel]] != offCode))
  
  # fill in all trial numbers 
  femgData <- femgData %>% mutate(trialNo = 0)
  for (n in seq_along(newTrials)[-length(newTrials)]) {
    femgData <- femgData %>% 
      mutate(trialNo = replace(trialNo, 
                               (newTrials[n]+1):newTrials[n+1], 
                               values = n))
  }
  
  # add phase info (pre-stim/stim)
  femgData <- femgData %>% 
    mutate(phase = ifelse(femgData[[stimChannel]] != offCode, 
                          'stimulus', 'prestim'),
           phase = replace(phase, trialNo == 0, NA))
  
  # time relative to stimulus onset
  femgData <- femgData %>%
    group_by(trialNo) %>%
    mutate(stimTime.sec = Time.sec - min(Time.sec[phase!='prestim']))
  
  return(femgData %>% ungroup)
}


#### loop ####
report <- list()

for (f in 1:length(femg.raw.files)) {
  fileName <- femg.raw.files[f]
  sessionID <- str_extract(fileName,'T[0-9]+_R[0-9]+')
  print(paste(f,'of',length(femg.raw.files),':',sessionID))
  report[[sessionID]] <- list()
  
  # clip some bad data from before the session started
  skiprows <- switch(sessionID,
                     T49_R59 = 410000,
                     T67_R46 = 200000,
                     T75_R74 = 500000, 
                     T97_R58 = 250000,
                     T99_R14 = 150000,
                     1)

  femg.raw <- read_raw_femg(fileName, skiprows = skiprows) 
  
  femg.labs <- femg.raw %>% 
    label_channels(sessionID, channels, roles)
  
  # Look for errors in the marker channel and try to fix them
  femg.clean.codes <- clean_acq_stim_codes(femgData = femg.labs,
                                           stimChannel = 'StimCode',
                                           usedStimCodes = c(1:6),
                                           nsamples = 400) 
  
  # now check for unexpected stim codes
  femg.clean.codes %>%
    filter(unexpected) %>%
    group_by(StimCode.corrected) %>%
    tally()

  # femg.clean.codes %>%
  #   plot_stim_code_sequence('StimCode.corrected') %>%
  #   ggplotly()
  
  #### video sync ####
  
  vf <- video.files[str_detect(video.files, sessionID)]
  if (length(vf) > 0 ) {
    
    videoStim <- get_video_stim(vf, 
                                femg.clean.codes, 'StimCode.corrected')
    
    # correct number of trials?
    print(videoStim$synced)
    report[[sessionID]]$synced <- videoStim$synced
    report[[sessionID]]$nTrials.femg <- videoStim$nTrials.femg
    report[[sessionID]]$nTrials.video <- videoStim$nTrials.video
    
    # median delay from button-press to video touch
    report[[sessionID]]$medianDelays <- videoStim$touchdelay %>% 
      summarise(across(.fns = median))
    
    # see differences
    report[[sessionID]]$plot.before <- plot_video_comparison(
      femg.clean.codes, 'StimCode.corrected', videoStim$stimSeqData) %>% 
      ggplotly()

  } else {
    videoStim <- list()
    videoStim$synced <- FALSE }
  
  if (videoStim$synced) {
    
    femg.synced <- align_to_video(femg.clean.codes, 'StimCode.corrected', videoStim)
    stimChannel <- 'StimCode.video'
    
    report[[sessionID]]$plot.after <- plot_video_comparison(
      femg.synced, 'StimCode.video', videoStim$stimSeqData) %>% 
      ggplotly()
    
  } else {
    
    femg.synced <- femg.clean.codes
    stimChannel <- 'StimCode.corrected'
  }
    
  
  #### finalise #####
  
  femg.session <- femg.synced %>% 
    add_session_variables(stimChannel)  %>% 
    select(c('Time.sec',
             'stimTime.sec',
             'Stimulus' = stimChannel,
             'trialNo',
             'phase',
             all_of(femg.channel.names))) %>% 
    full_join( commForfemg %>% filter(session == sessionID), by = c('trialNo' = 'trial'))
  
  # fEMG started recording part-way through the first trial, so exclude it
  if (sessionID == 'T87_R44') { 
    femg.session <- femg.session[3000:nrow(femg.session),]
  }
  
  expt.end <- femg.session %>% 
    filter(trialNo == max(trialNo)) %>% pull(Time.sec) %>% max()
  
  femg.session %>% 
    # cut out anything longer than 1 minute before the first trial
    filter(stimTime.sec > -60) %>% 
    # cut out anything longer than 1 minute after the last trial
    filter(stimTime.sec <= expt.end + 60) %>% 
    write_csv(paste0(output.folder,'/',sessionID,'_femg_aligned.csv'))

}

save(report, file = 'preprocess-femg-01_video_align_report.RData')

