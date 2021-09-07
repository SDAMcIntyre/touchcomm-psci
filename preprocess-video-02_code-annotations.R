library(readr)
library(stringr)

#### functions ####

to_regex <- function(x) {
  return(paste0('(', paste(x, collapse = ')|('), ')'))
}

sort_unique <- function(x) {
  y <- unique(x)
  y[order(y)] }

sort_unique_by <- function(x,by) {
  y <- unique(x)
  id <- order(ordered(y, levels=by))
  as.character(y[id]) }

get_coded_vars <- function(rawVideoData) {
  rawVideoData$Description <- tolower(rawVideoData$Description) # make all lower case
  rawVideoData$Performance[rawVideoData$correct==0] <- 'incorrect'
  rawVideoData$Performance[rawVideoData$correct==1] <- 'correct'
  rawVideoData$Performance[rawVideoData$response=='open'] <- 'open'
  
  locationOrder <- c('hand','wrist','lower','upper')
  locations <- to_regex(locationOrder)
  intensities <- '(light)|(medium)|(moderate)|(strong)'
  touchwith <- '(hand)|(thumb)|(fingers)|(finger)'
  
  touchLevels <- c('holding','squeezing','tapping','patting','lifting','poking','hitting','pressing','stroking','hugging','interlocking','massaging','swinging','shaking','rubbing','pinching','pushing','pulling','trembling','scratching')
  
  
  rawVideoData$Desc.Tch <- ''
  rawVideoData$Desc.Rec <- ''
  for (i in 1:nrow(rawVideoData)) {
    # split for description of the toucher's contact point and the receiver's
    if ( grepl('/ ',rawVideoData$Description[i]) ) { 
      trSplit <- str_split(rawVideoData$Description[i],'/ ')[[1]]
      rawVideoData$Desc.Tch[i] <- trSplit[1]
      rawVideoData$Desc.Rec[i] <- trSplit[2]
    } else { # sometimes only the receiver's contact point was recorded
      rawVideoData$Desc.Tch[i] <- ''
      rawVideoData$Desc.Rec[i] <- rawVideoData$Description[i]
    }
    
    #extract location from description
    ext.loc <- str_extract_all(rawVideoData$Desc.Rec[i], locations)[[1]]
    rawVideoData$location[i] <- paste(sort_unique_by(ext.loc, locationOrder), collapse = ' & ')
    
    #extract info about which part of the skin the toucher touched with
    ext.tw <- str_extract_all(rawVideoData$Desc.Tch[i], touchwith)[[1]]
    rawVideoData$touchWith[i] <- paste(sort_unique(ext.tw), collapse = ' & ')
    
    #extract intensity
    ext.int <- str_extract_all(rawVideoData$Description[i], intensities)[[1]]
    rawVideoData$intensity[i] <- paste(sort_unique(ext.int), collapse = ' & ')
    if (rawVideoData$intensity[i] == 'medium') rawVideoData$intensity[i] <- 'moderate'
    
    #extract touch descriptors
    ext.tch <- str_extract_all(rawVideoData$Description[i], to_regex(touchLevels))[[1]]
    rawVideoData$touch[i] <- paste(sort_unique(ext.tch), collapse = ' & ')
  }  
  
  # validation, check for and remove missing/uncategorised data etc.
  
  nRowsBefore <- nrow(rawVideoData)
  
  # print(rawVideoData$Description[rawVideoData$intensity==''])
  rawVideoData <- rawVideoData[rawVideoData$intensity!='',]
  nRowsIntensity <- nrow(rawVideoData)
  print(paste(nRowsBefore - nRowsIntensity, 'rows of', nRowsBefore, 
              'removed due to missing intensity (', 
              100*(nRowsBefore - nRowsIntensity)/nRowsBefore, '%).'))
  
  # print(rawVideoData$Description[rawVideoData$touch==''])
  rawVideoData <- rawVideoData[rawVideoData$touch!='',]
  nRowsTouch <- nrow(rawVideoData)
  print(paste(nRowsIntensity - nRowsTouch, 'rows of', nRowsBefore, 
              'removed due to missing touch (', 
              100*(nRowsIntensity - nRowsTouch)/nRowsBefore, '%).'))
  
  # print(rawVideoData$Description[rawVideoData$location==''])
  rawVideoData <- rawVideoData[rawVideoData$location!='',]
  nRowsLocation <- nrow(rawVideoData)
  print(paste(nRowsTouch - nRowsLocation, 'rows of', nRowsBefore, 
              'removed due to missing location (', 
              100*(nRowsTouch - nRowsLocation)/nRowsBefore, '%).'))
  
  # simplify locations 
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & wrist & lower & upper'] <- 'arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & wrist & lower'] <- 'hand and arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & wrist & upper'] <- 'hand and arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & lower & upper'] <- 'hand and arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'wrist & lower & upper'] <- 'arm'
  
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & wrist'] <- 'hand'
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & lower'] <- 'hand and arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'hand & upper'] <- 'hand and arm'
  rawVideoData$simpleLocation[rawVideoData$location == 'wrist & lower'] <- 'lower'
  rawVideoData$simpleLocation[rawVideoData$location == 'wrist & upper'] <- 'upper'
  rawVideoData$simpleLocation[rawVideoData$location == 'lower & upper'] <- 'arm'
  
  rawVideoData$simpleLocation[rawVideoData$location == 'hand'] <- 'hand'
  rawVideoData$simpleLocation[rawVideoData$location == 'wrist'] <- 'lower'
  rawVideoData$simpleLocation[rawVideoData$location == 'lower'] <- 'lower'
  rawVideoData$simpleLocation[rawVideoData$location == 'upper'] <- 'upper'
  
  rawVideoData <- rawVideoData[rawVideoData$simpleLocation!='',]
  nRowsAfter <- nrow(rawVideoData)
  print(paste(nRowsLocation - nRowsAfter, 'rows of', nRowsBefore, 
              'removed due to missing simple location (', 
              100*(nRowsLocation - nRowsAfter)/nRowsBefore, '%).'))
  
  print(paste(nRowsBefore - nRowsAfter, 'rows of', nRowsBefore, 
              'removed due to missing data (', 
              100*(nRowsBefore - nRowsAfter)/nRowsBefore, '%).'))
  
  
  return(rawVideoData)
}

#### read in data ####
video.data <- read_csv('data/processed/expt1_video-01_combined.csv')

#### apply coding scheme to annotations #### 
video.data.coded <- get_coded_vars(video.data) 

write_csv(video.data.coded, 'data/processed/expt1_video-02_processed.csv')
