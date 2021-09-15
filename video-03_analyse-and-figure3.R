library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(svglite)
source('comm_source.R')

#### functions ####

to_sentence_case <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#### read in data ####

videoData <- read_csv('data/processed/expt1_video-02_processed.csv')

# change default plot appearance
theme_set(theme_classic())

videoCueDuration <- videoData %>%
  group_by(pair, SessionNo, cued, trial, Performance) %>%
  summarise(totDuration = sum(Duration.sec))
videoCueDuration$Cued <- factor(toupper(videoCueDuration$cued), levels = toupper(orderedCues))

videoCueDurationSummary <- videoCueDuration %>%
  group_by(cued) %>%
  summarise(meanDuration = mean(totDuration), 
            sdDuration = sd(totDuration))

videoData %>% 
  group_by(cued,toucher, receiver, trial) %>% 
  tally() %>% tally() %>% group_by(cued) %>% 
  summarise(nTrials = sum(n))

### FIGURE 3 ####
# - colours were later adjusted in Adobe Illustrator to be more friendly to colour-blindness
# - y-axis labels were replaced in Adobe Illustrator with images showing arm location

###. duration density plot - top part of Figure 3 ####
videoCueDuration %>% 
  filter(Performance != 'open') %>%
  mutate(Performance = factor(to_sentence_case(Performance), levels = c('Incorrect', 'Correct'))) %>% 
  ggplot(aes(x = totDuration, fill=Performance)) +
  facet_grid(.~cued) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  scale_y_continuous(breaks = c()) +
  theme_nofacetbox + theme(strip.text.x= element_blank()) +
  labs(x=NULL, y = NULL, fill = NULL) -> density.plot

videoCombo <- videoData %>%
  filter(Performance != 'open') %>%
  mutate(simpleLocation = factor(simpleLocation, levels = c('hand and arm', 'hand', 'lower', 'upper', 'arm'))) %>% 
  group_by(Performance, cued, simpleLocation, intensity, touch) %>%
  summarise(freqTime = sum(Duration.sec)) %>%
  group_by(cued, Performance) %>%
  mutate(totFreqTime = sum(freqTime), wtFreqTime = 100*freqTime/sum(freqTime)) 

###. check distribution ####
videoCombo %>%
  ggplot(aes(x = wtFreqTime)) +
  geom_histogram(binwidth = 5)

###. video features ####

videoCombo %>% 
  filter(wtFreqTime > 5) %>%
  ungroup() %>% 
  complete(Performance, nesting(cued, touch, simpleLocation, intensity), fill = list(wtFreqTime = 0)) %>% 
  group_by(cued, touch, simpleLocation, intensity) %>% 
  mutate(time.pc.diff = wtFreqTime[1] - wtFreqTime[2]) %>% #positive means more represented in correct responses
  filter( (time.pc.diff > 0 & Performance == 'correct') | (time.pc.diff < 0 & Performance == 'incorrect') ) %>%  
  group_by(cued) %>% 
  mutate(best = rank(-time.pc.diff), worst = rank(time.pc.diff)) %>% 
  filter((best < 5 & Performance == 'correct') | (worst < 5 & Performance == 'incorrect')) %>% 
  arrange(cued, best, worst) -> videoBestWorst

videoBestWorst %>% View()


###. main part of Figure 3 ####
videoBestWorst %>%
  ggplot(aes(x = touch, y = simpleLocation)) +
  facet_grid(to_sentence_case(Performance) ~ toupper(cued), scales = 'free_x') +
  geom_point(aes(colour = intensity, size = abs(time.pc.diff)), alpha = 0.7) +
  scale_x_discrete(drop = TRUE) + 
  scale_size_area(max_size = 10) +
  scale_colour_manual(values = c("#377EB8","#984EA3","#E41A1C")) +
  theme_light() +
  theme_x45deg + theme_bwstrip + theme(panel.grid = element_line(size = 2)) +
  labs(size = 'Difference in\n% of time', shape = 'Touched on', colour = 'Intensity', y = NULL, x = NULL) -> bestworst.plot


quartz(width = 8.7, height = 4.4); plot(1:10)
density.plot / bestworst.plot + plot_layout(ncol = 1, heights = c(0.1,1))
ggsave('../Figures/video gestures.svg')
