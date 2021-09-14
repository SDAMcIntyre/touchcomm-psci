library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(svglite)

#### functions ####
clean_bins <- function(df, prefixes, pfkeep = 0) {
  # sets all data to NA in bins with more than pfkeep proportion of flagged data
  # set pfkeep to 0 to NA whole bins with any flagged samples
  for (muscleName in prefixes) {
    name.flagged <- paste0(muscleName, '.flagged')
    name.rawfixed <- paste0(muscleName, '.fixed')
    name.zfixed <- paste0(muscleName, '.zfixed')
    df <- df %>% 
      mutate(!!name.rawfixed := if_else(.[[name.flagged]] > pfkeep, 
                                        as.numeric(NA), 
                                        .[[name.rawfixed]])) %>% 
      mutate(!!name.zfixed := if_else(.[[name.flagged]] > pfkeep, 
                                      as.numeric(NA), 
                                      .[[name.zfixed]]))
  }
  return(df)
}

myboot <- function(x) { mean_cl_boot(x, B=1000) } 

time_plot <- function(df, muscle) {
  df %>% 
    ggplot(aes(x = time, y = {{muscle}}, 
               colour = cued, fill = cued, linetype = cued, shape = cued)) +
    geom_vline(xintercept = 0) +
    stat_summary(fun.data = 'myboot', geom = 'ribbon', alpha = 0.3) +
    stat_summary(fun = 'mean', geom = 'line') +
    stat_summary(fun = 'mean', geom = 'point') +
    scale_x_continuous(
      breaks = seq(min(df$time),max(df$time),1), expand = c(0.01,0.01)
      ) +
    coord_cartesian(ylim = c(-0.6,1.1)) +
    theme_bw() +
    labs(x = 'Time (seconds)', y = 'Muscle Activity (z)', 
         colour = NULL, fill = NULL, shape = NULL, linetype = NULL)
}

#### main ####

## use .RData file in case of memory problems
load(file = 'data/processed/expt1_femg-02_artifacts-labelled.RData')
femg.data <- read_csv('data/processed/expt1_femg-02_artifacts-labelled.csv',
                      col_types = 'ddiicddddccccccicddlddddlddddlddddldd')
prefixes <- c('t.zyg', 't.cor', 'r.zyg', 'r.cor')

# 100 ms windows
bin.sec <- 0.1

femg.binned <- femg.data %>% 
  mutate(
    bin.n = cut(stimTime.sec, 
                breaks = seq(min(stimTime.sec), max(stimTime.sec), by = bin.sec), 
                labels = FALSE),
    time = (bin.n-1) * bin.sec + min(stimTime.sec) 
  ) %>% 
  group_by(session,cued,trialNo,phase,time) %>%
  summarise(
    across(.cols = starts_with(prefixes), 
           .fns = ~mean(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  do(clean_bins(., prefixes)) 

femg.binned %>% 
  write_csv('data/processed/expt1_femg-03_binned-100ms-clean.csv')

# where to cut off the time plot
cutoffs <- femg.binned %>% 
  group_by(session,cued,trialNo) %>% 
  summarise(duration = max(time, na.rm = TRUE)) %>% 
  group_by(cued) %>% 
  # ggplot(aes(x = duration)) +facet_wrap(.~cued, scales = 'free') +geom_histogram()
  summarise(cut.time = quantile(duration, 0.75, na.rm = TRUE))

femg.to.plot <- femg.binned %>% 
  left_join(cutoffs, by = 'cued') %>% 
  filter(time <= cut.time)

#### plot ####

t.zyg.plot <- femg.to.plot %>% time_plot(t.zyg.zfixed) + 
  labs(x = NULL, title = 'A. Sender Zygomaticus')
r.zyg.plot <- femg.to.plot %>% time_plot(r.zyg.zfixed) + 
  labs(x = NULL, title = 'B. Receiver Zygomaticus')
t.cor.plot <- femg.to.plot %>% time_plot(t.cor.zfixed) + 
  labs(x = NULL, title = 'C. Sender Corrugator')
r.cor.plot <- femg.to.plot %>% time_plot(r.cor.zfixed) +
  labs(title = 'D. Receiver Corrugator')

quartz(width = 7.8, height = 10.2); plot(1:10) 

design <- "
A
B
B
B
B
C
C
C
C
D
D
D
D
E
E
E
E
"
guide_area() + 
  t.zyg.plot + 
  r.zyg.plot + 
  t.cor.plot + 
  r.cor.plot +
  plot_layout(design = design, guides = 'collect') & 
  theme(legend.position = 'top') & 
  guides(colour = guide_legend(nrow = 1))
ggsave(paste0('figures/FigS2_femg_binned-',round(bin.sec*1000),'ms-windows.png'))
ggsave(paste0('figures/FigS2_femg_binned-',round(bin.sec*1000),'ms-windows.svg'))
ggsave(paste0('figures/FigS2_femg_binned-',round(bin.sec*1000),'ms-windows.pdf'))

