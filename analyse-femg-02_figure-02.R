library(tidyverse)
# library(broom)
library(afex)
library(emmeans)
library(patchwork)
# library(svglite)
# #library(pwr)
# library(RcppRoll)
# source('analysis source.R')

#### functions ####
z_mean_diff <- function(.data) {
  .data %>% 
    group_by(session,cue,trial,phase) %>% 
    summarise_at(vars(ends_with('norm')), mean) %>% 
    rename(z.mean = ends_with('norm')) %>% 
    ungroup() %>% 
    complete(phase, nesting(session,cue,trial)) %>% 
    group_by(session,cue,trial) %>% 
    summarise(z.mean.diff = diff(z.mean)) %>% 
    rename(cued = cue) %>% 
    ungroup()
}
####

#### main ####

# read in facial EMG data, after arteface rejection (Matlab script)

femg.zyg.t.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_tzyg_0to4sec_24Mar2021.csv',
  col_types = cols(trial = col_integer()) )
femg.zyg.r.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_rzyg_0to4sec_24Mar2021.csv', 
  col_types = cols(trial = col_integer()) )
femg.cor.t.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_tcor_0to4sec_24Mar2021.csv',
  col_types = cols(trial = col_integer()) )
femg.cor.r.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_rcor_0to4sec_24Mar2021.csv', 
  col_types = cols(trial = col_integer()) )

orderedCues <- c('attention', 'love', 'happiness', 'calming', 'sadness', 'gratitude')

##### mean z and mean z difference (stim - baseline) #####

femg.zyg.t <-  femg.zyg.t.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) 
femg.zyg.t %>% ungroup() %>% tally()
femg.zyg.t %>% group_by(cued) %>% tally()

femg.zyg.r <- femg.zyg.r.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues))
femg.zyg.r %>% ungroup() %>% tally()
femg.zyg.r %>% group_by(cued) %>% tally()

femg.cor.t <- femg.cor.t.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) %>% 
  filter(!is.na(z.mean.diff)) # T87_R44 trial 14 is missing baseline
femg.cor.t %>% ungroup() %>% tally()
femg.cor.t %>% group_by(cued) %>% tally()

femg.cor.r <- femg.cor.r.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues))
femg.cor.r %>% ungroup() %>% tally()
femg.cor.r %>% group_by(cued) %>% tally()

# communication data
comm.femg <- read_csv('data/expt1_communication_data.csv', col_types = 'cccicccici')

# read in ml confusion matrix data
ml.confusion <- read_ml_matrix('../Data/expt1_fEMG_ml_trials.txt')

# read in ml performance on specific trials
ml.rows <- read_ml_rows_file('../Data/expt1_fEMG_ml_trials.txt', comm.rows = 1:nrow(comm.femg))
sum(ml.rows$rowN != seq_along(ml.rows$rowN)) # check there are no duplicate row numbers
comm.femg <- bind_cols(comm.femg, ml.rows %>% select(-rowN) )

ml.confusion <- ml.confusion %>%
  full_join(comm.femg %>% 
              filter(!is.na(ml.toucher.correct)) %>%
              group_by(cued) %>% 
              summarise(Total = n())
  ) %>% 
  mutate(respFreq = as.integer(Total*(Percent/100)) )


#compare numbers
joinby <- c('session', 'trial', 'cued') # one 'session' = one participant in one role (and partner in the other)
inner_join(femg.zyg.r, femg.cor.r, suffix = c('.z','.c'),
           by = joinby) %>% tally() 
comm.femg %>% filter(!is.na(ml.receiver.correct)) %>% tally()

inner_join(femg.zyg.t, femg.cor.t, suffix = c('.z','.c'),
           by = joinby) %>% tally()
comm.femg %>% filter(!is.na(ml.toucher.correct)) %>% tally()


#### mixed effects models - in caption of Figure 2A ####
set_sum_contrasts()

# ZYG
glmZyg.toucher <- mixed(z.mean.diff ~ cued + (1|session), 
                        data = femg.zyg.t, 
                        control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))

anova(glmZyg.toucher)
emmeans(glmZyg.toucher, ~ 1, infer = TRUE) # intercept = overall activity compared to baseline
(zyg.t.CIs <- emmeans(glmZyg.toucher, ~ cued) %>% as_tibble() %>% mutate(role = 'Toucher'))

glmZyg.receiver <- mixed(z.mean.diff ~ cued + (1|session),
                         data = femg.zyg.r, 
                         control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmZyg.receiver)
anova(glmZyg.receiver)
emmeans(glmZyg.receiver, ~ 1, infer = TRUE)
(zyg.r.CIs <- emmeans(glmZyg.receiver, ~ cued) %>% as_tibble() %>% mutate(role = 'Receiver'))

# COR
glmCor.toucher <- mixed(z.mean.diff ~ cued +(1|session),
                        data = femg.cor.t, 
                        control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmCor.toucher)
anova(glmCor.toucher)
emmeans(glmCor.toucher, ~ 1, infer = TRUE)
(cor.t.CIs <- emmeans(glmCor.toucher, ~ cued) %>% as_tibble() %>% mutate(role = 'Toucher'))

glmCor.receiver <- mixed(z.mean.diff ~ cued + (1|session),
                         data = femg.cor.r, 
                         control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmCor.receiver)
anova(glmCor.receiver)
emmeans(glmCor.receiver, ~ 1, infer = TRUE)
(cor.r.CIs <- emmeans(glmCor.receiver, ~ cued) %>% as_tibble() %>% mutate(role = 'Receiver'))

#### FIGURE 2 - Facial EMG summary plots ####

theme_set(theme_light()) 

femg_plot <- function(df, CIs, role.colour) {
  df %>% 
    group_by(session,cued) %>% 
    summarise(z.mean.diff = mean(z.mean.diff)) %>%  # average over trials
    ggplot(aes(x=cued)) +
    facet_grid(.~role) +
    geom_dotplot(aes(y=z.mean.diff),  
                 colour = role.colour, alpha = 0.7,
                 binaxis = "y", stackdir = "center", fill='white',dotsize = 3, binwidth = 0.1) +
    geom_hline(yintercept = 0) +
    geom_crossbar(data = CIs, 
                  aes(y = emmean, ymin = lower.CL, ymax = upper.CL), 
                  colour = role.colour, width = 0.7, fill = 'white') +
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
    # scale_y_continuous(limits = c(-2,2)) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1), 
                       limits = c(-2,2), breaks = c(-2,-1,-0.5,0,0.5,1,2),
                       labels = breaks = c(-2,-1,-0.5,0,0.5,1,2)) + 
    theme_x45deg + theme_nofacetbox + theme(legend.position = 'none') 
}

theme_x45deg <- theme(
  axis.text.x=element_text(angle=45, 
                           vjust = grid::unit(-1, "points"), 
                           hjust = grid::unit(1, 'points') ))

theme_nofacetbox <- theme(strip.background = element_blank(), 
                          strip.text = element_text(colour = 'black'))


# colour.intuitive <-  '#377EB8' #blue
# colour.expert <- '#4DAF4A' # green
colour.receiver <- "#E69F00" #yellow
colour.toucher <- '#984ea3' #purple 
# colour.swapped <- '#ff00b3' #pink

# Figure 2A (left)
## Zygomaticus toucher
femg.zyg.t %>% 
  mutate(role = 'Sender') %>% 
  femg_plot(zyg.t.CIs, colour.toucher) +
  labs(x= NULL) -> zyg.t.plot

## Zygomaticus receiver
femg.zyg.r %>% 
  mutate(role = 'Receiver') %>% 
  femg_plot(zyg.r.CIs, colour.receiver) +
  labs(x= NULL, y= NULL) -> zyg.r.plot

zyg.t.plot + zyg.r.plot # -2, >1

# Figure 2A (right)
## Corrugator toucher
femg.cor.t %>% 
  mutate(role = 'Sender') %>% 
  femg_plot(cor.t.CIs, colour.toucher) +
  labs(x= NULL) -> cor.t.plot 

## Corrugator receiver
femg.cor.r %>% 
  mutate(role = 'Receiver') %>% 
  femg_plot(cor.r.CIs, colour.receiver) +
  labs(title = ' ',
       x=NULL, y=NULL) -> cor.r.plot

cor.t.plot + cor.r.plot # -2, > 1

quartz(width = 8.5, height = 2.9); plot(1:10)

layout.fig2.top <- '
ABCD
'
wrap_plots(A = zyg.t.plot + labs(title = 'Zygomaticus', 
                                 y= 'Muscle Activity - Baseline (z)'), 
           B = zyg.r.plot, 
           C = cor.t.plot + labs(title = 'Corrugator', 
                                 y= 'Muscle Activity - Baseline (z)'), 
           D = cor.r.plot, 
           design = layout.fig2.top) 

ggsave('figures/femg_0to4sec.png')

# receiver performance confusion matrix

comm.t.compare <- comm.femg %>%
  filter(response != 'open' & !is.na(ml.toucher.correct))  %>%
  mutate(cued = factor(cued, levels = orderedCues),
         response = factor(response, levels = rev(c(orderedCues,'other')))) 

# Figure 2B
comm.t.compare %>%
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.intuitive, grad.limit = c(0,100), ylabels = c(orderedCues,'other')) +
  labs(title = 'B. Receiver performance', x = 'Cued word', y = 'Receiver response') -> rec.matrix
rec.matrix

# # overall performance - in caption of Figure 2B
# comm.forced.choice %>%
#   summarise(nCorrect = sum(correct), nTotal = n(), pCorrect = 100*mean(correct),
#             nCues = length(unique(cued)), chance = 1/nCues[1], alpha = 0.05/nCues[1]) %>%
#   mutate(result = prop.test(x = nCorrect, n = nTotal, p = chance, conf.level = 1-alpha) %>% 
#            tidy() %>% list()) %>%
#   unnest(result) %>% 
#   mutate(effect.size = ES.h(p1 = estimate, p2 = chance)) %>% glimpse()


# classifier confusion matrix for toucher

# Figure 2C
ml.confusion %>%
  mutate(role = 'Toucher') %>% 
  confusion_matrix_plot(colour.toucher, grad.limit = c(10,50), ylabels = orderedCues) +
  facet_grid(.~role) +
  labs(title = 'C. Classifier performance', x = 'Cued word', y = 'Classified') -> ml.t.matrix

ml.t.matrix

# correlation

# check n
comm.femg %>% 
  group_by(receiver) %>% 
  summarise(n = sum(!is.na(ml.toucher.correct))) %>%  #ggplot(aes(x = n)) + geom_histogram(binwidth = 5, closed = 'left') + scale_y_continuous(breaks = 0:12)
  filter(n >= 10) %>% # keep receivers with at least 10 trials of ml data
  pull(receiver) -> keepReceivers

ml.by.pair <- comm.femg %>%
  filter(response != 'open' & !is.na(ml.toucher.correct) & receiver %in% keepReceivers) %>%
  group_by(receiver) %>% 
  summarise(receiver.pc = mean(correct)*100,
            toucher.classifier.pc = mean(ml.toucher.correct, na.rm = TRUE)*100,
            receiver.n = sum(!is.na(correct)),
            toucher.classifier.n = sum(!is.na(ml.toucher.correct))) 

(ml.corr.t <- cor.test(ml.by.pair$toucher.classifier.pc, ml.by.pair$receiver.pc, alternative = 'greater'))
ml.corr <- tibble(toucher.r = paste('r =',round(ml.corr.t$estimate,3)),
                  toucher.p = paste('p =',round(ml.corr.t$p.value,3)))

# Figure 2D
ml.by.pair %>%
  ggplot() +
  geom_point(aes(toucher.classifier.pc, receiver.pc), 
             shape = 1, size = 2, colour = colour.toucher) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,100)) +
  labs(title = 'D. Correlation', 
       y = 'Receiver hit rate (%)', x = 'Classifier hit rate (%)') +
  annotate(geom = 'text', x = 48, y = 90, colour = colour.toucher,
           label = paste(ml.corr$toucher.r,'\n', ml.corr$toucher.p) ) +
  theme_classic() -> correlation.plot
correlation.plot

# Figure 2 

windows(8.5, 5.7)

layout.fig2.top <- '
ABCD
'
layout.fig2.bottom <- '
EFG
'

wrap_plots(A = zyg.t.plot + labs(title = 'A. Facial muscle activity\nZygomaticus (smiling)', 
                                 y= 'Muscle Activity - Baseline (z)'), 
           B = zyg.r.plot, 
           C = cor.t.plot + labs(title = 'Corrugator (frowning)', 
                                 y= 'Muscle Activity - Baseline (z)'), 
           D = cor.r.plot, 
           design = layout.fig2.top) / 
  wrap_plots(E = rec.matrix, F = ml.t.matrix, G = correlation.plot, 
             design = layout.fig2.bottom) 


ggsave('../Figures/Figure 2 femg combined.svg')
