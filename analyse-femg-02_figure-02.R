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
  max.n.bins <- femg.zyg.t.samples %>% 
    group_by(session,cue,trial) %>% 
    tally() %>% ungroup() %>% pull(n) %>% max()
  .data %>% 
  # femg.zyg.t.samples %>% 
    group_by(session,cue,trial,phase) %>% 
    summarise(across(.cols = ends_with('norm'), .fns = mean),
              n = n()) %>% 
    # summarise_at(vars(ends_with('norm')), mean) %>% 
    rename(z.mean = ends_with('norm')) %>%
    ungroup() %>%
    complete(phase, nesting(session,cue,trial)) %>%
    mutate(n = replace_na(n,0)) %>% 
    group_by(session,cue,trial) %>%
    summarise(z.mean.diff = diff(z.mean),
              n.bins = sum(n),
              total.n.bins = max.n.bins,
              p.bins = sum(n)/max.n.bins) %>%
    rename(cued = cue) %>%
    ungroup()
}

read_ml_matrix <- function(file, skiplines) {
  
  ml.mat.names <- read_lines(file, skip = skiplines+6, n_max = 1) %>% 
    str_extract_all('[:alpha:]+') %>% first()
  
  read_lines(file, skip = skiplines, n_max = 6) %>%
    str_extract_all('[0-9]+\\.[0-9]*', simplify = TRUE) %>% t() %>% 
    as_tibble() %>% setNames(ml.mat.names) %>% 
    mutate_all(as.numeric) %>% 
    mutate(response = ml.mat.names) %>% 
    gather('cued', 'Percent', -response)
}

read_ml_rows_file <- function(file, comm.rows) {
  
  ml.toucher <- tibble(rowN = read_ml_lines(file, blockN = 12),
                       ml.toucher.correct = 1) %>% 
    rbind(tibble(rowN = read_ml_lines(file, blockN = 14),
                 ml.toucher.correct = 0)) 
  
  ml.receiver <- tibble(rowN = read_ml_lines(file, blockN = 22),
                        ml.receiver.correct = 1) %>% 
    rbind(tibble(rowN = read_ml_lines(file, blockN = 24),
                 ml.receiver.correct = 0)) 
  ml.rows <- ml.toucher %>% 
    full_join(ml.receiver, by = 'rowN') %>% 
    full_join(tibble(rowN = comm.rows), by = 'rowN') %>% 
    arrange(rowN)
  
  return(ml.rows)
}

read_ml_lines <- function(blockN, ...) {
  read_lines(...) %>% 
    paste(collapse = ' ') %>% 
    str_split('\\[|\\]') %>% unlist() %>% nth(blockN) %>%
    str_extract_all('[0-9]+') %>% first() %>% 
    as.numeric() -1
}

tally_by <- function(.data, ...) {
  byVars <- enquos(..., .named = TRUE)
  xformula <- reformulate(termlabels = c(names(byVars)))
  .data %>%
    xtabs(formula = xformula) %>%
    as_tibble() %>%
    arrange(...)
}

confusion_matrix_data <- function(.data, ...) {
  if (!all(c("cued", "response") %in% names(.data))) {
    stop("`data` must contain `cued` and `response` columns")
  }
  .data %>% 
    tally_by(..., cued, response) %>%
    rename('respFreq' = 'n') %>%
    group_by(...,cued) %>%
    mutate(cuedFreq = sum(respFreq),
           Percent = 100*respFreq/cuedFreq) %>%
    ungroup()
}

order_PIDs <- function(.data, PID) {
  .data %>%
    confusion_matrix_data({{PID}}) %>% 
    filter(cued == response) %>% 
    group_by({{PID}}) %>% 
    summarise(performance = mean(Percent)) %>% 
    arrange(-performance) %>% 
    pull({{PID}})
}

round_integer <- function(x) trunc(x+sign(x)*0.5)

confusion_matrix_plot <- function(df, grad.colour, grad.limit = c(0,100), ylabels, abr = 3) {
  df %>%
    mutate(cued = factor(cued, levels = orderedCues),
           response = factor(response, levels = rev(ylabels))) %>%
    ggplot(aes(x=cued, y=response, fill=Percent)) +
    geom_tile(color="black",size=0.1) +
    geom_text(aes(label=round_integer(respFreq)), size=3, colour="black") +
    scale_fill_gradient(name =' %', na.value = 'white', low='white', high=grad.colour,
                        guide = 'legend', limits = grad.limit) +
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),abr,'right','')) +
    scale_y_discrete(label = str_trunc(str_to_title(rev(ylabels)),abr,'right','')) +
    theme_classic() + theme_x45deg + theme_confmat_legend +
    theme_nofacetbox + theme(axis.line = element_blank()) +
    labs(x = 'Cued word', y = 'Receiver response')
}

####

#### plot theme customisations ####
theme_set(theme_light()) 

theme_x45deg <- theme(axis.text.x=element_text(angle=45, hjust = 1))

theme_nofacetbox <- theme(strip.background = element_blank(), 
                          strip.text = element_text(colour = 'black'))

theme_biggerfonts <- theme(
  axis.title.x=element_text(size=14), 
  axis.title.y=element_text(size=14,angle=90, margin = margin(t = 0, r = 5, b = 0, l = 15)), 
  axis.text.x=element_text(size=12), 
  axis.text.y=element_text(size=12), 
  strip.text.x=element_text(size=14), 
  legend.text=element_text(size=12), 
  legend.title=element_text(size=14))

colour.intuitive <-  '#377EB8' #blue
# colour.expert <- '#4DAF4A' # green
colour.receiver <- "#E69F00" #yellow
colour.toucher <- '#984ea3' #purple 
# colour.swapped <- '#ff00b3' #pink

theme_confmat_legend <- theme(legend.margin=margin(0,0,0,0),
                              legend.box.margin=margin(t = 0, r = 0, b = 0,l = -10), 
                              legend.text=element_text(size=8))
####

#### main ####

# read in facial EMG data, after arteface rejection 

femg.zyg.t.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_tzyg_0to4sec_100ms-binned_25Mar2021.csv',
  col_types = cols(trial = col_integer()) )
femg.zyg.r.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_rzyg_0to4sec_100ms-binned_25Mar2021.csv', 
  col_types = cols(trial = col_integer()) )
femg.cor.t.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_tcor_0to4sec_100ms-binned_25Mar2021.csv',
  col_types = cols(trial = col_integer()) )
femg.cor.r.samples <- read_csv(
  'data/expt1_femg-03_cleaned-time-selected/fEMG_clean_rcor_0to4sec_100ms-binned_25Mar2021.csv', 
  col_types = cols(trial = col_integer()) )

orderedCues <- c('attention', 'love', 'happiness', 'calming', 'sadness', 'gratitude')

##### mean z and mean z difference (stim - baseline) #####

femg.zyg.t <-  femg.zyg.t.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) 
 
femg.zyg.r <- femg.zyg.r.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) #%>% 

femg.cor.t <- femg.cor.t.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) #%>% 

femg.cor.r <- femg.cor.r.samples %>% 
  z_mean_diff() %>% # z.mean.diff = stimulus mean z - baseline mean z
  mutate(cued = factor(cued, levels = orderedCues)) #%>% 

##### communication data ####
comm.femg <- read_csv('data/expt1_communication_data.csv', col_types = 'cccicccici') %>% 
  mutate(
    session = paste(
      str_replace(toucher,'P','T'),
      str_replace(receiver, 'P','R'), sep = '_')
  ) %>% 
  select(c('session', 'trial','cued', 'response', 'correct'))

##### read in ml confusion matrix data ####
ml.t.confusion <- read_ml_matrix('data/ML_predictions_output.txt', 1)
ml.r.confusion <- read_ml_matrix('data/ML_predictions_output.txt', 9)

# read in ml performance on specific trials
ml.rows <- read_ml_rows_file('data/ML_predictions_output.txt', comm.rows = 1:nrow(comm.femg))
# ml.rows[!complete.cases(ml.rows),]
sum(ml.rows$rowN != seq_along(ml.rows$rowN)) # check there are no duplicate row numbers

#### combine comm data with ml output ####
comm.ml.femg <- bind_cols(comm.femg, ml.rows  ) # %>% select(-rowN)

##### toucher compare numbers #####
joinby <- c('session', 'trial', 'cued') # one 'session' = one participant in one role (and partner in the other)
comm.ml.femg.t <- inner_join(femg.zyg.t, femg.cor.t, 
                             suffix = c('.z','.c'),
                             by = joinby) %>% 
  full_join(comm.ml.femg) 

# any ml data where there shouldn't be?
comm.ml.femg.t %>% 
  filter( ( is.na(z.mean.diff.z) | is.na(z.mean.diff.c) ) & !is.na(ml.toucher.correct))

# any missing ml data where there should be?
comm.ml.femg.t %>% 
  filter( !is.na(z.mean.diff.z) & !is.na(z.mean.diff.c)  & is.na(ml.toucher.correct))

##### receiver compare numbers #####

comm.ml.femg.r <- inner_join(femg.zyg.r, femg.cor.r, 
                             suffix = c('.z','.c'),
                             by = joinby) %>% 
  full_join(comm.ml.femg) 

# any ml data where there shouldn't be?
comm.ml.femg.r %>% 
  filter( ( is.na(z.mean.diff.z) | is.na(z.mean.diff.c) ) & !is.na(ml.receiver.correct))

# any missing ml data where there should be?
comm.ml.femg.r %>% 
  filter( !is.na(z.mean.diff.z) & !is.na(z.mean.diff.c)  & is.na(ml.receiver.correct))


#### mixed effects models - in caption of Figure 2A ####
set_sum_contrasts()

# ZYG

femg.zyg.t.data <- femg.zyg.t %>% 
  filter(!is.na(z.mean.diff)) %>% # 955
  filter(n.bins >= 10) # 939
femg.zyg.t.data %>% tally()
femg.zyg.t.data %>% group_by(cued) %>% tally()

glmZyg.toucher <- mixed(z.mean.diff ~ cued + (1|session), 
                        data = femg.zyg.t.data, 
                        control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))

anova(glmZyg.toucher)
emmeans(glmZyg.toucher, ~ 1, infer = TRUE) # intercept = overall activity compared to baseline
(zyg.t.CIs <- emmeans(glmZyg.toucher, ~ cued) %>% as_tibble() %>% mutate(role = 'Toucher'))

# receiver 
femg.zyg.r.data <- femg.zyg.r %>% 
  filter(!is.na(z.mean.diff)) %>% # 962
  filter(n.bins >= 10) # 954
femg.zyg.r.data %>% tally()
femg.zyg.r.data %>% group_by(cued) %>% tally()

glmZyg.receiver <- mixed(z.mean.diff ~ cued + (1|session),
                         data = femg.zyg.r.data, 
                         control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmZyg.receiver)
anova(glmZyg.receiver)
emmeans(glmZyg.receiver, ~ 1, infer = TRUE)
(zyg.r.CIs <- emmeans(glmZyg.receiver, ~ cued) %>% as_tibble() %>% mutate(role = 'Receiver'))

# COR

femg.cor.t.data <- femg.cor.t %>% 
  filter(!is.na(z.mean.diff)) %>% # 937
  filter(n.bins >= 10) # 924
femg.cor.t.data %>% tally()
femg.cor.t.data %>% group_by(cued) %>% tally()

glmCor.toucher <- mixed(z.mean.diff ~ cued +(1|session),
                        data = femg.cor.t.data, 
                        control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmCor.toucher)
anova(glmCor.toucher)
emmeans(glmCor.toucher, ~ 1, infer = TRUE)
(cor.t.CIs <- emmeans(glmCor.toucher, ~ cued) %>% as_tibble() %>% mutate(role = 'Toucher'))

# receiver 
femg.cor.r.data <- femg.cor.r %>% 
  filter(!is.na(z.mean.diff)) %>% # 940
  filter(n.bins >= 10) # 935
femg.cor.r.data %>% tally()
femg.cor.r.data %>% group_by(cued) %>% tally()

glmCor.receiver <- mixed(z.mean.diff ~ cued + (1|session),
                         data = femg.cor.r.data, 
                         control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)))
summary(glmCor.receiver)
anova(glmCor.receiver)
emmeans(glmCor.receiver, ~ 1, infer = TRUE)
(cor.r.CIs <- emmeans(glmCor.receiver, ~ cued) %>% as_tibble() %>% mutate(role = 'Receiver'))

#### FIGURE 2 - Facial EMG summary plots ####

##### femg overall #####

femg_plot <- function(df, CIs, role.colour) {
  df %>% 
    group_by(session,cued) %>% 
    summarise(z.mean.diff = mean(z.mean.diff)) %>%  # average over trials
    ggplot(aes(x=cued)) +
   geom_jitter(aes(y = z.mean.diff), height = 0,width = 0.25,
                colour = role.colour, alpha = 0.7,
                fill='white', shape = 21) +
    geom_hline(yintercept = 0) +
    geom_crossbar(data = CIs,
                  aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
                  colour = role.colour, width = 0.7, fill = 'white', alpha = 0.5) +
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
    # scale_y_continuous(limits = c(-2,2.5)) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.1, base = exp(1)),
                       limits = c(-2,2.5), breaks = c(-2,-1,-0.5,0,0.5,1,2),
                       labels = c(-2,-1,-0.5,0,0.5,1,2)) +
    theme_x45deg + theme_nofacetbox + theme(legend.position = 'none')

}

# Figure 2A (left)
## Zygomaticus toucher
femg.zyg.t.data %>% 
  filter(!is.na(z.mean.diff)) %>% 
  femg_plot(zyg.t.CIs, colour.toucher) -> zyg.t.plot

## Zygomaticus receiver
femg.zyg.r.data %>% 
  filter(!is.na(z.mean.diff)) %>% 
  femg_plot(zyg.r.CIs, colour.receiver) -> zyg.r.plot

zyg.t.plot + zyg.r.plot # -2, >1

# Figure 2A (right)
## Corrugator toucher
femg.cor.t.data %>% 
  filter(!is.na(z.mean.diff)) %>% 
  femg_plot(cor.t.CIs, colour.toucher) -> cor.t.plot 

## Corrugator receiver
femg.cor.r.data %>% 
  filter(!is.na(z.mean.diff)) %>% 
  femg_plot(cor.r.CIs, colour.receiver)  -> cor.r.plot

cor.t.plot + cor.r.plot # -2, > 1

##### classifier confusion matrix #####

# Figure 2B
ml.t.confusion %>%
  full_join(comm.ml.femg.t %>% 
              filter(!is.na(ml.toucher.correct)) %>%
              group_by(cued) %>% 
              summarise(Total = n())
  ) %>% 
  mutate(respFreq = as.integer(Total*(Percent/100)) ) %>%
  confusion_matrix_plot(colour.toucher, 
                        grad.limit = c(10,50), ylabels = orderedCues) -> ml.t.matrix


# Figure 2C

ml.r.confusion %>%
  full_join(comm.ml.femg.r %>% 
              filter(!is.na(ml.receiver.correct)) %>%
              group_by(cued) %>% 
              summarise(Total = n())
  ) %>% 
  mutate(respFreq = as.integer(Total*(Percent/100)) ) %>%
  confusion_matrix_plot(colour.receiver, 
                        grad.limit = c(10,50), ylabels = orderedCues) -> ml.r.matrix


ml.t.matrix + ml.r.matrix

##### correlation #####

# check n
comm.ml.femg.t %>% 
  group_by(session) %>% 
  summarise(n = sum(!is.na(ml.toucher.correct))) %>%  #ggplot(aes(x = n)) + geom_histogram(binwidth = 5, closed = 'left') + scale_y_continuous(breaks = 0:12)
  filter(n >= 10) %>% # keep receivers with at least 10 trials of ml data
  pull(session) -> keepSessions.t

ml.t.by.session <- comm.ml.femg.t %>%
  filter(response != 'open' & !is.na(ml.toucher.correct) & session %in% keepSessions.t) %>%
  group_by(session) %>% 
  summarise(receiver.pc = mean(correct)*100,
            toucher.classifier.pc = mean(ml.toucher.correct, na.rm = TRUE)*100,
            receiver.n = sum(!is.na(correct)),
            toucher.classifier.n = sum(!is.na(ml.toucher.correct))) 

(ml.corr.t <- cor.test(~ toucher.classifier.pc + receiver.pc, 
                       ml.t.by.session, alternative = 'two.sided'))
ml.corr <- tibble(toucher.r = paste('r =',round(ml.corr.t$estimate,3)),
                  toucher.p = paste('p =',round(ml.corr.t$p.value,3)))


comm.ml.femg.r %>% 
  group_by(session) %>% 
  summarise(n = sum(!is.na(ml.receiver.correct))) %>%  #ggplot(aes(x = n)) + geom_histogram(binwidth = 5, closed = 'left') + scale_y_continuous(breaks = 0:12)
  filter(n >= 10) %>% # keep receivers with at least 10 trials of ml data
  pull(session) -> keepSessions.r

ml.r.by.session <- comm.ml.femg.r %>%
  filter(response != 'open' & !is.na(ml.receiver.correct) & session %in% keepSessions.r) %>%
  group_by(session) %>% 
  summarise(receiver.pc = mean(correct)*100,
            receiver.classifier.pc = mean(ml.receiver.correct, na.rm = TRUE)*100,
            receiver.n = sum(!is.na(correct)),
            receiver.classifier.n = sum(!is.na(ml.receiver.correct))) 

(ml.corr.r <- cor.test(~ receiver.classifier.pc + receiver.pc, 
                       ml.r.by.session, alternative = 'two.sided'))
ml.corr <- ml.corr %>% bind_cols(
  tibble(receiver.r = paste('r =',round(ml.corr.r$estimate,3)),
                  receiver.p = paste('p =',round(ml.corr.r$p.value,3)))
)



# Figure 2D
ml.t.by.session %>%
  ggplot() +
  geom_smooth(aes(toucher.classifier.pc, receiver.pc), 
              method = 'lm', se = FALSE,
             size = 1, colour = colour.toucher) +
  geom_smooth(data = ml.r.by.session,
             aes(receiver.classifier.pc, receiver.pc), 
             method = 'lm', se = FALSE,
             size = 1, colour = colour.receiver) +
  geom_point(aes(toucher.classifier.pc, receiver.pc), 
            shape = 1, size = 2, colour = colour.toucher) +
  geom_point(data = ml.r.by.session,
             aes(receiver.classifier.pc, receiver.pc), 
             shape = 1, size = 2, colour = colour.receiver) +
  coord_cartesian(xlim = c(0,60), ylim = c(20,100))  +
  theme_classic() -> correlation.plot
correlation.plot

#### whole Figure 2 ####

# windows(8.5, 5.7)
quartz(width = 8.5, height = 5.7); plot(1:10)

layout.fig2.top <- '
ABCD
'
layout.fig2.bottom <- '
EFG
'
sender_annotation <- annotate(geom = 'text', x = 5.4, y = 2.4, 
                              label = 'Sender', colour = colour.toucher)
receiver_annotation <- annotate(geom = 'text', x = 5.2, y = 2.4, 
                                label = 'Receiver', colour = colour.receiver)
correlation_annotation <-  annotate(geom = 'text', x = c(50,50), y = c(98,90), 
                                    colour = c(colour.toucher, colour.receiver),
                                    label = c(paste(ml.corr$toucher.p),
                                              paste(ml.corr$receiver.p)) 
                                    )

wrap_plots(A = zyg.t.plot +
             sender_annotation +
             labs(title = 'A. Zygomaticus', 
                  x= NULL, y= 'Muscle Activity - Baseline (z)') , 
           B = zyg.r.plot +
             receiver_annotation +
             labs(x= NULL, y= NULL), 
           C = cor.t.plot +
             sender_annotation +
             labs(title = 'B. Corrugator', 
                  x= NULL, y= NULL), 
           D = cor.r.plot +
             receiver_annotation +
             labs(title = ' ',
                  x=NULL, y=NULL), 
           design = layout.fig2.top) / 
  wrap_plots(E = ml.t.matrix  +
               labs(title = 'C. Sender Classifier', x = 'Cued word', y = 'Classified'), 
             F = ml.r.matrix + 
               labs(title = 'D. Receiver Classifier', x = 'Cued word', y = NULL) , 
             G = correlation.plot +
               # correlation_annotation +
               labs(title = 'E. Correlation', 
                    y = 'Receiver hit rate (%)', x = 'Classifier hit rate (%)'), 
             design = layout.fig2.bottom) 


ggsave('figures/Figure 2 femg combined.svg')
