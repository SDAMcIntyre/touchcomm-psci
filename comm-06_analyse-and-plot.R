library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(afex)
library(emmeans)
library(psych)
library(patchwork)
library(parallel)

source('comm_source.R')

#### read full data ####
comm.e1 <- read_csv('data/primary/comm_expt1-collated.csv') %>%
  mutate(exptPID = paste('E1', pair, sep = '.'),
         roles = if_else(SessionNo == 'Session 1', 'Initial roles', 'Swapped roles'))

comm.e2 <- read_csv('data/primary/comm_expt2-collated.csv') %>%
  mutate(exptPID = paste('E2',pair, sep = '.'))

comm.e3 <- read_csv('data/primary/comm_expt3-collated.csv') %>%
  mutate(toucher = 1,
         exptPID = paste('E3',PID, sep = '.'))

comm.e4 <- read_csv('data/primary/comm_expt3-collated.csv') %>%
  mutate(exptPID = paste('E4',PID, sep = '.'))


#### read performance metrics ####

performance.e1 <- read_csv('data/processed/comm_expt1-performance.csv') %>% 
  mutate(cued = factor(cued, levels = orderedCues))

performance.e2 <- read_csv('data/processed/comm_expt2-performance.csv') %>% 
  mutate(cued = factor(cued, levels = orderedCues))

performance.e3 <- read_csv('data/processed/comm_expt3-performance.csv') %>% 
  mutate(cued = factor(cued, levels = orderedCues))

performance.e4 <- read_csv('data/processed/comm_expt4-performance.csv') %>% 
  mutate(cued = factor(cued, levels = orderedCues))

performance.over.time <- read_csv('data/processed/comm_expts3-4-performance-over-time.csv') %>% 
  mutate(cued = factor(cued, levels = orderedCues))

#### mixed effects models ####
set_sum_contrasts()
theme_set(theme_light())

###. experiment 1 ####

mm.e1 <- mixed(F1 ~ roles*cued + (1|exptPID), 
               data = performance.e1, 
               method = 'LRT',
               control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
               family = binomial, weights=performance.e1$Total )
summary(mm.e1)

# reported
anova(mm.e1)
emmeans(mm.e1, ~ cued, type = 'response')
emmeans(mm.e1,  ~  roles, type = 'response')
(emm.e1 <- emmeans(mm.e1,  ~  roles + cued))
plot(emm.e1, comparisons = TRUE, adjust = 'holm', by = 'roles') # quick look

# reported
pairs(emm.e1, simple = 'roles', adjust = 'holm', type = 'response', infer = TRUE)

# reported
( e1.vs.chance <- rbind(emmeans(mm.e1, ~ cued), emmeans(mm.e1, ~1)) %>% # add intercept
    as_tibble() %>% 
    mutate(emmean.p = logistic(emmean),
           chance = performance.e1$F1chance[1],
           z.vs.chance = (emmean - logit(chance))/(emmean*SE),
           p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
           p.holm = p.adjust(p.vs.chance, method = 'holm')) )
max(e1.vs.chance$p.holm)
# report CIs
emmeans(mm.e1, ~1, type = 'response')

#(emm.e2 <- emmeans(mm.e2, ~ cued, type = 'response'))

###.. figure 1d ####
emmeans(mm.e1,  ~  roles + cued, type = 'response') %>%
  as_tibble() %>% 
  ggplot(aes(y = prob, x = cued, colour = roles, fill = roles)) +
  geom_point(data = performance.e1, aes(y = F1),
             shape = 21, size = 3, alpha = 0.2,
             position = position_dodge(0.2)) +
  geom_hline(yintercept = performance.e1$F1chance[1], 
             colour = 'grey', linetype = 'dashed', size = 1) +
  geom_errorbar(aes(x = as.numeric(cued) - 0.2, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.4, size = 1.3,
                position = position_dodge(0.2)) +
  geom_point(aes(x = as.numeric(cued) - 0.2),
             size = 6, shape = "\u2014", # unicode m-dash for horizontal line
             position = position_dodge(0.2)) +
  scale_color_manual(values = c(colour.intuitive, colour.swapped)) +
  scale_fill_manual(values = c(colour.intuitive, colour.swapped)) +
  scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = 'Experiment 1', x = NULL, y = 'Performance F1', colour = NULL, fill = NULL) +
  theme_biggerfonts + theme_x45deg +
  annotate("text", x = 1.2, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') -> f1.e1

###. experiment 2 ####

mm.e2 <- mixed(F1 ~ cued + (1|exptPID), 
               data = performance.e2, 
               method = 'LRT',
               control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
               family = binomial, weights=performance.e2$Total )
summary(mm.e2)

# reported
anova(mm.e2)

plot(emmeans(mm.e2,  ~  cued), comparisons = TRUE, adjust = 'holm') # quick look

# reported
( e2.vs.chance <- rbind(emmeans(mm.e2, ~ cued), emmeans(mm.e2, ~1)) %>% # add intercept
    as_tibble() %>% 
    mutate(emmean.p = logistic(emmean),
           chance = performance.e2$F1chance[1],
           z.vs.chance = (emmean - logit(chance))/(emmean*SE),
           p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
           p.holm = p.adjust(p.vs.chance, method = 'holm')) )
max(e2.vs.chance$p.holm)

# report CIs
emmeans(mm.e2, ~1, type = 'response')

(emm.e2 <- emmeans(mm.e2, ~ cued, type = 'response'))

###.. figure 1f ####

emm.e2 %>%
  as_tibble() %>% 
  f1_mm_plot(performance.e2, colour.intuitive) + 
  labs(title = 'Experiment 2')  -> f1.e2

###. experiment 3 ####

mm.e3 <- mixed(F1 ~ cued + (1|exptPID), 
               data = performance.e3, 
               method = 'LRT',
               control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
               family = binomial, weights=performance.e3$Total )
summary(mm.e3)

# reported
anova(mm.e3)

plot(emmeans(mm.e3,  ~  cued), comparisons = TRUE, adjust = 'holm') # quick look

# reported
( e3.vs.chance <- rbind(emmeans(mm.e3, ~ cued), emmeans(mm.e3, ~1)) %>% # add intercept
    as_tibble() %>% 
    mutate(emmean.p = logistic(emmean),
           chance = performance.e3$F1chance[1],
           z.vs.chance = (emmean - logit(chance))/(emmean*SE),
           p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
           p.holm = p.adjust(p.vs.chance, method = 'holm')) )
max(e3.vs.chance$p.holm)

# report CIs
emmeans(mm.e3, ~1, type = 'response')

(emm.e3 <- emmeans(mm.e3, ~ cued, type = 'response'))

###.. figure 4b ####
emm.e3 %>%
  as_tibble() %>% 
  f1_mm_plot(performance.e3, colour.expert) + 
  labs(title = 'Experiment 3')  -> f1.e3

###. experiment 4 ####

mm.e4 <- mixed(F1 ~ cued + (1|exptPID), 
               data = performance.e4, 
               method = 'LRT',
               control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
               family = binomial, weights=performance.e4$Total )
summary(mm.e4)

# reported
anova(mm.e4)

plot(emmeans(mm.e4,  ~  cued), comparisons = TRUE, adjust = 'holm') # quick look

# reported
( e4.vs.chance <- rbind(emmeans(mm.e4, ~ cued), emmeans(mm.e4, ~1)) %>% # add intercept
    as_tibble() %>% 
    mutate(emmean.p = logistic(emmean),
           chance = performance.e4$F1chance[1],
           z.vs.chance = (emmean - logit(chance))/(emmean*SE),
           p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
           p.holm = p.adjust(p.vs.chance, method = 'holm')) )
max(e4.vs.chance$p.holm)

# report CIs
emmeans(mm.e4, ~1, type = 'response')

(emm.e4 <- emmeans(mm.e4, ~ cued, type = 'response'))

###.. figure 4d ####
emm.e4 %>%
  as_tibble() %>% 
  f1_mm_plot(performance.e4, colour.expert) + 
  labs(title = 'Experiment 4')  -> f1.e4

###. intuitive vs standardized ####

# combine data
performance.all <- performance.e1 %>% 
  full_join(performance.e2 ) %>% 
  full_join(performance.e3 ) %>% 
  full_join(performance.e4 ) %>% 
  mutate(experiment = str_extract(exptPID, 'E[0-9]'),
         touchType = if_else(parse_number(experiment) < 3, 'intuitive', 'expert'))

(nc <- detectCores()) # number of cores
cl <- makeCluster(rep("localhost", nc), setup_strategy = "sequential") # make cluster

#  this takes a long time
mm.pb <- mixed(F1 ~ touchType*cued + (1|exptPID),
               data = performance.all, 
               method = 'PB', args_test = list(nsim =1000, cl = cl),
               control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e9)),
               family = binomial, weights = performance.all$Total )

# excluding the swapped roles trials from expt 1
# performance.exclswapped <- performance.all %>% filter(is.na(roles) | roles == 'Starting roles')
# mm.pb.exclswapped <- mixed(F1 ~touchType*cued + (1|exptPID),
#                            data = performance.exclswapped, 
#                            method = 'PB', args_test = list(nsim =1000, cl = cl),
#                            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e9)),
#                            family = binomial, weights = performance.exclswapped$Total )

# reported
anova(mm.pb)

# similar result
# anova(mm.pb.exclswapped) 

(emm <- emmeans(mm.pb,  ~  touchType + cued))
plot(emm, comparisons = TRUE, adjust = 'holm') # quick look

# reported
pairs(emm, simple = 'touchType', adjust = 'holm', type = 'response', infer = TRUE)
emmeans(mm.pb, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)

emmeans(mm.pb, ~ touchType, type = 'response')
# emmeans(mm.pb.exclswapped, ~ touchType, type = 'response')

###. figure 5b ####
emmeans(mm.pb,  ~  touchType + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(touchType = factor(touchType, levels = c('intuitive','expert')),
         cued = factor(cued, levels = orderedCues)) %>% 
  ggplot(aes(y = prob, x = cued, colour = touchType, fill = touchType)) +
  geom_hline(yintercept = performance.all$F1chance[1], 
             colour = 'grey', linetype = 'dashed', size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.4, size = 1.3,
                position = position_dodge(0.2)) +
  geom_point(size = 6, shape = "\u2014", # unicode m-dash for horizontal line
             position = position_dodge(0.2)) +
  scale_color_manual(values = c(colour.intuitive, colour.expert)) +
  scale_fill_manual(values = c(colour.intuitive, colour.expert)) +
  scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = 'Intuitive vs. Standardized touch', x = NULL, y = 'Performance F1', colour = NULL, fill = NULL) +
  theme_light() + theme_biggerfonts + theme_x45deg + theme_insidelegend(0.85,0.85) +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') -> compare.plot

#### performance over time ####

###. figure 5c ####
performance.over.time %>% 
  mutate(cued = factor(cued, levels = orderedCues),
         presentationNumber = as.integer(presentationNumber)) %>% 
  ggplot(aes(x = presentationNumber, y = F1, colour = cued, fill = cued)) +
  geom_point(shape = 1, size = 2, position = position_jitter(0.1)) + 
  geom_smooth(method = 'loess', alpha = 0.15, span = 1) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = 'Standardized touch', y = 'Performance F1', x = 'Presentation number', 
       colour = NULL, fill = NULL) -> time.expert

#### confusion matrices ####

###. figure 1b ####
comm.e1 %>%
  filter(response != 'open' & SessionNo == 'Session 1') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.intuitive, ylabels = c(orderedCues,'other')) + 
  labs(title = 'E1: Initial roles') -> confmat.e1.role1

###. figure S1a ####
orderedPIDs1.1 <- comm.e1 %>% 
  filter(response != 'open' & SessionNo == 'Session 1') %>% 
  order_PIDs(exptPID)
comm.e1 %>%
  filter(response != 'open' & SessionNo == 'Session 1') %>% 
  confusion_matrix_data(exptPID)  %>%
  mutate(exptPID = factor(exptPID, levels = orderedPIDs1.1)) %>% 
  confusion_matrix_individual_plot(colour.intuitive, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ exptPID, ncol = 4) -> confmat.e1.role1.ind

###. figure 1c ####
comm.e1 %>%
  filter(response != 'open' & SessionNo == 'Session 2') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.swapped, ylabels = c(orderedCues,'other')) + 
  labs(title = 'E1: Swapped roles') -> confmat.e1.role2

###. figure S1b ####
orderedPIDs1.2 <- comm.e1 %>% 
  filter(response != 'open' & SessionNo == 'Session 2') %>% 
  order_PIDs(exptPID)
comm.e1 %>%
  filter(response != 'open' & SessionNo == 'Session 2') %>% 
  confusion_matrix_data(exptPID)  %>%
  mutate(exptPID = factor(exptPID, levels = orderedPIDs1.2)) %>% 
  confusion_matrix_individual_plot(colour.swapped, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ exptPID, ncol = 4) -> confmat.e1.role2.ind

###. figure 1e ####
comm.e2 %>% 
  confusion_matrix_data() %>% 
  confusion_matrix_plot(colour.intuitive,  ylabels = c(orderedCues,'timeout')) + 
  labs(title = 'Experiment 2') -> confmat.e2

###. figure S1c ####
orderedPIDs2 <- comm.e2 %>% order_PIDs(exptPID)
comm.e2 %>%
  confusion_matrix_data(exptPID) %>% 
  mutate(exptPID = factor(exptPID, levels = orderedPIDs2) ) %>%
  confusion_matrix_individual_plot(colour.intuitive, ylabels = c(orderedCues,'timeout')) +
  facet_wrap(. ~ exptPID, ncol = 10) -> confmat.e2.ind

###. figure 4a ####
comm.e3 %>% 
  confusion_matrix_data() %>% 
  confusion_matrix_plot(colour.expert, ylabels = orderedCues) + 
  labs(title = 'Experiment 3') -> confmat.e3

###. figure S3a ####
orderedPIDs3 <- comm.e3 %>% order_PIDs(exptPID)
comm.e3 %>% 
  confusion_matrix_data(exptPID) %>% 
  mutate(exptPID = factor(exptPID, levels = orderedPIDs3) ) %>%
  confusion_matrix_individual_plot(colour.expert, ylabels = orderedCues) +
  facet_wrap(. ~ exptPID, ncol = 5) -> confmat.e3.ind

###. figure 4c ####
comm.e4 %>% 
  confusion_matrix_data() %>% 
  confusion_matrix_plot(colour.expert, ylabels = c(orderedCues,'other')) + 
  labs(title = 'Experiment 4') -> confmat.e4

###. figure S3b ####
orderedPIDs4 <- comm.e4 %>% order_PIDs(exptPID)
comm.e4 %>% 
  confusion_matrix_data(exptPID) %>% 
  mutate(exptPID = factor(exptPID, levels = orderedPIDs4) ) %>%
  confusion_matrix_individual_plot(colour.expert, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ exptPID, ncol = 4) -> confmat.e4.ind

#### combine figures ####

###. figure 1 ####
layout.fig1 <- '
AAB
CDD
EFF
'
quartz(width = 9.6, height = 8.7); plot(1:10)
wrap_plots(A = plot_spacer(), B = confmat.e1.role1, 
           C = confmat.e1.role2, E = confmat.e2, 
           D = (f1.e1 + theme_insidelegend(0.8,1.2)), F = f1.e2, 
           design = layout.fig1) + #plot_layout(guides = 'collect') 
  plot_annotation(tag_levels = 'A')
ggsave('figures/Figure 1 intuitive comm performance.svg')
# combined with line drawing (panel A) in Adobe Illustrator

###. figure 4 ####
layout.fig4 <- '
ABB
CDD
'
quartz(width = 8.7, height = 6.3); plot(1:10)
wrap_plots(A = confmat.e3, B = f1.e3, C = confmat.e4, E = f1.e4,
           design = layout.fig4) +
  plot_annotation(tag_levels = 'A')
ggsave('figures/Figure 4 expert comm performance.svg')
ggsave('figures/Figure 4 expert comm performance.pdf')

###. figure 5 ####

quartz(width = 5.6, height = 7.0); plot(1:10)
compare.plot / time.expert +
  plot_annotation(tag_levels = 'A')
ggsave('figures/Figure 5 intuitive vs standardized.svg')
# combined with SH's tracking figure (panel A) in Adobe Illustrator

#### supplementary figures ####

###. figure S1 ####

design.intuitive = '
AAABBBC
AAABBBC
DDDDDDD
'
quartz(width = 8.2, height = 7.6); plot(1:10)
confmat.e1.role1.ind + labs(title = 'A. Experiment 1 initial roles') +
  confmat.e1.role2.ind + labs(title = 'B. Experiment 1 swapped roles') +
  guide_area() +
  confmat.e2.ind + labs(title = 'C. Experiment 2') +
  plot_layout(design = design.intuitive, guides = 'collect') 

ggsave('figures/FigS1_intuitive_individual-conf-mat.svg')
ggsave('figures/FigS1_intuitive_individual-conf-mat.pdf')

###. figure S3 ####

quartz(width = 7.3, height = 5.2); plot(1:10)
confmat.e3.ind + labs(title = 'A. Experiment 3') +
  confmat.e4.ind + labs(title = 'B. Experiment 4') +
  plot_layout(widths = c(4.8,4), guides = 'collect') & 
  theme(legend.position = 'bottom', legend.spacing.x = unit(0,'cm')) &
  guides(fill = guide_legend(label.position = "bottom"))

ggsave('figures/FigS3_expert_individual-conf-mat.svg')
ggsave('figures/FigS3_expert_individual-conf-mat.pdf')

