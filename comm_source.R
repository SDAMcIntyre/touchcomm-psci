library(dplyr)
library(ggplot2)
library(stringr)

#### plot appearance ####
orderedCues <- c('attention','love','happiness','calming','sadness','gratitude')

colour.intuitive <-  '#377EB8' #blue
colour.expert <- '#4DAF4A' # green
colour.receiver <- "#E69F00" #yellow
colour.toucher <- '#984ea3' #purple 
colour.swapped <- '#ff00b3' #pink

theme_basic <- theme_bw() +theme(panel.border = element_blank(), axis.line = element_line(colour = 'black'))

theme_biggerfonts <- theme(
  axis.title.x=element_text(size=14), 
  axis.title.y=element_text(size=14,angle=90, margin = margin(t = 0, r = 5, b = 0, l = 15)), 
  axis.text.x=element_text(size=12), 
  axis.text.y=element_text(size=12), 
  strip.text.x=element_text(size=14), 
  legend.text=element_text(size=12), 
  legend.title=element_text(size=14))

theme_x45deg <- theme(
  axis.text.x=element_text(angle=45, hjust = 1 ))

theme_confmat_legend <- theme(legend.margin=margin(0,0,0,0),
                              legend.box.margin=margin(t = 0, r = 0, b = 0,l = -10), 
                              legend.text=element_text(size=8))

theme_nofacetbox <- theme(strip.background = element_blank(), 
                          strip.text = element_text(colour = 'black'))

theme_nolegend <- theme(legend.position = "none")

theme_insidelegend <- function(x,y) {
  theme(legend.position = c(x,y), 
        legend.background = element_blank(),
        legend.key = element_rect(colour = 'grey'))}

theme_bwstrip <- theme(strip.background = element_rect(fill = 'white',colour = 'grey',size = 1), 
                       strip.text = element_text(colour = 'black'))

#### confusion matrix functions ####

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

round_integer <- function(x) 
  { trunc(x+sign(x)*0.5) }

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

order_PIDs <- function(.data, PID) {
  .data %>%
    confusion_matrix_data({{PID}}) %>% 
    filter(cued == response) %>% 
    group_by({{PID}}) %>% 
    summarise(performance = mean(Percent)) %>% 
    arrange(-performance) %>% 
    pull({{PID}})
}

confusion_matrix_individual_plot <- function(df, grad.colour, grad.limit = c(0,100), ylabels, abr = 1) {
  df %>% 
    mutate(cued = factor(cued, levels = orderedCues),
           response = factor(response, levels = rev(ylabels))) %>%
    ggplot(aes(x=cued, y=response, fill=Percent)) +
    geom_tile(color="black",size=0.1) +
    scale_fill_gradient(name =' %', na.value = 'white', low='white', high=grad.colour, 
                        guide = 'legend', limits = grad.limit) + 
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),abr,'right','')) +
    scale_y_discrete(label = str_trunc(str_to_title(rev(ylabels)),abr,'right','')) +
    theme_classic() + theme_confmat_legend + 
    theme_nofacetbox + theme(axis.line = element_blank(), axis.ticks = element_blank()) +
    labs(x = 'Cued word', y = 'Receiver response')
}

#### F1 functions ####

f1_mm_plot_e1 <- function(.emdata, .rawdata, fill) {
  .emdata %>% 
    ggplot(aes(y = prob, x = cued, colour = roles, fill = roles)) +
    geom_point(data = .rawdata, aes(y = F1),
               shape = 21, size = 3, alpha = 0.2,
               position = position_dodge(0.2)) +
    geom_hline(yintercept = .rawdata$F1chance[1], 
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
    annotate("text", x = 1.2, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') 
}
  
f1_mm_plot <- function(.emdata, .rawdata, fill) {
  .emdata %>% 
    ggplot(aes(y = prob, x = cued)) +
    geom_jitter(data = .rawdata, aes(y = F1),
                shape = 21, size = 3, alpha = 0.4,
                fill = {{fill}}, width = 0.1) +
    geom_hline(yintercept = .rawdata$F1chance[1], 
               colour = 'grey', linetype = 'dashed', size = 1) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 0.4, size = 1) +
    geom_point(size = 6, shape = "\u2014") + # unicode m-dash for horizontal line
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Performance F1') +
    theme_biggerfonts + theme_x45deg + 
    annotate("text", x = 1.2, y = 0.25, 
             label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}