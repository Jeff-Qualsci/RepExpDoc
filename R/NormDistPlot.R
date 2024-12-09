library(tidyverse)
library(patchwork)

cathist.plot <- function(x, Type) {
  
  df <- tibble(x, Type)
  ggplot(df, aes(x = x, fill = Type)) +
    geom_histogram() + 
    scale_fill_manual(breaks = c("Flagged", "Diff.Limits", "Agreement"), 
                      values=c("red", "blue", "green")) +
    theme_linedraw()
}

# Set the figure directory for the generated figures
fig_dir = 'www'

# If the directory does not exist, then create directory
if (!dir.exists(fig_dir)){
  dir.create(fig_dir)
} 

# Replicate Experiment Difference Statistics Plot --------------
diffPlot <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm,
              geom = "line",
              xlim = c(-4, 4)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'blue',
                xlim = c(-.2, .2)) +
  geom_text(x = 0, y = .2, label = 'Diff. Limits', angle = 90, color = 'white') +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'green',
                xlim = c(0.15, 2)) +
  geom_text(x = 0.8, y = .15, label = 'MSD') +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'green',
                xlim = c(-2, -0.2)) +
  geom_text(x = -0.8, y = .15, label = 'MSD') +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'red',
                xlim = c(2, 4)) +
  geom_text(x = 2.15, y = .1, label = 'LSA') +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'red',
                xlim = c(-4, -2)) +
  geom_text(x = -2.2, y = .1, label = 'LSA') +
  xlim(-4, 4) + 
  labs(title = 'Difference Distribution with Replicate Experiment Statistics',
       x = 'Standard Deviation',
       y = '') +
  scale_y_continuous(breaks = NULL) +
  theme_linedraw()
  
ggsave(filename = file.path(fig_dir, 'DiffPlot.png'), plot = diffPlot, height = 3, width = 5, units = 'in')

# Efficacy Difference Histogram -------------------
# effStats <- read_csv('Webtool Output/MSD20_JRW/MSD20Data320_EFF_Stats_tbl.csv')
# 
# effCalcData <- read_csv('Webtool Output/MSD20_JRW/MSD20Data320_EFF_OutputData_tbl.csv')%>% 
#   select(c(1, 2, 3, 7, 8)) %>% 
#   rename(Mean = MeasMean, Difference = MeasDiff) %>% 
#   mutate(Type = if_else(between(Difference, effStats$'Lower Difference Limit', effStats$'Upper Difference Limit'), 'Diff.Limits',
#                                  if_else(between(Difference, effStats$'Lower Agreement Limit', effStats$'Upper Agreement Limit'), 'Agreement','Flagged')))
# 
# effDiffHist <- cathist.plot(x = effCalcData$Difference, Type = effCalcData$Type)
# 
# ggsave(filename = file.path(fig_dir, 'MSD20DiffHist.png'), plot = effDiffHist, width = 4, height = 3, units = 'in')


# Log Normal example ------------------------------

logPot <- rnorm(n = 10000, mean = 2, sd = log10(1.5))
Pot = 10 ^ logPot


potData <- tibble(Pot, logPot) %>% 
  mutate(Type = if_else(between(logPot, -0.2, 0.2), "Ratio Limits",
                        if_else(between(logPot, -2, -0.2) | between(logPot, 0.2, 2), "Agreement", "Flagged")),
         Side = if_else(logPot < 0, 'Left', 'Right')) %>% 
  pivot_longer(cols = contains('Pot'), names_to = 'Scale', values_to = 'Concentration')

potBounds <- potData %>% 
  group_by(Scale) %>% 
  summarise(Rng = range(Concentration),
            Mean = max(Concentration),
            SD = sd(Concentration)) %>% 
  ungroup()

logNormPlot <- ggplot(filter(potData, Scale == 'Pot'), aes(x = Concentration)) +
  geom_density() +
  labs(title = 'Distribution of Potency Data') +
  xlab('Potency') +
  xlim(0, 400) +
  theme_linedraw() 

logPotPlot <- ggplot(filter(potData, Scale == 'logPot'), aes(x = Concentration)) +
  geom_density() +
  labs(title = 'Distribution of log(Potency) Data') +
  xlab('log10(Potency)') +
  theme_linedraw()

transNormPlot <- logNormPlot +
  scale_x_continuous(trans='log10', labels = scales::comma)

logNormFig <- logNormPlot / logPotPlot / transNormPlot + plot_annotation(tag_levels = 'A')

ggsave(filename = file.path(fig_dir, 'LogNormFig.png'), plot = logNormFig, height = 6, width = 4, units = 'in')
