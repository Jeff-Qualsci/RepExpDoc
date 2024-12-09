source(file.path("R", 'fun_msx.R'))
set.seed(10302024) #For reproducible example. 

mdData <- msd_data(SmplNum = 100, TstMSD = 20, Shift = 2) %>% 
  mutate(Sample = as.character(Sample),
         Mean = (Exp1 + Exp2) / 2,
         Difference = Exp1 - Exp2)


mdStats <- repexp.stats(mdData) %>% 
  mutate(across(-n, \(x) signif(x, digits = 3)))

dataCols <- c('Difference', 'MeanDiff', 'Reference', 'UDL', 'LDL', 'ULSA', 'LLSA')

plotData <- mdData %>% 
  select(!starts_with('Exp')) %>% 
  mutate(Class = if_else(Difference > mdStats$ULSA | Difference < mdStats$LLSA, 'flagged', NA),
         MeanDiff = mdStats$MeanDiff,
         Reference = 0,
         UDL = mdStats$UDL,
         LDL = mdStats$LDL,
         ULSA = mdStats$ULSA,
         LLSA = mdStats$LLSA) %>% 
  pivot_longer(cols = all_of(dataCols), values_to = 'DataVal', names_to = 'DataType') %>% 
  mutate(Lines = if_else(str_ends(DataType,'SA'), 'Agreement Limit',
                             if_else(str_ends(DataType,'L'), 'Difference Limit', DataType, NA)))

mdConceptPlot <- ggplot(mdData, aes(x = Mean, y = Difference)) +
  annotate(geom = "rect", xmin = -50, xmax = 150, ymin = mdStats$LDL, ymax = mdStats$UDL,
           fill = "blue", linetype = 0) +
  annotate(geom = "rect", xmin = -50, xmax = 150, ymin = mdStats$UDL, ymax = mdStats$ULSA,
           fill = "green", linetype = 0) +
  annotate(geom = "rect", xmin = -50, xmax = 150, ymin = mdStats$LLSA, ymax = mdStats$LDL,
           fill = "green", linetype = 0) +
  annotate(geom = "rect", xmin = -50, xmax = 150, ymin = mdStats$ULSA, ymax = 90,
           fill = "red", linetype = 0) +
  annotate(geom = "rect", xmin = -50, xmax = 150, ymin = -90, ymax = mdStats$LLSA,
           fill = "red", linetype = 0) +
  geom_point() +
  coord_fixed(ratio = 0.6, xlim = c(-50, 150), ylim = c(-90, 90), expand = FALSE) +
  theme_linedraw() +
  ylab('Difference (std. dev.)')+
  theme(axis.text = element_blank())

dBreaks <- c('Difference', 'MeanDiff', 'Reference', 'Difference Limit', 'Agreement Limit')
dColors <- c('black', 'mediumblue', 'black', 'mediumblue', 'red')
dLinetypes <- c('blank', 'solid', 'solid', 'dashed', 'dashed')
  
mdPlot <- ggplot(plotData, aes(x = Mean, y = DataVal, group = DataType, color = Lines, linetype = Lines)) +
  geom_line(show.legend = TRUE) +
  geom_point(data = plotData %>% filter(DataType == 'Difference'))+
  scale_color_manual(name = NULL, labels = dBreaks, breaks = dBreaks, values = dColors) +
  scale_linetype_manual(name = NULL, labels = dBreaks, breaks = dBreaks, values = dLinetypes) +
  coord_fixed(ratio = 0.6) +
  ylab('Difference') +
  theme_linedraw() +
  theme(legend.position = 'bottom')       
  
library(patchwork)

logNormFig <- mdConceptPlot / mdPlot + plot_annotation(tag_levels = 'A')

corrPlot <-  ggplot(mdData,aes(x = Exp1, y = Exp2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 2) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed(ratio = 1) +
  labs(title = "Correlation Exp1 vs Exp2",
       subtitle = paste("Concordance Correlation r =", mdStats$r),
       x = "Exp1",
       y = "Exp2") +
  theme_minimal()


  


