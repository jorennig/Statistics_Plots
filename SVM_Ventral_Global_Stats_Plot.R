# Clear environment
rm(list=ls())

pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

library(lme4)
library(lmerTest)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data (POF)
accuracy <- read.table("SVM_Ventral_Global_acc.txt", sep=",", header=T)

accuracy <- gather(accuracy, key = "stim", value = "acc", -sub, -roi, -hem)

accuracy$acc <- accuracy$acc*100

accuracy$roi <- paste(accuracy$roi, "ROI")

accuracy$hem[accuracy$hem == "l"]  <- "L"
accuracy$hem[accuracy$hem == "r"]  <- "R"

# Data summary
accuracy_sum_hem <- accuracy %>%
  group_by(roi, hem, stim) %>%
  summarise(mean = mean(acc, na.rm = T),
            sd = sd(acc, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)


accuracy_sum <- accuracy %>%
  group_by(roi, stim) %>%
  summarise(mean = mean(acc, na.rm = T),
            sd = sd(acc, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

accuracy_tot <- accuracy[accuracy$stim == "acc_tot",]

accuracy_con <- accuracy[accuracy$stim != "acc_tot",]


# LME accuracy
lme_tot_hem <- lmer(acc ~ roi*hem + (1|sub), data = accuracy_tot, REML = F)
summary(lme_tot_hem)
Anova(lme_tot_hem)

lme_tot <- lmer(acc ~ roi + (roi|sub) + (1|sub), data = accuracy_tot, REML = F)
summary(lme_tot)
Anova(lme_tot)

comparisons <- list(c('Objects ROI', 'Faces ROI'), c('Objects ROI', 'Places ROI'), c('Faces ROI', 'Places ROI'))
results_lmem <- NULL
for (c in comparisons) {
  
  data_c <- accuracy_tot[accuracy_tot$roi==c[1] | accuracy_tot$roi==c[2],]
  
  lmem <- lmer(acc ~ roi + (1|sub), data=data_c, REML=F)
  summary <- summary(lmem)
  results <- Anova(lmem)
  
  results$roi1 <- c[1]
  results$roi2 <- c[2]
  
  results_lmem <- rbind(results_lmem, results)
}
p <- results_lmem$`Pr(>Chisq)`
results_lmem$p_adjusted <- p.adjust(p, method='BH', n = length(p))


# LME predictive values
lme_con_hem <- lmer(acc ~ stim*roi*hem + (1|sub) + (stim|sub), data = accuracy_con, REML = F)
summary(lme_con_hem)
Anova(lme_con_hem)

lme_con <- lmer(acc ~ stim*roi + (roi|sub) + (stim|sub), data = accuracy_con, REML = F)
summary(lme_con)
Anova(lme_con)


# T test against chance
t_test_chance <- accuracy %>%
  group_by(roi, hem, stim) %>%                       
  summarise(res = list(tidy(t.test(acc, mu=50)))) %>%
  unnest(cols = c(res))

t_test_tot <- t_test_chance[t_test_chance$stim=='acc_tot',]
p <- t_test_tot$p.value
t_test_tot$p_adjusted <- p.adjust(p, method='BH', n = length(p))

t_test_con <- t_test_chance[t_test_chance$stim!='acc_tot',]
p <- t_test_con$p.value
t_test_con$p_adjusted <- p.adjust(p, method='BH', n = length(p))


## Plot
accuracy_tot_sum <- accuracy_sum_hem[accuracy_sum_hem$stim == "acc_tot",]
accuracy_tot_sum$roi <- factor(accuracy_tot_sum$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))

# Accuracy total
accuracy_tot_lh_rh_bar <- ggplot(accuracy_tot_sum, aes(x = hem, y = mean)) +
  
  geom_bar(stat="identity", position=position_dodge(), fill = "azure4") +
  
  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.2, position=position_dodge(0.9)) +

  ylab("Classification Accuracy (%)") +
  
  xlab(" ") +

  ylim(0,70) +
  
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 0.5) +
  
  facet_wrap(vars(roi)) +
  
  # Remove dark background + Additional paramaters for displaying plot
  #guides(fill = FALSE) + 
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10))

accuracy_tot_lh_rh_bar
ggsave("Accuracy_tot_Global_lh_rh_bar.pdf", width = 12, height = 10, units = "cm", dpi = 300)


## Accuracy conditions
accuracy_gpof_sum <- accuracy_sum_hem[accuracy_sum_hem$stim == "acc_global" | accuracy_sum_hem$stim == "acc_scrambeled",]

accuracy_gpof_sum$stim[accuracy_gpof_sum$stim == "acc_global"]  <- "Global"
accuracy_gpof_sum$stim[accuracy_gpof_sum$stim == "acc_scrambeled"]  <- "Scrambled"

accuracy_gpof_sum$stim <- factor(accuracy_gpof_sum$stim, levels = c("Global", "Scrambled"))
accuracy_gpof_sum$roi <- factor(accuracy_gpof_sum$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))


accuracy_con_lh_rh_bar <- ggplot(accuracy_gpof_sum, aes(x = hem, y = mean, fill = stim)) +

  geom_bar(stat="identity", position=position_dodge()) +

  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.2, position=position_dodge(0.9)) +

  scale_fill_manual(values =  c("azure3", "azure4", "grey40", "slategrey")) +

  ylab("Classification Accuracy (%)") +

  xlab(" ") +

  ylim(0,70) +

  geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 0.5) +

  facet_wrap(vars(roi)) +

  labs(fill = 'Stimulus') +
  
  # Remove dark background + Additional paramaters for displaying plot
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10))

accuracy_con_lh_rh_bar
ggsave("Accuracy_con_Global_lh_rh_bar.pdf", width = 24, height = 12, units = "cm", dpi = 300)
