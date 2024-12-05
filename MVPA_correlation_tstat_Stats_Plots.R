# Clear environment
rm(list=ls())

library(lme4)
library(lmerTest)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)

pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# Read data (POF)
data <- read.table("MVPA_correlation_tstat.csv", sep=",", header=T)

data$roi <- paste(data$roi, "ROI")

# Data summary correlation
summary_roi <- data %>%
  group_by(roi) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

summary_correlation_hem <- data %>%
  group_by(roi, hem, correlation) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

summary_correlation <- data %>%
  group_by(roi, correlation) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

# Data summary congruency
summary_congruency_hem <- data %>%
  group_by(roi, hem, congruency) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

summary_congruency <- data %>%
  group_by(roi, congruency) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

# Data summary form
summary_form_hem <- data %>%
  group_by(roi, hem, form) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

summary_form <- data %>%
  group_by(roi, form) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

# Data summary form identical
summary_form_identical_hem <- data %>%
  group_by(roi, hem, form_identical) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)

summary_form_identical <- data %>%
  group_by(roi, form_identical) %>%
  summarise(mean = mean(r, na.rm = T),
            sd = sd(r, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)


# LME correlation
# lme_correlation_hem <- lmer(r ~ roi*hem*correlation + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_correlation_hem)
# Anova(lme_correlation_hem)
# 
# lme_correlation <- lmer(r ~ roi*correlation + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_correlation)
# Anova(lme_correlation)

# LME congruency
# lme_congruency_hem <- lmer(r ~ roi*hem*congruency + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_congruency_hem)
# Anova(lme_congruency_hem)

lme_congruency <- lmer(r ~ roi*congruency + (roi|subject_id) + (1|subject_id), data = data, REML = F)
summary(lme_congruency)
Anova(lme_congruency)

# LME form
# lme_form_hem <- lmer(r ~ roi*hem*form + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_form_hem)
# Anova(lme_form_hem)

lme_form <- lmer(r ~ roi*form + (roi|subject_id) + (1|subject_id), data = data, REML = F)
summary(lme_form)
Anova(lme_form)

# LME form_identical
# lme_form_identical_hem <- lmer(r ~ roi*hem*form_identical + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_form_identical_hem)
# Anova(lme_form_identical_hem)
# 
# lme_form_identical <- lmer(r ~ roi*form_identical + (roi|subject_id) + (1|subject_id), data = data, REML = F)
# summary(lme_form_identical)
# Anova(lme_form_identical)


## Post-hoc analysis
comparisons <- list(c('Objects ROI', 'Faces ROI'), c('Objects ROI', 'Places ROI'), c('Faces ROI', 'Places ROI'))
results_roi <- NULL
for (c in comparisons) {
  data_c <- data[(data$roi==c[1] | data$roi==c[2]),]
  
  lmem <- lmer(r ~ roi + (1|subject_id), data=data_c, REML=F)
  summary <- summary(lmem)
  results <- Anova(lmem)
  
  results$roi1 <- c[1]
  results$roi2 <- c[2]
  
  results_roi <- rbind(results_roi, results)
}
p <- results_roi$`Pr(>Chisq)`
results_roi$p_adjusted <- p.adjust(p, method='BH', n = length(p))

comparisons <- list(c('congruent*congruent', 'congruent*incongruent'), c('congruent*congruent', 'incongruent*incongruent'), c('congruent*incongruent', 'incongruent*incongruent'))
rois <- c('Objects ROI', 'Faces ROI', 'Places ROI')
results_roi_congruency <- NULL
for (r in rois) {
  for (c in comparisons) {
    data_c <- data[(data$roi==r | data$congruency==c[1] | data$congruency==c[2]),]
  
    lmem <- lmer(r ~ congruency + (1|subject_id), data=data_c, REML=F)
    summary <- summary(lmem)
    results <- Anova(lmem)
    
    results$roi <- r
    results$correlation1 <- c[1]
    results$correlation2 <- c[2]
  
    results_roi_congruency <- rbind(results_roi_congruency, results)
  }
}
p <- results_roi_congruency$`Pr(>Chisq)`
results_roi_congruency$p_adjusted <- p.adjust(p, method='BH', n = length(p))

comparisons <- list(c('circle*circle', 'circle*square'), c('circle*circle', 'square*square'), c('circle*square', 'square*square'))
results_form <- NULL
for (c in comparisons) {
  data_c <- data[(data$form==c[1] | data$form==c[2]),]
  
  lmem <- lmer(r ~ form + (1|subject_id), data=data_c, REML=F)
  summary <- summary(lmem)
  results <- Anova(lmem)
  
  results$correlation1 <- c[1]
  results$correlation2 <- c[2]
  
  results_form <- rbind(results_form, results)
}
p <- results_form$`Pr(>Chisq)`
results_form$p_adjusted <- p.adjust(p, method='BH', n = length(p))


## Plot by correlation
summary_correlation$roi <- factor(summary_correlation$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))
summary_correlation$correlation <- factor(summary_correlation$correlation, levels = c("congruent_circle*congruent_square", "congruent_circle*incongruent_circle","congruent_circle*incongruent_square", "congruent_square*incongruent_circle", "congruent_square*incongruent_square", "incongruent_circle*incongruent_square"))

correlations_bar <- ggplot(summary_correlation, aes(x = correlation, y = mean)) +
  
  geom_bar(stat="identity", position=position_dodge(), fill = "azure4") +
  
  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.15, position=position_dodge(0.9)) +
  
  ylab("r") +
  
  facet_wrap(vars(roi)) +
  
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  
  scale_y_continuous(limits = c(0, 0.9), breaks=seq(0, 0.8, 0.2)) +
  
  # Remove dark background + Additional paramaters for displaying plot
  #guides(fill = FALSE) + 
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), axis.title.x = element_blank())

correlations_bar
ggsave("Correlations_tstat_global_con.pdf", width = 14, height = 10, units = "cm", dpi = 300)
ggsave("Correlations_tstat_global_con.png", width = 14, height = 10, units = "cm", dpi = 300)


## Plot by congruency
summary_congruency$roi <- factor(summary_congruency$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))
summary_congruency$congruency <- factor(summary_congruency$congruency, levels = c("congruent*congruent", "incongruent*incongruent","congruent*incongruent"))

congruency_bar <- ggplot(summary_congruency, aes(x = congruency, y = mean)) +
  
  geom_bar(stat="identity", position=position_dodge(), fill = "azure4") +
  
  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.15, position=position_dodge(0.9)) +
  
  ylab("r") +
  
  facet_wrap(vars(roi)) +
  
  scale_x_discrete(guide = guide_axis(angle = 50)) +

  scale_y_continuous(limits = c(0, 0.9), breaks=seq(0, 0.8, 0.2)) +
  
  # Remove dark background + Additional paramaters for displaying plot
  #guides(fill = FALSE) + 
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), axis.title.x = element_blank())

congruency_bar
ggsave("Congruency_tstat_global_con.pdf", width = 14, height = 10, units = "cm", dpi = 300)
ggsave("Congruency_tstat_global_con.png", width = 14, height = 10, units = "cm", dpi = 300)


## Plot by form
summary_form$roi <- factor(summary_form$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))
summary_form$form <- factor(summary_form$form, levels = c("circle*circle", "square*square","circle*square"))

form_bar <- ggplot(summary_form, aes(x = form, y = mean)) +
  
  geom_bar(stat="identity", position=position_dodge(), fill = "azure4") +
  
  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.15, position=position_dodge(0.9)) +
  
  ylab("r") +
  
  facet_wrap(vars(roi)) +
  
  scale_x_discrete(guide = guide_axis(angle = 45)) +

  scale_y_continuous(limits = c(0, 0.9), breaks=seq(0, 0.8, 0.2)) +
  
  # Remove dark background + Additional paramaters for displaying plot
  #guides(fill = FALSE) + 
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), axis.title.x = element_blank())

form_bar
ggsave("Form_tstat_global_con.pdf", width = 14, height = 10, units = "cm", dpi = 300)
ggsave("Form_tstat_global_con.png", width = 14, height = 10, units = "cm", dpi = 300)


## Plot by form_identical
summary_form_identical$roi <- factor(summary_form_identical$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))
#form_identical$form_identical <- factor(form_identical$form, levels = c("circle*circle", "square*square","circle*square"))

form_identical_bar <- ggplot(summary_form_identical, aes(x = form_identical, y = mean)) +
  
  geom_bar(stat="identity", position=position_dodge(), fill = "azure4") +
  
  geom_errorbar(aes(ymin=se_low, ymax=se_high), width=.15, position=position_dodge(0.9)) +
  
  ylab("r") +
  
  facet_wrap(vars(roi)) +
  
  scale_x_discrete(guide = guide_axis(angle = 45)) +

  scale_y_continuous(limits = c(0, 0.9), breaks=seq(0, 0.8, 0.2)) +
  
  # Remove dark background + Additional paramaters for displaying plot
  #guides(fill = FALSE) + 
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), axis.title.x = element_blank())

form_identical_bar
ggsave("Form_identical_tstat_global_con.pdf", width = 14, height = 10, units = "cm", dpi = 300)
ggsave("Form_identical_tstat_global_con.png", width = 14, height = 10, units = "cm", dpi = 300)
