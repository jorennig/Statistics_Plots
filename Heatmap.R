# Clear environment
rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)

pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# POF
data <- read.table("data_MVPA_tstat_correlation.txt", sep=",", header=T)
data$roi <- paste(data$roi, "ROI")

data <- data %>%
  group_by(subject_id, roi, hem) %>%
  mutate(n = n())

data <- data[data$n > 3, ]

corr_reshape <- function(group, selected_vars) {
  cor_matrix <- as.data.frame(cor(group[,selected_vars]))
  cor_matrix <- cbind(var1 = rownames(cor_matrix), cor_matrix)
  rownames(cor_matrix) <- NULL
  cor_matrix <- gather(cor_matrix, key="var2", value="r", all_of(selected_vars))
  cor_matrix <- cor_matrix[order(cor_matrix$var1),]
  return(cor_matrix)
}

cols <- c("congruent_circle", "congruent_square", "incongruent_circle", "incongruent_square")
correlation_data <- data %>% 
  group_by(subject_id, roi, hem) %>%
  group_modify(~corr_reshape(., cols))

data_summary <- correlation_data %>%
  group_by(roi, var1, var2) %>%
  summarise(r = mean(r, na.rm = T)) %>%
  ungroup()

data_summary$roi <- factor(data_summary$roi, levels = c('Objects ROI', 'Faces ROI', 'Places ROI'))

heatmap_global_con <- ggplot(data_summary, aes(x = var1, y = var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%0.2f", round(r, digits = 2))), color="black", size=3) +
  facet_wrap(vars(roi)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_fill_gradient(low = "white", high = "red", limits=c(0.199,1)) +
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), 
        axis.title.x = element_blank(), axis.title.y = element_blank())
heatmap_global_con

w = 26
h = w/2.8
ggsave("heatmap_global_con.pdf", width=w, height=h, units="cm", dpi=300)
ggsave("heatmap_global_con.png", width=w, height=h, units="cm", dpi=300)
