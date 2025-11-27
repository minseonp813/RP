##########################################################
# Written By Byunghun Hahn, 2025 08 25
# Replication Codes for Figures 1, 2, A1
##########################################################

# Figure 1 - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData") 

names(panel_final)

# calculate median value of individual ccei
ccei_long <- panel_final %>%
  select(group_id, ccei_1, ccei_2) %>%
  pivot_longer(
    cols = starts_with("ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    ccei_both_median = as.integer(ccei_1 >= ccei_median) +
      as.integer(ccei_2 >= ccei_median),
    group_label_base = factor(
      ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_baseline_plot <- ggplot(
  panel_final,
  aes(x = ccei_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective CCEI",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_1a.png",
  plot     = cdf_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

####################################################################

# Figure 1 - (b)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData")

# calculate median value of individual ccei
ccei_long <- panel_final %>%
  select(group_id, ccei_1_end, ccei_2_end) %>%
  pivot_longer(
    cols = starts_with("ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    ccei_both_median = as.integer(ccei_1_end >= ccei_median) +
      as.integer(ccei_2_end >= ccei_median),
    group_label_end = factor(
      ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_endline_plot <- ggplot(
  panel_final,
  aes(x = ccei_g_end,
      colour = group_label_end,
      linetype = group_label_end)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective CCEI",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_1b.png",
  plot     = cdf_endline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

#####################################################################

# Figure 2 - (a), (b)

# Note that 8 individuals whose Risk Aversion > 0.5 are excluded in the graph
# X axis is limited to (0, 0.5)

rm(list = ls())

library(ggplot2)
library(dplyr)

load("data/finalized_panel_individual.RData")

# cdf
plot_ra_diff <- function(data, time_val, output_name) {
  df <- data %>%
    filter(time == time_val) %>%
    mutate(RA_ig = abs(RA_i - RA_g))
  
  ggplot(df, aes(x = RA_ig, color = factor(high_dummy), linetype = factor(high_dummy))) +
    stat_ecdf(geom = "step", size = 1) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    labs(
      x = expression("|RA"["i"] - RA["g"]~"|"),
      y = "Cumulative Frequency",
      color = NULL,
      linetype = NULL
    ) +
    scale_x_continuous(
      limits = c(0, 0.5022),
      breaks = seq(0, 0.5, by = 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = c(0.65, 0.4),
      legend.justification = c("left", "top"),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA)
    ) -> plot
  
  ggsave(
    filename = output_name,
    plot     = plot,
    path     = "results",
    width    = 7, height = 5, dpi = 300
  )
}

plot_ra_diff(panel_individual, time_val = 0, output_name = "figure_2a.png")
plot_ra_diff(panel_individual, time_val = 1, output_name = "figure_2b.png")

#############################################################

# Figure 4 - (a), (b)

# Note that 102 individuals (100 at Endline) whose index cannot be defined are excluded.
# If CCEI_G equals CCEI_HLG, the index cannot be defined (denominator = 0).


rm(list = ls())

library(ggplot2)
library(dplyr)

load("data/finalized_panel_individual.RData")

plot_I_ccei <- function(data, time_val, output_name) {
  df <- data %>%
    filter(time == time_val)
  
  ggplot(df, aes(x = new2_I_ig, color = factor(high_dummy), linetype = factor(high_dummy))) +
    stat_ecdf(geom = "step", size = 1) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    labs(
      x = expression(I[ig]),
      y = "Cumulative Frequency",
      color = NULL,
      linetype = NULL
    ) +
    scale_x_continuous(
      limits = c(-0.0022, 1.0022),
      breaks = seq(0, 1.0, by = 0.2),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c("left", "top"),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA)
    ) -> plot
  
  ggsave(
    filename = output_name,
    plot     = plot,
    path     = "results",
    width    = 7, height = 5, dpi = 300
  )
}

plot_I_ccei(panel_individual, time_val = 0, output_name = "figure_4a.png")
plot_I_ccei(panel_individual, time_val = 1, output_name = "figure_4b.png")


##############################################################

# Figure A1 - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

ccei_baseline_long <- panel_final %>%
  select(ccei_1, ccei_2) %>%
  pivot_longer(cols = everything(), values_to = "ccei") %>%
  mutate(source = "Baseline")

ccei_endline_long <- panel_final %>%
  select(ccei_1_end, ccei_2_end) %>%
  pivot_longer(cols = everything(), values_to = "ccei") %>%
  mutate(source = "Endline")

ccei_combined <- bind_rows(ccei_baseline_long, ccei_endline_long)

mean_baseline <- mean(ccei_baseline_long$ccei, na.rm = TRUE)
mean_endline  <- mean(ccei_endline_long$ccei,  na.rm = TRUE)

F_baseline <- ecdf(ccei_baseline_long$ccei)
F_endline  <- ecdf(ccei_endline_long$ccei)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_ccei <- ggplot(
  ccei_combined,
  aes(x = ccei, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.03,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual CCEI",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1a.png",
  plot     = cdf_ccei,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

########################################

# Figure A1 - (b)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

ra_baseline_long <- panel_final %>%
  select(RA_1, RA_2) %>%
  pivot_longer(cols = everything(), values_to = "RA") %>%
  mutate(source = "Baseline")

ra_endline_long <- panel_final %>%
  select(RA_1_end, RA_2_end) %>%
  pivot_longer(cols = everything(), values_to = "RA") %>%
  mutate(source = "Endline")

ra_combined <- bind_rows(ra_baseline_long, ra_endline_long) %>%
  mutate(RA = ifelse(RA < 0 & RA > -1e-6, 0, RA))

mean_baseline <- mean(ra_baseline_long$RA, na.rm = TRUE)
mean_endline  <- mean(ra_endline_long$RA,  na.rm = TRUE)

F_baseline <- ecdf(ra_baseline_long$RA)
F_endline  <- ecdf(ra_endline_long$RA)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_RA <- ggplot(
  ra_combined,
  aes(x = RA, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.1,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual Risk Aversion",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1b.png",
  plot     = cdf_RA,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

########################################

# Figure A1 - (c)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

fccei_baseline_long <- panel_final %>%
  select(f_ccei_1, f_ccei_2) %>%
  pivot_longer(cols = everything(), values_to = "f_ccei") %>%
  mutate(source = "Baseline")

fccei_endline_long <- panel_final %>%
  select(f_ccei_1_end, f_ccei_2_end) %>%
  pivot_longer(cols = everything(), values_to = "f_ccei") %>%
  mutate(source = "Endline")

fccei_combined <- bind_rows(fccei_baseline_long, fccei_endline_long) %>%
  mutate(f_ccei = ifelse(f_ccei < 0 & f_ccei > -1e-6, 0, f_ccei))

mean_baseline <- mean(fccei_baseline_long$f_ccei, na.rm = TRUE)
mean_endline  <- mean(fccei_endline_long$f_ccei,  na.rm = TRUE)

F_baseline <- ecdf(fccei_baseline_long$f_ccei)
F_endline  <- ecdf(fccei_endline_long$f_ccei)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_f_ccei <- ggplot(
  fccei_combined,
  aes(x = f_ccei, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.03,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual F GARP",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1c.png",
  plot     = cdf_f_ccei,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)


#############################################

# Figure A1 - (d)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

mpi_baseline_long <- panel_final %>%
  select(max_mpi_1, max_mpi_2) %>%
  pivot_longer(cols = everything(), values_to = "max_mpi") %>%
  mutate(value = 1 - max_mpi,
         source = "Baseline")

mpi_endline_long <- panel_final %>%
  select(max_mpi_1_end, max_mpi_2_end) %>%
  pivot_longer(cols = everything(), values_to = "max_mpi") %>%
  mutate(value = 1 - max_mpi,
         source = "Endline")

# ← tiny negative 보정 단계 제거
mpi_combined <- bind_rows(mpi_baseline_long, mpi_endline_long)

mean_baseline <- mean(mpi_baseline_long$value, na.rm = TRUE)
mean_endline  <- mean(mpi_endline_long$value,  na.rm = TRUE)

F_baseline <- ecdf(mpi_baseline_long$value)
F_endline  <- ecdf(mpi_endline_long$value)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_mpi <- ggplot(
  mpi_combined,
  aes(x = value, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(x = mean_baseline, xend = mean_baseline, y = 0, yend = y_baseline,
               color = "blue", linetype = "dashed", size = 0.6) +
  geom_segment(x = mean_endline, xend = mean_endline, y = 0, yend = y_endline,
               color = "red", linetype = "dashed", size = 0.6) +
  annotate("text", x = mean_baseline, y = y_baseline + 0.05,
           label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
           hjust = 1, vjust = 0, color = "blue", size = 4) +
  annotate("text", x = mean_endline, y = y_endline + 0.03,
           label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
           hjust = 1, vjust = 0, color = "red", size = 4) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1d.png",
  plot     = cdf_mpi,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

#########################################################

# Figure A1 - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData") 

# calculate median value of individual FGARP ccei
ccei_long <- panel_final %>%
  select(group_id, f_ccei_1, f_ccei_2) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median = as.integer(f_ccei_1 >= ccei_median) +
      as.integer(f_ccei_2 >= ccei_median),
    group_label_base = factor(
      f_ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_f_baseline_plot <- ggplot(
  panel_final,
  aes(x = f_ccei_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective FGARP",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1a.png",
  plot     = cdf_f_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

# Figure A1 - (b)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData")

# calculate median value of individual FGARP ccei at endline
ccei_long <- panel_final %>%
  select(group_id, f_ccei_1_end, f_ccei_2_end) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median = as.integer(f_ccei_1_end >= ccei_median) +
      as.integer(f_ccei_2_end >= ccei_median),
    group_label_end = factor(
      f_ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_f_endline_plot <- ggplot(
  panel_final,
  aes(x = f_ccei_g_end,
      colour = group_label_end,
      linetype = group_label_end)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective FGARP",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1b.png",
  plot     = cdf_f_endline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

############################################

# Figure A1 - (c): MPI baseline

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData")

# transform variables: 1 - max_mpi
panel_final <- panel_final %>%
  mutate(
    one_minus_mpi_1    = 1 - max_mpi_1,
    one_minus_mpi_2    = 1 - max_mpi_2,
    one_minus_mpi_g    = 1 - max_mpi_g
  )

# median of individual (1 - max_mpi)
mpi_long <- panel_final %>%
  select(group_id, one_minus_mpi_1, one_minus_mpi_2) %>%
  pivot_longer(
    cols = starts_with("one_minus_mpi_"),
    names_to = "member",
    values_to = "mpi_val"
  )

mpi_median <- median(mpi_long$mpi_val, na.rm = TRUE)

# label groups
panel_final <- panel_final %>%
  mutate(
    mpi_both_median = as.integer(one_minus_mpi_1 >= mpi_median) +
      as.integer(one_minus_mpi_2 >= mpi_median),
    group_label_base = factor(
      mpi_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# plot CDF
cdf_mpi_baseline_plot <- ggplot(
  panel_final,
  aes(x = one_minus_mpi_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.0, 1.0022),
                     breaks = seq(0.0, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1c.png",
  plot     = cdf_mpi_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

# Figure A1 - (d): MPI endline

rm(list = ls())
load("data/finalized_panel_final.RData")

# transform variables: 1 - max_mpi (endline)
panel_final <- panel_final %>%
  mutate(
    one_minus_mpi_1_end = 1 - max_mpi_1_end,
    one_minus_mpi_2_end = 1 - max_mpi_2_end,
    one_minus_mpi_g_end = 1 - max_mpi_g_end
  )

# median of individual (1 - max_mpi_end)
mpi_long <- panel_final %>%
  select(group_id, one_minus_mpi_1_end, one_minus_mpi_2_end) %>%
  pivot_longer(
    cols = starts_with("one_minus_mpi_"),
    names_to = "member",
    values_to = "mpi_val"
  )

mpi_median <- median(mpi_long$mpi_val, na.rm = TRUE)

# label groups
panel_final <- panel_final %>%
  mutate(
    mpi_both_median = as.integer(one_minus_mpi_1_end >= mpi_median) +
      as.integer(one_minus_mpi_2_end >= mpi_median),
    group_label_end = factor(
      mpi_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# plot CDF
cdf_mpi_endline_plot <- ggplot(
  panel_final,
  aes(x = one_minus_mpi_g_end,
      colour = group_label_end,
      linetype = group_label_end)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.0, 1.0022),
                     breaks = seq(0.0, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1d.png",
  plot     = cdf_mpi_endline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

####################################
library(ggplot2)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Class FE"),
  Shapley = c(0.0215, 0.0718, 0.0258, 0.0763),
  Contribution = c(11.0, 36.7, 13.2, 39.0)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley, fill = Block)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Time" = "skyblue",
                               "CCEI" = "orange",
                               "Group/Friendship" = "green",
                               "Class FE" = "grey70")) +
  labs(title = expression("Shapley Decomposition of " ~ R^2),
       y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_v1.png", width = 6, height = 5, dpi = 300)





library(ggplot2)
library(ggpattern)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Class FE"),
  Shapley = c(0.0215, 0.0718, 0.0258, 0.0763),
  Contribution = c(11.0, 36.7, 13.2, 39.0)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley,
               fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   # aesthetic 값 매핑은 scale_*_manual로 빼기
                   pattern_angle = c(-45, -45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group/Friendship" = "pink",
               "Class FE" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group/Friendship" = "stripe",
               "Class FE" = "none")
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_v2.png", width = 6, height = 5, dpi = 300)

######################################################

library(ggplot2)
library(dplyr)

##############################################
# Group A: Friend, Gender
##############################################
dfA <- data.frame(
  xpos = factor(c("Friend: No","Friend: Mutual",
                  "Gender: Different","Gender: Same"),
                levels=c("Friend: No","Friend: Mutual",
                         "Gender: Different","Gender: Same")),
  coef = c(-0.439, -0.475, -0.423, -0.449),
  ci_low = c(-0.474, -0.541, -0.497, -0.481),
  ci_high = c(-0.403, -0.410, -0.349, -0.417),
  stars = c("**","**","**","**"),
  group = "normal"
)

dfA <- dfA %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2))

block_labels_A <- data.frame(
  xpos=c(1.5, 3.5),
  ypos=-0.85,
  label=c("Friend","Gender")
)

pA <- ggplot(dfA, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.05, color="black") +
  geom_point(shape=16, size=3, color="black") +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust), vjust=-0.5, size=3.5) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("No","Mutual","Different","Same")) +
  geom_text(data=block_labels_A, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_A_ver1.png", pA, width=6, height=4, dpi=300)


##############################################
# Group B: Outgoing, Valuetrust, Valuecowork
##############################################
dfB <- data.frame(
  xpos=factor(c("Outgoing: Low","Outgoing: High",
                "Valuetrust: Low","Valuetrust: High",
                "Valuecowork: Low","Valuecowork: High"),
              levels=c("Outgoing: Low","Outgoing: High",
                       "Valuetrust: Low","Valuetrust: High",
                       "Valuecowork: Low","Valuecowork: High")),
  coef=c(-0.438, -0.461, -0.434, -0.585, -0.440, -0.565),
  ci_low=c(-0.470, -0.526, -0.464, -0.693, -0.470, -0.773),
  ci_high=c(-0.405, -0.396, -0.403, -0.478, -0.409, -0.358),
  stars=c("**","**","**","**","**","**"),
  group=c("normal","normal","trust","trust","normal","normal")
)

dfB <- dfB %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2),
         vjust=ifelse(xpos=="Outgoing: High", 1.5, -0.5))

block_labels_B <- data.frame(
  xpos=c(1.5, 3.5, 5.5),
  ypos=-0.85,
  label=c("Outgoing","Value Trust","Value Cowork")
)

pB <- ggplot(dfB, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high,
                    color=group, linetype=group), width=0.05) +
  geom_point(aes(shape=group, color=group), size=3) +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust, vjust=vjust), size=3.5) +
  annotate("text", x=3.5, y=min(dfB$ci_low[dfB$group=="trust"])-0.02,
           label="p=0.005", size=4, color="red") +
  scale_color_manual(values=c("normal"="black","trust"="red")) +
  scale_shape_manual(values=c("normal"=16,"trust"=18)) +
  scale_linetype_manual(values=c("normal"="solid","trust"="solid")) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("Low","High","Low","High","Low","High")) +
  geom_text(data=block_labels_B, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_B_ver1.png", pB, width=6, height=4, dpi=300)


##############################################
# Group C: Pbl share idea, Teacher induce, Class participate
##############################################
dfC <- data.frame(
  xpos=factor(c("Pbleffectsharidea: Low","Pbleffectsharidea: High",
                "Teacherinduce: Low","Teacherinduce: High",
                "Classparticipate: Low","Classparticipate: High"),
              levels=c("Pbleffectsharidea: Low","Pbleffectsharidea: High",
                       "Teacherinduce: Low","Teacherinduce: High",
                       "Classparticipate: Low","Classparticipate: High")),
  coef=c(-0.433,-0.529,-0.438,-0.485,-0.436,-0.508),
  ci_low=c(-0.467,-0.609,-0.472,-0.563,-0.470,-0.597),
  ci_high=c(-0.399,-0.448,-0.405,-0.407,-0.401,-0.420),
  stars=c("**","**","**","**","**","**"),
  group=c("share","share","normal","normal","normal","normal")
)

dfC <- dfC %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2),
         vjust=ifelse(id %% 2 == 0, 1.5, -0.5))

block_labels_C <- data.frame(
  xpos=c(1.5,3.5,5.5),
  ypos=-0.85,
  label=c("PBL Share Idea","Teacher Induce","Class Participate")
)

pC <- ggplot(dfC, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high,
                    color=group, linetype=group), width=0.05) +
  geom_point(aes(shape=group, color=group), size=3) +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust, vjust=vjust), size=3.5) +
  annotate("text", x=1.5, y=min(dfC$ci_low[dfC$group=="share"])-0.02,
           label="p=0.040", size=4, color="red") +
  scale_color_manual(values=c("normal"="black","share"="red")) +
  scale_shape_manual(values=c("normal"=16,"share"=18)) +
  scale_linetype_manual(values=c("normal"="solid","share"="solid")) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("Low","High","Low","High","Low","High")) +
  geom_text(data=block_labels_C, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_C_ver1.png", pC, width=6, height=4, dpi=300)
