library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")

names(panel_final)

panel_final <- panel_final %>%
  mutate(
    new1_I_hg = (ccei_g - ccei_hg) / (ccei_g - ccei_hlg),
    new1_I_lg = (ccei_hg - ccei_hlg) / (ccei_g - ccei_hlg),
    new1_I_hg_end = (ccei_g_end - ccei_hg_end) / (ccei_g_end - ccei_hlg_end),
    new1_I_lg_end = (ccei_hg_end - ccei_hlg_end) / (ccei_g_end - ccei_hlg_end)
  )

plot_histogram_index <- function(df, variable, title_text) {
  breaks_seq <- seq(0, 1, by = 0.05)
  hist_detail <- hist(df[[variable]], breaks = breaks_seq, plot = FALSE)
  mid_detail <- hist_detail$mids
  count_detail <- hist_detail$counts
  prop_detail <- round(count_detail / sum(count_detail, na.rm = TRUE) * 100, 1)
  
  x_mean <- mean(df[[variable]], na.rm = TRUE)
  x_median <- median(df[[variable]], na.rm = TRUE)
  y_max <- max(count_detail, na.rm = TRUE)
  
  ggplot(df, aes_string(x = variable)) +
    geom_histogram(breaks = breaks_seq, fill = "gray70", color = "black") +
    geom_text(
      data = data.frame(x = mid_detail, y = count_detail,
                        label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
      aes(x = x, y = y * 0.6, label = label),
      size = 4, fontface = "bold"
    ) +
    geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
    geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
    annotate("text", x = x_mean, y = y_max * 1,
             label = paste0("mean = ", round(x_mean, 3)),
             color = "red", size = 4, hjust = 0.5) +
    annotate("text", x = x_median, y = y_max * 0.9,
             label = paste0("median = ", round(x_median, 3)),
             color = "blue", size = 4, hjust = 0.5) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      limits = c(0, y_max * 1.2),
      expand = c(0, 0)
    ) +
    labs(
      title = title_text,
      x = variable,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(10, 20, 10, 10),
      panel.grid.minor.x = element_blank()
    )
}

plot_histogram_index(panel_final, "new1_I_hg", bquote("Distribution of " ~ I[hg] ~ "(Baseline)"))
plot_histogram_index(panel_final, "new1_I_lg", bquote("Distribution of " ~ I[lg] ~ "(Baseline)"))
plot_histogram_index(panel_final, "new1_I_hg_end", bquote("Distribution of " ~ I[hg] ~ "(Endline)"))
plot_histogram_index(panel_final, "new1_I_lg_end", bquote("Distribution of " ~ I[lg] ~ "(Endline)"))

panel_final <- panel_final %>%
  select(-new1_I_hg, -new1_I_hg_end)
save(panel_final, file = "../results/panel_final.RData")


###############################################################33

panel_final <- panel_final %>%
  mutate(
    # baseline 방식 1 (hg 먼저)
    temp1_I_hg = (ccei_g - ccei_hg) / (ccei_g - ccei_hlg),
    temp1_I_lg = (ccei_hg - ccei_hlg) / (ccei_g - ccei_hlg),
    
    # baseline 방식 2 (lg 먼저)
    temp2_I_lg = (ccei_g - ccei_lg) / (ccei_g - ccei_hlg),
    temp2_I_hg = (ccei_lg - ccei_hlg) / (ccei_g - ccei_hlg),
    
    # baseline 평균
    new2_I_hg = (temp1_I_hg + temp2_I_hg) / 2,
    new2_I_lg = (temp1_I_lg + temp2_I_lg) / 2,
    
    # endline 방식 1 (hg 먼저)
    temp1_I_hg_end = (ccei_g_end - ccei_hg_end) / (ccei_g_end - ccei_hlg_end),
    temp1_I_lg_end = (ccei_hg_end - ccei_hlg_end) / (ccei_g_end - ccei_hlg_end),
    
    # endline 방식 2 (lg 먼저)
    temp2_I_lg_end = (ccei_g_end - ccei_lg_end) / (ccei_g_end - ccei_hlg_end),
    temp2_I_hg_end = (ccei_lg_end - ccei_hlg_end) / (ccei_g_end - ccei_hlg_end),
    
    # endline 평균
    new2_I_hg_end = (temp1_I_hg_end + temp2_I_hg_end) / 2,
    new2_I_lg_end = (temp1_I_lg_end + temp2_I_lg_end) / 2
  )


plot_histogram_index(panel_final, "new2_I_hg",      bquote("Distribution of " ~ I[hg]      ~ "(Order-averaged, Baseline)"))
plot_histogram_index(panel_final, "new2_I_lg",      bquote("Distribution of " ~ I[lg]      ~ "(Order-averaged, Baseline)"))
plot_histogram_index(panel_final, "new2_I_hg_end",  bquote("Distribution of " ~ I[hg] ~ "(Order-averaged, Endline)"))
plot_histogram_index(panel_final, "new2_I_lg_end",  bquote("Distribution of " ~ I[lg] ~ "(Order-averaged, Endline)"))

panel_final <- panel_final %>%
  select(-starts_with("temp1_"), -starts_with("temp2_"))

save(panel_final, file = "../results/panel_final.RData")

names(panel_final)


####################################################33

needed_vars <- c(
  "group_id", "id.x", "partner_id.x",
  "index_hg", "index_hg_end",
  "new1_I_lg", "new1_I_lg_end",
  "new2_I_hg", "new2_I_lg", "new2_I_hg_end", "new2_I_lg_end",
  "ccei_g", "ccei_hg", "ccei_lg", "ccei_hlg",
  "ccei_g_end", "ccei_hg_end", "ccei_lg_end", "ccei_hlg_end",
  "high", "high_end"
)


panel_final <- panel_final %>%
  select(all_of(needed_vars))
