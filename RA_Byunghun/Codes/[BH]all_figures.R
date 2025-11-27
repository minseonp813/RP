rm(list = ls())
load("../results/panel_final.RData")

library(ggplot2)
library(dplyr)

# 그룹 라벨: 0 → "Low, Low", 1 → "Low, High", 2 → "High, High"
panel_final <- panel_final %>%
  mutate(group_label = factor(
    ccei_both_median,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

# CDF plot
cdf_baseline_plot <- ggplot(panel_final, aes(x = ccei_g, color = group_label, linetype = group_label)) +
  stat_ecdf(geom = "step", size = 1) +   # 여기 size 값을 1.5 로 변경
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
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
  )


ggsave(
  filename = "cdf_baseline_median.png",
  path = "C:/Users/hahn0/Desktop/RP Slides",
  width = 7, height = 5, dpi = 300
)

#################################################

# 그룹 라벨 생성 (endline 기준)
panel_final <- panel_final %>%
  mutate(group_label_end = factor(
    ccei_both_median_end,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

# CDF plot (endline)
cdf_endline_plot <- ggplot(panel_final, aes(x = ccei_g_end, color = group_label_end, linetype = group_label_end)) +
  stat_ecdf(geom = "step", size = 1) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),         # 항목 높이 조절
    legend.text = element_text(size = 12),        # 텍스트 크기 증가
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 저장
ggsave(
  filename = "cdf_endline_median.png",
  plot = cdf_endline_plot,
  path = "C:/Users/hahn0/Desktop/RP Slides",
  width = 7, height = 5, dpi = 300
)

######################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")

# 거리 계산
panel_final <- panel_final %>%
  mutate(
    RT_h_dist_base = abs(RT_h_baseline - RT_g_baseline),
    RT_l_dist_base = abs(RT_l_baseline - RT_g_baseline),
    RT_h_dist_end  = abs(RT_h_endline - RT_g_endline),
    RT_l_dist_end  = abs(RT_l_endline - RT_g_endline)
  )

# 데이터 변환 (baseline / endline)
df_base <- panel_final %>%
  select(group_id, RT_h_dist_base, RT_l_dist_base) %>%
  pivot_longer(cols = c(RT_h_dist_base, RT_l_dist_base),
               names_to = "type", values_to = "value") %>%
  mutate(time = "Baseline")

df_end <- panel_final %>%
  select(group_id, RT_h_dist_end, RT_l_dist_end) %>%
  pivot_longer(cols = c(RT_h_dist_end, RT_l_dist_end),
               names_to = "type", values_to = "value") %>%
  mutate(time = "Endline")

# 결합 및 label 정리
df_all <- bind_rows(df_base, df_end) %>%
  mutate(
    type = recode(type,
                  "RT_h_dist_base" = "RT_h",
                  "RT_l_dist_base" = "RT_l",
                  "RT_h_dist_end"  = "RT_h",
                  "RT_l_dist_end"  = "RT_l")
  )

# 범례 이름 설정
legend_labels <- c(
  expression(paste("|RT"["h"], " - RT"["g"], "|")),
  expression(paste("|RT"["l"], " - RT"["g"], "|"))
)

# Baseline plot
cdf_baseline_plot <- ggplot(df_all %>% filter(time == "Baseline"), 
                            aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 1) +
  scale_color_manual(
    values = c("red", "blue"),
    labels = legend_labels
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = legend_labels
  ) +
  labs(
    x = expression("|RT - RT"["g"] ~ "|"),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )

# Endline plot
cdf_endline_plot <- ggplot(df_all %>% filter(time == "Endline"), 
                           aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 1) +
  scale_color_manual(
    values = c("red", "blue"),
    labels = legend_labels
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = legend_labels
  ) +
  labs(
    x = expression("|RT - RT"["g"] ~ "|"),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(
  filename = "rt_diff_baseline.png",
  plot = cdf_baseline_plot,
  path = "C:/Users/hahn0/Desktop/RP Slides",
  width = 7, height = 5, dpi = 300
)

ggsave(
  filename = "rt_diff_endline.png",
  plot = cdf_endline_plot,
  path = "C:/Users/hahn0/Desktop/RP Slides",
  width = 7, height = 5, dpi = 300
)
