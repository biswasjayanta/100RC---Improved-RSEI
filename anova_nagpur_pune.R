library(rstatix)
library(dplyr)
library(ggplot2)
df <- read.delim("/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/topical_long_pun_nag.txt", header=TRUE, sep = "\t")

# Make sure Year is treated as a factor
df <- df %>%
  mutate(Year = as.factor(Year),
         Group = as.factor(Group))

# Two-way ANOVA
anova_res <- anova_test(
  data = df,
  dv = RSEI,
  between = c(Group, Year)
)

anova_res

posthoc_group <- df %>%
  group_by(Year) %>%
  pairwise_t_test(RSEI ~ Group, p.adjust.method = "bonferroni")
posthoc_group

posthoc_year <- df %>%
  group_by(Group) %>%
  pairwise_t_test(RSEI ~ Year, p.adjust.method = "bonferroni")
posthoc_year

# ensure ordering on the x-axis
df <- df %>%
  mutate(
    Year = factor(Year, levels = c(2016, 2018, 2020, 2022, 2024)),
    Group = factor(Group, levels = c("RC","nonRC"))
  )

# Okabe–Ito palette (color-blind safe)
pal <- c("RC" = "#0072B2", "nonRC" = "#D55E00")

ggplot(df, aes(x = Year, y = RSEI,
               group = Group, color = Group,
               linetype = Group, shape = Group)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               width = 0.15, linewidth = 0.6) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2.8, stroke = 0.8, fill = "white") +
  scale_color_manual(values = pal, name = NULL) +
  scale_linetype_manual(values = c("solid","dashed"), name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  labs(
    title = "RSEI over Time: 100RC vs non-100RC (Pune vs Nagpur)",
    x = "Year",
    y = "Remote Sensing Ecological Index (RSEI)",
    caption = "Points/lines show means; error bars are 95% CIs from pixel-level samples."
  ) +
  theme_minimal(base_size = 12, base_family = "Palatino Linotype") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 6)),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1),
    plot.margin = margin(8, 12, 8, 8),
    plot.caption = element_text(hjust = 0.5, size = 9, face = "italic") # centered, small, italic
  )


# Save for journals (high DPI, modest size)
ggsave("RSEI_interaction_Pune_vs_Nagpur.png", width = 6.5, height = 5, dpi = 600)


