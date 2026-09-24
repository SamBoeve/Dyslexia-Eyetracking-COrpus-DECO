library("tidyverse")
library("lme4")
library("lmtest")
library("poolr")
library("gt")
library("showtext")
library('viridis')
library('hrbrthemes')
library("nortest")
library('EnvStats')
library('qqplotr')
library('psych')
library('RColorBrewer')
library('colorspace')
library('gghalves')

font_add_google('Roboto Mono')
showtext_auto()

# 2. Summary Statistics ------------------------------------------------------

## ---- Data ----

df <- read_rds("ET Data/DECO.rds")
class <- sapply(df, class)

# convert columns to numeric
cols <- colnames(df)
num_cols <- cols[12:58]
df <- df %>%
  mutate(across(all_of(num_cols), ~ as.numeric(replace(as.character(.x), .x == ".", NA))))

# check NAs and zero values 
print(sum(is.na(df$WORD_TOTAL_READING_TIME)))
print(sum(df$WORD_TOTAL_READING_TIME == 0, na.rm = TRUE))
print(sum(df$WORD_TOTAL_READING_TIME == '.', na.rm = TRUE))

print(sum(is.na(df$WORD_GAZE_DURATION)))
print(sum(df$WORD_GAZE_DURATION == 0, na.rm = TRUE))
print(sum(df$WORD_GAZE_DURATION == '.', na.rm = TRUE))

print(sum(is.na(df$WORD_FIRST_FIXATION_DURATION)))
print(sum(df$WORD_FIRST_FIXATION_DURATION == 0, na.rm = TRUE))
print(sum(df$WORD_FIRST_FIXATION_DURATION == '.',na.rm = TRUE))

# prepare data for analysis
data <- df%>%
  rename(FFD = WORD_FIRST_FIXATION_DURATION, 
         GD = WORD_GAZE_DURATION, 
         TRT = WORD_TOTAL_READING_TIME, 
         GPT = WORD_GO_PAST_TIME,
         SKIP = WORD_SKIP)%>%
  mutate(SFD = if_else(WORD_FIXATION_COUNT == 1, FFD, NA_real_))%>% # create single fixation duration
  group_by(PART, TRIAL, LANGUAGE, PP_NR)%>%
  mutate(TRIAL_PROP = WORD_ID_WITHIN_TRIAL / max(WORD_ID_WITHIN_TRIAL, na.rm = TRUE))%>%
  ungroup%>%
  select(PART, TRIAL, WORD_ID_WITHIN_TRIAL, TRIAL_PROP, WORD_ID, LANGUAGE, LANGUAGE_RANK, PP_NR,
         WORD, FFD, SFD, GD, TRT, GPT, SKIP)%>%
  filter(!PP_NR == 1) # remove the first participant (too low comprehension score)

# put all ind. fixation durations shorter than 100 to NA
trimdata <- data %>%
  mutate(LANGUAGE = factor(LANGUAGE, levels = c("Dutch","English"))) %>%
  mutate(LANGUAGE_RANK = factor(LANGUAGE_RANK, levels = c("L1","L2"))) %>%
  mutate(
    FFD = if_else(FFD >= 100, FFD, NA_real_),
    SFD = if_else(SFD >= 100, SFD, NA_real_),
    GD  = if_else(GD  >= 100, GD,  NA_real_),
    GPT = if_else(GPT  >= 100, GPT,  NA_real_),
    TRT = if_else(TRT == 0, NA_real_, TRT), # TRT of 0 -> NA
    TRT = if_else(TRT >= 100, TRT, NA_real_)
    ) %>% 
  filter(!(SKIP == 1)) %>%
  group_by(PP_NR, LANGUAGE) %>%
  mutate(
    across(
      .cols = c(FFD, SFD, GD, TRT, GPT),
      .fns = ~ if_else(.x <= mean(.x, na.rm = TRUE) + 2.5 * sd(.x, na.rm = TRUE),
                       .x, NA_real_))) %>%
  ungroup()
  
# count number of observations
n_table <- trimdata %>%
  group_by(LANGUAGE_RANK) %>%
  summarise(
    across(
      .cols = c(FFD, SFD, GD, TRT, GPT),
      .fns = ~ sum(!is.na(.x)),
      .names = "{.col}_n"
    ),
    N_participants = n_distinct(PP_NR)  # adjust to your actual participant ID column
  ) %>%
  mutate(
    LANGUAGE_RANK = recode(
      LANGUAGE_RANK,
      "L1" = "L1 (Dutch)",
      "L2" = "L2 (English)"
    )
  )

print(n_table)

# percentage of data removed
measures <- c("FFD", "SFD", "GD", "TRT", "GPT")

# per-measure non-missing counts, before and after, split by language
n_before <- data %>%
  group_by(LANGUAGE) %>%
  summarise(across(all_of(measures), ~ sum(!is.na(.x)), .names = "{.col}")) %>%
  pivot_longer(-LANGUAGE, names_to = "Measure", values_to = "N_before")

n_after <- trimdata %>%
  group_by(LANGUAGE) %>%
  summarise(across(all_of(measures), ~ sum(!is.na(.x)), .names = "{.col}")) %>%
  pivot_longer(-LANGUAGE, names_to = "Measure", values_to = "N_after")

data_loss <- left_join(n_before, n_after, by = c("LANGUAGE", "Measure")) %>%
  mutate(
    N_removed   = N_before - N_after,
    Pct_removed = round(N_removed / N_before * 100, 2)
  )

print(data_loss)

### ---- Table 3 ----

# DECO data 
summary_table <- trimdata %>%
  group_by(LANGUAGE_RANK) %>%
  summarise(
    across(
      .cols = c(FFD, SFD, GD, TRT, GPT),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        min  = ~ min(.x, na.rm = TRUE),
        max  = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"))%>%
  mutate(
    LANGUAGE_RANK = recode(
      LANGUAGE_RANK,
      "L1" = "L1 (Dutch)",
      "L2" = "L2 (English)")) %>%
  select(
    LANGUAGE_RANK,
    FFD_mean, FFD_sd, FFD_min, FFD_max,
    SFD_mean, SFD_sd, SFD_min, SFD_max,
    GD_mean,  GD_sd,  GD_min,  GD_max,
    TRT_mean, TRT_sd, TRT_min, TRT_max,
    GPT_mean, GPT_sd, GPT_min, GPT_max
  ) %>%
  pivot_longer(
    -LANGUAGE_RANK,
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  mutate(ET = rep(c("First-fixation duration",
                    "Single-fixation duration",
                    "Gaze duration",
                    "Total reading time",
                    "Go past time"),
                  each = 4,
                  times = 2)) %>%
  mutate(
    Stat = rep(
      c("Mean", "SD", "Min", "Max"),
      times = 10)) %>%
  select(-Measure) %>%
  pivot_wider(
    names_from = c(LANGUAGE_RANK, Stat),
    values_from = Value) %>%
  select(-matches("_Min")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))%>%
  mutate(Corpus = 'DECO')

# t-test L1-L2
avg_pp <- trimdata %>%
  group_by(PP_NR, LANGUAGE_RANK)%>%
  summarise(FFD_mean = mean(FFD, na.rm = TRUE),
            SFD_mean = mean(SFD, na.rm = TRUE),
            GD_mean  = mean(GD, na.rm = TRUE),
            TRT_mean = mean(TRT, na.rm = TRUE),
            GPT_mean = mean(GPT, na.rm = TRUE))

### new
# one row per participant
wide_pp <- avg_pp %>%
  pivot_wider(
    id_cols = PP_NR,
    names_from = LANGUAGE_RANK,
    values_from = c(FFD_mean, SFD_mean, GD_mean, TRT_mean, GPT_mean)
  )

# compute paired differences
diffs <- wide_pp %>%
  transmute(
    PP_NR,
    FFD_diff = FFD_mean_L1 - FFD_mean_L2,
    SFD_diff = SFD_mean_L1 - SFD_mean_L2,
    GD_diff  = GD_mean_L1  - GD_mean_L2,
    TRT_diff = TRT_mean_L1 - TRT_mean_L2,
    GPT_diff = GPT_mean_L1 - GPT_mean_L2
  )

diff_cols <- c("FFD_diff", "SFD_diff", "GD_diff", "TRT_diff", "GPT_diff")

normality_results <- lapply(diff_cols, function(col) {
  d <- diffs[[col]]
  d <- d[!is.na(d)]
  sw <- shapiro.test(d)
  tibble(
    Measure   = sub("_diff", "", col),
    N         = length(d),
    Shapiro_W = unname(sw$statistic),
    p_value   = sw$p.value,
    Normal    = sw$p.value > .05
  )
}) %>% bind_rows()

print(normality_results)

par(mfrow = c(2, 3))  # grid of plots
for (col in diff_cols) {
  d <- diffs[[col]]
  d <- d[!is.na(d)]
  qqnorm(d, main = paste("QQ plot:", sub("_diff", "", col)))
  qqline(d, col = "red")
}
par(mfrow = c(1, 1))

####

t1 <- t.test(avg_pp$FFD_mean[avg_pp$LANGUAGE_RANK == 'L1'], avg_pp$FFD_mean[avg_pp$LANGUAGE_RANK == 'L2'], paired = TRUE)
t2 <-t.test(avg_pp$SFD_mean[avg_pp$LANGUAGE_RANK == 'L1'], avg_pp$SFD_mean[avg_pp$LANGUAGE_RANK == 'L2'], paired = TRUE)
t3 <-t.test(avg_pp$GD_mean[avg_pp$LANGUAGE_RANK == 'L1'], avg_pp$GD_mean[avg_pp$LANGUAGE_RANK == 'L2'], paired = TRUE)
t4 <-t.test(avg_pp$TRT_mean[avg_pp$LANGUAGE_RANK == 'L1'], avg_pp$TRT_mean[avg_pp$LANGUAGE_RANK == 'L2'], paired = TRUE)
t5 <-t.test(avg_pp$GPT_mean[avg_pp$LANGUAGE_RANK == 'L1'], avg_pp$GPT_mean[avg_pp$LANGUAGE_RANK == 'L2'], paired = TRUE)

t_list <- list(
  paste0(round(t1$statistic, 2), " [", round(t1$parameter, 1), "]",
         if (t1$p.value < 0.001) "***" else if (t1$p.value < 0.01) "**" else if (t1$p.value < 0.05) "*" else if (t1$p.value < 0.1) "." else ""),
  paste0(round(t2$statistic, 2), " [", round(t2$parameter, 1), "]",
         if (t2$p.value < 0.001) "***" else if (t2$p.value < 0.01) "**" else if (t2$p.value < 0.05) "*" else if (t2$p.value < 0.1) "." else ""),
  paste0(round(t3$statistic, 2), " [", round(t3$parameter, 1), "]",
         if (t3$p.value < 0.001) "***" else if (t3$p.value < 0.01) "**" else if (t3$p.value < 0.05) "*" else if (t3$p.value < 0.1) "." else ""),
  paste0(round(t4$statistic, 2), " [", round(t4$parameter, 1), "]",
         if (t4$p.value < 0.001) "***" else if (t4$p.value < 0.01) "**" else if (t4$p.value < 0.05) "*" else if (t4$p.value < 0.1) "." else ""),
  paste0(round(t5$statistic, 2), " [", round(t5$parameter, 1), "]",
         if (t5$p.value < 0.001) "***" else if (t5$p.value < 0.01) "**" else if (t5$p.value < 0.05) "*" else if (t5$p.value < 0.1) "." else "")
)

t_vector <- unlist(t_list)

summary_side <- summary_table %>%
  rename_with(~paste0("DECO_", .), -ET) %>%   # Prefix DECO columns
  select(-c(DECO_Corpus))%>%
  mutate(DECO_tvalue = t_vector)%>%
  relocate(
    DECO_tvalue,
    .after = "DECO_L2 (English)_Max"
  )

gt_table_nested <- summary_side %>%
  gt(rowname_col = "ET") %>%
  tab_header(
    title = "Table 3",
    subtitle = md("Averages, standard deviation (SD), and maximal values for the 
    reading time measures for L1 (Dutch) and L2 (English) reading. 
    The result of the *t*-test comparing L1 and L2 reading in DECO is presented in the *t*-value column with degrees of freedom in brackets.
    Statistical significance is indicated by asterisks: p < .05 (\\*), p < .01 (\\*\\*), and p < .001 (\\*\\*\\*)."
    )) %>%
  tab_spanner(
    label = "L1",
    columns = c(
      "DECO_L1 (Dutch)_Mean", 
      "DECO_L1 (Dutch)_SD", 
      "DECO_L1 (Dutch)_Max"
    ),
    id = "DECO_L1_spanner"
  ) %>%
  tab_spanner(
    label = "L2",
    columns = c(
      "DECO_L2 (English)_Mean",
      "DECO_L2 (English)_SD",
      "DECO_L2 (English)_Max",
    ),
    id = "DECO_L2_spanner"
  ) %>%
  tab_spanner(
    label = md("*t* Value (L1-L2)"),
    columns = "DECO_tvalue",
    id = "sep"
  )%>%
  tab_spanner(
    label = "DECO",
    columns = starts_with("DECO"),
    id = "DECO_spanner"
  ) %>%
  cols_label(
    "DECO_L1 (Dutch)_Mean" = "Mean",
    "DECO_L1 (Dutch)_SD"   = "SD",
    "DECO_L1 (Dutch)_Max"  = "Max",
    
    "DECO_L2 (English)_Mean" = "Mean",
    "DECO_L2 (English)_SD"   = "SD",
    "DECO_L2 (English)_Max"  = "Max",
    "DECO_tvalue"            = md("*t* [df]"),
  ) %>%
  cols_align(
    align = "center",
    columns = !ET  # all numeric columns
  )%>%
  tab_options(
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = everything() %>% setdiff("ET")  # all columns except the row names
    )
  ) %>%
  opt_table_font(
    font = "Garamond",
    size = 15
  )

gt_table_nested
gtsave(gt_table_nested, "Table3.docx", "Figures")

## ---- Summary plots ----

measure_colors <- RColorBrewer::brewer.pal(5, "Set2")
names(measure_colors) <- c("FFD", "SFD", "GD", "TRT", "GPT")

plot_raw <- trimdata %>%
  pivot_longer(
    cols = c(FFD, SFD, GD, TRT, GPT),
    names_to = "Measure",
    values_to = "RT") %>%
  filter(!is.na(RT)) %>%
  mutate(
    Measure = factor(Measure, levels = c("FFD", "SFD", "GD", "TRT", "GPT")),
    LANGUAGE = case_when(
      LANGUAGE == "Dutch"   ~ "L1",
      LANGUAGE == "English" ~ "L2")) %>%
  select(Measure, RT, LANGUAGE, PP_NR)%>%
  mutate(
    base_color = measure_colors[Measure],
    fill_color = ifelse(LANGUAGE == "L1",
                        lighten(base_color, amount = 0.3),
                        darken(base_color, 0.3)))


plot_raw$LANGUAGE <- factor(plot_raw$LANGUAGE, levels = c("L1", "L2"))

participant_means <- plot_raw %>%
  group_by(PP_NR, Measure, LANGUAGE) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop")%>%
  mutate(fill_color = 'black')

plot_log <- plot_raw %>%
  mutate(RT = log(RT))#log-transform

participant_means_log <- plot_log %>%
  group_by(PP_NR, Measure, LANGUAGE) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop")%>%
  mutate(fill_color = 'black')
  
plot_log_cent <- plot_log %>%
  mutate(RT = scale(RT)[,1]) #standardize

### ---- distribution of reading times ----

ggplot(plot_raw, aes(x = Measure, y = RT, fill = fill_color)) +
  theme_minimal(
    base_size = 30,
    base_family = "Roboto Mono"
  ) +
  geom_half_violin(
    data = subset(plot_raw, LANGUAGE == "L1"),
    position = position_nudge(x = -0.20),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  geom_half_violin(
    data = subset(plot_raw, LANGUAGE == "L2"),
    position = position_nudge(x = 0.25),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  geom_line(
    data = participant_means %>%
      pivot_wider(names_from = LANGUAGE, values_from = mean_RT) %>%
      pivot_longer(cols = c(L1, L2), names_to = "LANGUAGE", values_to = "mean_RT"),
    aes(
      x = as.numeric(factor(Measure)) + ifelse(LANGUAGE == "L1",-0.25,0.20),
      y = mean_RT,
      group = interaction(PP_NR, Measure)
    ),
    color = "black",
    alpha = 0.1, 
    position=position_jitter(w=0, h=1, seed = 123)
  ) +
  geom_point(
    data = participant_means,
    aes(
      x = as.numeric(factor(Measure)) + ifelse(LANGUAGE == "L1",-0.25,0.20),
      y = mean_RT
    ),
    size = 0.5,
    color = "black",
    alpha = 0.3, 
    position=position_jitter(w=0, h=1, seed = 123)
  )+
  scale_fill_identity() +  
  labs(y = "Reading time (ms)", x = NULL) +
  theme(
    legend.position = "none", 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1, color = "grey"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave('Figures/RT_dist_raw_revised2.tiff', width = 20, height = 13, unit = 'cm', dpi=300)


### ---- Distribution of reading times - log transform ----

ggplot(plot_log, aes(x = Measure, y = RT, fill = fill_color)) +
  theme_minimal(
    base_size = 30,
    base_family = "Roboto Mono"
  ) +
  geom_half_violin(
    data = subset(plot_log, LANGUAGE == "L1"),
    position = position_nudge(x = -0.20),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  geom_half_violin(
    data = subset(plot_log, LANGUAGE == "L2"),
    position = position_nudge(x = 0.25),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  geom_line(
    data = participant_means_log %>%
      pivot_wider(names_from = LANGUAGE, values_from = mean_RT) %>%
      pivot_longer(cols = c(L1, L2), names_to = "LANGUAGE", values_to = "mean_RT"),
    aes(
      x = as.numeric(factor(Measure)) + ifelse(LANGUAGE == "L1",-0.25,0.20),
      y = mean_RT,
      group = interaction(PP_NR, Measure)
    ),
    color = "black",
    alpha = 0.1
  ) +
  geom_point(
    data = participant_means_log,
    aes(
      x = as.numeric(factor(Measure)) + ifelse(LANGUAGE == "L1",-0.25,0.20),
      y = mean_RT
    ),
    size = 0.5,
    color = "black",
    alpha = 0.3
  )+
  scale_fill_identity() +  
  labs(y = "Reading time (log ms)", x = NULL) +
  theme(
    legend.position = "none", 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1, color = "grey"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave('Figures/RT_dist_log_revised3.tiff', width = 20, height = 13, unit = 'cm', dpi=300)


### ---- QQ - plots ----

stats <- plot_log_cent %>%
  group_by(Measure, LANGUAGE) %>%
  summarise(
    L = lillie.test(RT)$statistic,
    p = lillie.test(RT)[["p.value"]],
    G = skewness(RT, na.rm = TRUE)
  )

x_pos <- c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5)

stats$x_pos <- x_pos

ggplot(plot_log_cent, aes(sample = RT)) +
  stat_qq_line(color = "grey") +
  stat_qq_point(color = "black", size = 1) +
  facet_grid(LANGUAGE ~ Measure) +
  geom_text(
    data = stats,  
    aes(
      x = x_pos, 
      y = -2.5,  # adjust as needed for your plot's scale
      label = paste0("L = ", round(L, 2), "\nG = ", round(G, 2))
    ),
    inherit.aes = FALSE,
    size = 10, 
    lineheight = 0.5, 
    family = 'Roboto Mono')+
  labs(
    x = "Standard normal quantiles",
    y = "Log duration quantiles"
  ) +
  theme_minimal(base_size = 30, 
                base_family = 'Roboto Mono')+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1, color = 'grey'),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave('Figures/QQ_log2.tiff', width = 22, height = 15, unit = 'cm', dpi=300)

## ---- Split half reliability  ----

# Split half correlations for half the novel separately
# because different participants read parts 1&2 and parts 3&4
measures <- c("FFD", "SFD", "GD", "TRT", "GPT")

compute_sh <- function(data, measure) {
  
  mat <- data %>%
    select(WORD_ID, PP_NR, all_of(measure)) %>%
    pivot_wider(names_from = PP_NR, values_from = all_of(measure)) %>%
    select(-WORD_ID)
  
  tibble(
    mean_r = 2 * splitHalf(mat)$meanr / (1 + splitHalf(mat)$meanr),   # average split-half reliability + correction
    max_r  = 2 * splitHalf(mat)$maxrb / (1 + splitHalf(mat)$maxrb),   # maximum split-half reliability + correction
    min_r  = 2 * splitHalf(mat)$minrb / (1 + splitHalf(mat)$minrb),    # minimum split-half reliability + correction
    CI_low = 2 * unname(splitHalf(mat, raw = TRUE)$ci[1]) / (1 + unname(splitHalf(mat, raw = TRUE)$ci[1])),
    CI_high = 2 * unname(splitHalf(mat, raw = TRUE)$ci[3]) / (1 + unname(splitHalf(mat, raw = TRUE)$ci[3])) 
  )
}

# test
data_L1_p1 <- trimdata %>%
  filter(LANGUAGE == "Dutch", PART %in% c(1, 2))

mat_FFD <- data_L1_p1 %>%
  select(WORD_ID, PP_NR, FFD) %>%
  pivot_wider(names_from = PP_NR, values_from = FFD) %>%
  select(-WORD_ID)

check <- splitHalf(mat_FFD, raw = TRUE)
unname(check$ci[1])
check$ci[3]

# Dutch

# first half of the novel
data_L1_p1 <- trimdata %>%
  filter(LANGUAGE == "Dutch", PART %in% c(1, 2))
rel_L1_p1 <- as.data.frame(t(sapply(measures, compute_sh, data = data_L1_p1)))
rel_L1_p1[] <- as.data.frame(lapply(rel_L1_p1, as.numeric))

# second part of the novel
data_L1_p2 <- trimdata %>%
  filter(LANGUAGE == "Dutch", PART %in% c(3, 4))
rel_L1_p2 <- as.data.frame(t(sapply(measures, compute_sh, data = data_L1_p2)))
rel_L1_p2[] <- as.data.frame(lapply(rel_L1_p2, as.numeric))


reliability_L1 <- tibble(
  measure = rownames(rel_L1_p1),
  mean_r = (rel_L1_p1$mean_r + rel_L1_p2$mean_r) / 2
)

# English 
data_L2_p1 <- trimdata %>%
  filter(LANGUAGE == "English", PART %in% c(1, 2))
rel_L2_p1 <- as.data.frame(t(sapply(measures, compute_sh, data = data_L2_p1)))
rel_L2_p1[] <- as.data.frame(lapply(rel_L2_p1, as.numeric))

data_L2_p2 <- trimdata %>%
  filter(LANGUAGE == "English", PART %in% c(3, 4))
rel_L2_p2 <- as.data.frame(t(sapply(measures, compute_sh, data = data_L2_p2)))
rel_L2_p2[] <- as.data.frame(lapply(rel_L2_p2, as.numeric))

reliability_L2 <- tibble(
  measure = rownames(rel_L2_p1),
  mean_r = (rel_L2_p1$mean_r + rel_L2_p2$mean_r) / 2
)


### ---- Table 6  ----

reliability <- tibble(
  Measure = gsub("_trim$", "", reliability_L1$measure),  # remove "_trim"
  "L1 (Dutch)" = reliability_L1$mean_r,                # numeric vector
  "L2 (English)" = reliability_L2$mean_r               # numeric vector
)

reliability <- reliability %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))%>%
  mutate(Measure = recode(
    Measure,
    "FFD" = "First-fixation duration", 
    "SFD" = "Single-fixation duration", 
    "GD" = "Gaze duration", 
    "TRT" = "Total reading time", 
    "GPT" = "Go past time"
  ))

table_6 <- reliability %>%
  gt() %>%
  tab_header(
    title = "Table 6",
    subtitle = "Spearman-Brown split-half reliability coefficients for L1 (Dutch) and L2 (English) reading."
  ) %>%
  cols_label(
    Measure = ""
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  tab_options(
    heading.align = "left", 
    table.width = pct(40)
  ) %>%
  tab_style(
    style = cell_text(size = px(15)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  opt_table_font(
    font = "Garamond",
    size = 15
  )

table_6
gtsave(table_6, "Table6.docx", "Figures")


### ---- Supplementary table 1 ----

prep_rel <- function(df, half, lang) {
  df <- as.data.frame(df)                
  df$measure <- rownames(df)            
  
  # create reliability and min-max columns
  tibble(
    measure = df$measure,
    language = lang,
    !!paste0("reliability_", half) := round(df$mean_r, 2),
    !!paste0("minmax_", half) := paste0(round(df$min_r, 2), "-", round(df$max_r, 2))
  )
}

rel_L1_p1_tib <- prep_rel(rel_L1_p1, "first", "L1 (Dutch)")
rel_L2_p1_tib <- prep_rel(rel_L2_p1, "first", "L2 (English)")

# second half
rel_L1_p2_tib <- prep_rel(rel_L1_p2, "second", "L1 (Dutch)")
rel_L2_p2_tib <- prep_rel(rel_L2_p2, "second", "L2 (English)")

# combine first and second half per language
rel_L1 <- left_join(rel_L1_p1_tib, rel_L1_p2_tib, by = c("measure", "language"))
rel_L2 <- left_join(rel_L2_p1_tib, rel_L2_p2_tib, by = c("measure", "language"))

# combine L1 and L2
supp_table <- bind_rows(rel_L1, rel_L2)

supp_table <- supp_table %>%
  mutate(Measure = gsub("_trim$", "", measure))%>%
  mutate(Measure = recode(
    Measure,
    "FFD" = "First-fixation duration", 
    "SFD" = "Single-fixation duration", 
    "GD" = "Gaze duration", 
    "TRT" = "Total reading time", 
    "GPT" = "Go past time"
  )) %>%
  select(-measure)

supp_wide <- supp_table %>%
  pivot_wider(
    id_cols = Measure,
    names_from = language,
    values_from = c(reliability_first, minmax_first, reliability_second, minmax_second),
    names_sep = "_"
  )


gt_tablesupp <- supp_wide %>%
  gt() %>%
  tab_header(
    title = "Table S1",
    subtitle = "Spearman-Brown split-half reliability coefficients for the first and second half of the novel separately as participants read only half of the novel in one language."
  ) %>%
  tab_spanner(
    label = "L1 (Dutch)",
    columns = starts_with("reliability_first_L1") | starts_with("minmax_first_L1") |
      starts_with("reliability_second_L1") | starts_with("minmax_second_L1")
  ) %>%
  tab_spanner(
    label = "L2 (English)",
    columns = starts_with("reliability_first_L2") | starts_with("minmax_first_L2") |
      starts_with("reliability_second_L2") | starts_with("minmax_second_L2")
  ) %>%
  cols_label(
    # L1 columns
    "reliability_first_L1 (Dutch)"   = "Reliability first half",
    "minmax_first_L1 (Dutch)"      = "Min-Max",
    "reliability_second_L1 (Dutch)" = "Reliability second half",
    "minmax_second_L1 (Dutch)"     = "Min-Max",
    # L2 columns (same visible labels!)
    "reliability_first_L2 (English)"   = "Reliability first half",
    "minmax_first_L2 (English)"       = "Min-Max",
    "reliability_second_L2 (English)" = "Reliability second half",
    "minmax_second_L2 (English)"      = "Min-Max"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  tab_options(
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(size = px(15)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  opt_table_font(
    font = "Garamond",
    size = 15
  )


gt_tablesupp
gtsave(gt_tablesupp, "Supp_Table1.png", "Figures")
