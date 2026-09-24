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
  mutate(
    TRT = na_if(TRT, 0),
    SFD = if_else(WORD_FIXATION_COUNT == 1, FFD, NA_real_))%>% # create single fixation duration
  mutate(refix = as.integer(WORD_FIRST_RUN_FIXATION_COUNT > 1))%>%
  group_by(PART, TRIAL, LANGUAGE, PP_NR)%>%
  mutate(TRIAL_PROP = WORD_ID_WITHIN_TRIAL / max(WORD_ID_WITHIN_TRIAL, na.rm = TRUE))%>%
  ungroup%>%
  select(PART, TRIAL, WORD_ID_WITHIN_TRIAL, TRIAL_PROP, WORD_ID, LANGUAGE, LANGUAGE_RANK, PP_NR,
         WORD, FFD, SFD, GD, TRT, GPT, SKIP, refix)%>%
  filter(!PP_NR == 1)%>% # remove the first participant (too low comprehension score)
  filter(PP_NR < 16) # run the analyses only on the first 15 (14 after excluding number 1) participants

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
    reading time measures for L1 (Dutch) and L2 (English) reading including only the first 15 participants. 
    The result of the *t*-test comparing L1 and L2 reading in DECO is presented in the *t*-value column with degrees of freedom in brackets.
    Statistical significance is indicated by asterisks: p < .05 (\\*), p < .01 (\\*\\*), and p < .001 (\\*\\*\\*)."
    )) %>%
  # Nested L1/L2 spanners for DECO
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
  # Top-level spanners DECO and GECO (on top of L1/L2)
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
gtsave(gt_table_nested, "Table3.docx", "Figures/supplementary")

## ---- summary plots ----

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
  ## L1: left half
  geom_half_violin(
    data = subset(plot_raw, LANGUAGE == "L1"),
    position = position_nudge(x = -0.20),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  ## L2: right half
  geom_half_violin(
    data = subset(plot_raw, LANGUAGE == "L2"),
    position = position_nudge(x = 0.25),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  # Lines connecting L1 → L2 per participant
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
  # Points for participant means
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

ggsave('Figures/supplementary/RT_dist_raw_revised.tiff', width = 20, height = 13, unit = 'cm', dpi=300)


### ---- Distribution of reading times - log transform ----

ggplot(plot_log, aes(x = Measure, y = RT, fill = fill_color)) +
  theme_minimal(
    base_size = 30,
    base_family = "Roboto Mono"
  ) +
  ## L1: left half
  geom_half_violin(
    data = subset(plot_log, LANGUAGE == "L1"),
    position = position_nudge(x = -0.20),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  ## L2: right half
  geom_half_violin(
    data = subset(plot_log, LANGUAGE == "L2"),
    position = position_nudge(x = 0.25),
    side = 'r',
    width = 0.6,
    trim = FALSE,
    alpha = 0.9
  ) +
  # Lines connecting L1 → L2 per participant
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
  # Points for participant means
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


ggsave('Figures/supplementary/RT_dist_log_revised.tiff', width = 20, height = 13, unit = 'cm', dpi=300)


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


ggsave('Figures/supplementary/QQ_log2.tiff', width = 22, height = 15, unit = 'cm', dpi=300)

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
    min_r  = 2 * splitHalf(mat)$minrb / (1 + splitHalf(mat)$minrb)   # minimum split-half reliability + correction
  )
}

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
gtsave(table_6, "Table6.docx", "Figures/supplementary")


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

# Second half
rel_L1_p2_tib <- prep_rel(rel_L1_p2, "second", "L1 (Dutch)")
rel_L2_p2_tib <- prep_rel(rel_L2_p2, "second", "L2 (English)")

# Combine first and second half per language
rel_L1 <- left_join(rel_L1_p1_tib, rel_L1_p2_tib, by = c("measure", "language"))
rel_L2 <- left_join(rel_L2_p1_tib, rel_L2_p2_tib, by = c("measure", "language"))

# Combine L1 and L2
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
    title = "Table S5",
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
gtsave(gt_tablesupp, "Supp_Table1.png", "Figures/supplementary")

## ---- DECO GECO comparison ----

pp_avg <- trimdata %>%
  group_by(PP_NR, LANGUAGE)%>%
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
  ungroup%>%
  select(PP_NR, LANGUAGE, contains('mean'), contains('sd'), contains('min'), contains('max'))


skip_data <- data %>%
  mutate(new_SKIP = if_else(SKIP == 1 & is.na(TRT), 1, 0)) %>%
  group_by(PP_NR, LANGUAGE)%>%
  summarise(mean_SKIP = mean(new_SKIP, na.rm = TRUE), 
            sd_SKIP = sd(new_SKIP, na.rm = TRUE), 
            mean_refix = mean(refix, na.rm = TRUE), 
            sd_refix = sd(refix, na.rm = TRUE))%>%
  ungroup()

### ---- GECO ----

df_G <- read_rds("ET Data/GECO/GECO.rds")
class <- sapply(df_G, class)

cols <- colnames(df_G)
num_cols <- cols[12:57]
df_G <- df_G %>%
  mutate(across(all_of(num_cols), ~ as.numeric(replace(as.character(.x), .x == ".", NA))))

# prepare data for analysis
data_G <- df_G%>%
  rename(FFD = WORD_FIRST_FIXATION_DURATION, 
         GD = WORD_GAZE_DURATION, 
         TRT = WORD_TOTAL_READING_TIME, 
         GPT = WORD_GO_PAST_TIME,
         SKIP = WORD_SKIP)%>%
  mutate(SFD = if_else(WORD_FIXATION_COUNT == 1, FFD, NA_real_))%>% # create single fixation duration
  mutate(refix = as.integer(WORD_FIRST_RUN_FIXATION_COUNT > 1))%>%
  select(PART, TRIAL, WORD_ID_WITHIN_TRIAL, WORD_ID, LANGUAGE, LANGUAGE_RANK, PP_NR,
         WORD, FFD, SFD, GD, TRT, GPT, SKIP, refix)

# trimming and outlier removal ~ following GECO 
trimdata_G <- data_G%>%
  mutate(LANGUAGE = factor(LANGUAGE, levels = c("Dutch","English"))) %>%
  mutate(LANGUAGE_RANK = factor(LANGUAGE_RANK, levels = c("L1","L2"))) %>%
  filter(FFD >= 100) %>%
  filter(SKIP == 0)%>%
  group_by(PP_NR, LANGUAGE)%>%
  mutate(
    across(
      .cols = c(FFD, SFD, GD, TRT, GPT),
      .fns = ~ if_else(.x <= mean(.x, na.rm = TRUE) + 2.5 * sd(.x, na.rm = TRUE),
                       .x, NA_real_))) %>%
  ungroup()

pp_avg_G <- trimdata_G %>%
  group_by(PP_NR, LANGUAGE)%>%
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
  ungroup%>%
  select(PP_NR, LANGUAGE, contains('mean'), contains('sd'), contains('min'), contains('max'))


skip_data_G <- data_G %>%
  mutate(new_SKIP = if_else(SKIP == 1 & (is.na(TRT) | TRT == 0), 1, 0)) %>% #pp16 has TRT of zero instead of . 
  group_by(PP_NR, LANGUAGE)%>%
  summarise(mean_SKIP = mean(new_SKIP, na.rm = TRUE), 
            sd_SKIP = sd(new_SKIP), 
            mean_refix = mean(refix, na.rm = TRUE),
            sd_refix = sd(refix, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!(PP_NR == 'pp18'))


### ---- check normality ----
measures <- c("FFD_mean", "SFD_mean", "GD_mean", "TRT_mean", "GPT_mean")

check_normality <- function(measure, lang, df1, df2) {
  x1 <- df1[[measure]][df1$LANGUAGE == lang]
  x2 <- df2[[measure]][df2$LANGUAGE == lang]
  
  sw1 <- shapiro.test(x1)
  sw2 <- shapiro.test(x2)
  
  tibble(
    ET       = sub("_mean", "", measure),
    Language = lang,
    N_corpus1  = length(x1),
    N_corpus2  = length(x2),
    Shapiro_W_corpus1 = round(unname(sw1$statistic), 3),
    Shapiro_p_corpus1 = round(sw1$p.value, 4),
    Shapiro_W_corpus2 = round(unname(sw2$statistic), 3),
    Shapiro_p_corpus2 = round(sw2$p.value, 4),
    Normal_corpus1 = sw1$p.value > .05,
    Normal_corpus2 = sw2$p.value > .05
  )
}

normality_check <- bind_rows(
  lapply(measures, check_normality, lang = "Dutch",   df1 = pp_avg, df2 = pp_avg_G),
  lapply(measures, check_normality, lang = "English", df1 = pp_avg, df2 = pp_avg_G)
)


measures <- c("FFD_mean", "SFD_mean", "GD_mean", "TRT_mean", "GPT_mean")
languages <- c("Dutch", "English")
lang_labels <- c("L1", "L2")

calc_diff <- function(measure, lang, df1, df2, alpha = .05) {
  x1 <- df1[[measure]][df1$LANGUAGE == lang]
  x2 <- df2[[measure]][df2$LANGUAGE == lang]
  
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  # check normality per group
  sw1 <- shapiro.test(x1)
  sw2 <- shapiro.test(x2)
  use_parametric <- (sw1$p.value > alpha) & (sw2$p.value > alpha)
  
  if (use_parametric) {
    # --- Welch's t-test ---
    M_diff  <- mean(x1) - mean(x2)
    SE_diff <- sqrt((sd(x1)^2 / n1) + (sd(x2)^2 / n2))
    
    res <- t.test(x1, x2, paired = FALSE)
    
    CI_str <- paste0("[", round(res$conf.int[1], 2), ", ", round(res$conf.int[2], 2), "]")
    
    stars <- case_when(
      res$p.value < 0.001 ~ "***",
      res$p.value < 0.01  ~ "**",
      res$p.value < 0.05  ~ "*",
      TRUE                ~ ""
    )
    
    stat_str <- paste0(round(res$statistic, 2), " [", round(res$parameter, 1), "]", stars)
    test_used <- "Welch's t"
    
  } else {
    # --- Mann-Whitney U (Wilcoxon rank-sum) ---
    res <- wilcox.test(x1, x2, paired = FALSE, conf.int = TRUE, exact = FALSE)
    
    # Hodges-Lehmann estimate
    M_diff  <- round(res$estimate, 2)
    SE_diff <- NA 
    
    CI_str <- paste0("[", round(res$conf.int[1], 2), ", ", round(res$conf.int[2], 2), "]")
    
    stars <- case_when(
      res$p.value < 0.001 ~ "***",
      res$p.value < 0.01  ~ "**",
      res$p.value < 0.05  ~ "*",
      TRUE                ~ ""
    )
    
    stat_str <- paste0("W = ", round(res$statistic, 1), stars)
    test_used <- "Mann-Whitney U"
  }
  
  tibble(
    ET      = sub("_mean", "", measure),
    Test    = test_used,
    M_diff  = round(M_diff, 2),
    SE_diff = if (is.na(SE_diff)) NA_real_ else round(SE_diff, 2),
    CI_diff = CI_str,
    t_diff  = stat_str
  )
}

result_L1 <- bind_rows(lapply(measures, calc_diff, lang = "Dutch", df1 = pp_avg, df2 = pp_avg_G)) %>%
  rename(Test_L1 = Test, M_diff_L1 = M_diff, SE_diff_L1 = SE_diff, CI_L1 = CI_diff, t_L1 = t_diff)

result_L2 <- bind_rows(lapply(measures, calc_diff, lang = "English", df1 = pp_avg, df2 = pp_avg_G)) %>%
  rename(Test_L2 = Test, M_diff_L2 = M_diff, SE_diff_L2 = SE_diff, CI_L2 = CI_diff, t_L2 = t_diff)

final_result <- left_join(result_L1, result_L2, by = "ET") %>%
  mutate(
    ET = recode(
      ET,
      FFD = "First-fixation duration",
      SFD = "Single-fixation duration",
      GD  = "Gaze duration",
      TRT = "Total reading time",
      GPT = "Go-past time"))

print(final_result)

### ---- Table 4: comparison DECO - GECO  ----

#### new 
gt_table_nested <- final_result %>%
  select(-SE_diff_L1, -SE_diff_L2) %>%
  gt(rowname_col = "ET") %>%
  tab_header(
    title = "Table 4",
    subtitle = md("Comparison of the reading times in the current DECO corpus (Part 1) and the previous GECO corpus 
                   with 95% confidence intervals (95% CI) and test statistics for both L1 (Dutch) and L2 (English) reading. 
                   Positive values mean the reading times were longer in the DECO corpus. Comparisons for which the 
                   normality assumption held used Welch's *t*-test (mean difference, *t* [df] reported); comparisons for 
                   which normality was violated used the Mann-Whitney *U* test (W* reported). 
                   Statistical significance is indicated by asterisks: p < .05 (\\*), p < .01 (\\*\\*), and p < .001 (\\*\\*\\*)."))%>%
  tab_options(
    heading.align = "left"
  )%>%
  # Top-level spanners
  tab_spanner(label = "L1 (Dutch)",
              columns = c("M_diff_L1", "CI_L1", "t_L1"),
              id = "L1_top") %>%
  tab_spanner(label = "L2 (English)",
              columns = c("M_diff_L2", "CI_L2", "t_L2"),
              id = "L2_top") %>%
  # Column labels
  cols_label(
    M_diff_L1 = md("Δ"),
    CI_L1 = "95% CI",
    t_L1 = md("Statistic [df]"),
    M_diff_L2 = md("Δ"),
    CI_L2 = "95% CI",
    t_L2 = md("Statistic [df]")) %>%
  # Center numeric columns
  cols_align(
    align = "center",
    columns = !ET) %>%
  opt_table_font(
    font = "Garamond",
    size = 15)

gt_table_nested
gtsave(gt_table_nested, "Table4_revision.docx", "Figures/supplementary")


### ---- Table 5: comparison skipping   ----

summarise_var <- function(df, label, var, measure_label) {
  
  df <- df %>% mutate(VAR = .data[[var]])
  
  stats <- df %>%
    group_by(LANGUAGE) %>%
    summarise(
      mean = mean(VAR, na.rm = TRUE),
      sd   = sd(VAR, na.rm = TRUE),
      min  = min(VAR, na.rm = TRUE),
      max  = max(VAR, na.rm = TRUE),
      .groups = "drop"
    )
  
  t_res <- t.test(
    df$VAR[df$LANGUAGE == "Dutch"],
    df$VAR[df$LANGUAGE == "English"],
    paired = TRUE
  )
  
  p_val <- t_res$p.value
  
  stars <- case_when(
    p_val < .001 ~ "***",
    p_val < .01  ~ "**",
    p_val < .05  ~ "*",
    TRUE         ~ ""
  )
  
  t_str <- paste0(
    round(t_res$statistic, 2),
    "[", round(t_res$parameter, 1), "]",
    stars
  )
  
  tibble(
    Corpus = label,
    L1_mean = stats$mean[stats$LANGUAGE == "Dutch"],
    L1_sd   = stats$sd[stats$LANGUAGE == "Dutch"],
    L1_min  = stats$min[stats$LANGUAGE == "Dutch"],
    L1_max  = stats$max[stats$LANGUAGE == "Dutch"],
    L2_mean = stats$mean[stats$LANGUAGE == "English"],
    L2_sd   = stats$sd[stats$LANGUAGE == "English"],
    L2_min  = stats$min[stats$LANGUAGE == "English"],
    L2_max  = stats$max[stats$LANGUAGE == "English"],
    `t [df]` = t_str,
    Measure = measure_label
  )
}

skip_summary_df <- bind_rows(
  summarise_var(skip_data,   "DECO", "mean_SKIP",  "Average skipping probability"),
  summarise_var(skip_data_G, "GECO", "mean_SKIP",  "Average skipping probability"),
  summarise_var(skip_data,   "DECO", "mean_refix", "Average refixation probability"),
  summarise_var(skip_data_G, "GECO", "mean_refix", "Average refixation probability")
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


# make table
gt_table_5 <- skip_summary_df %>%
  gt(
    rowname_col   = "Measure",
    groupname_col = "Corpus"
  ) %>%
  tab_header(
    title = "Table 5",
    subtitle = "Average skipping probability for L1 (Dutch) and L2 (English) reading for the current DECO corpus (Part 1) and the previous GECO corpus.
    Statistical significance is indicated by asterisks: p < .05 (*), p < .01 (**), and p < .001 (***)."
  ) %>%
  
  # L1 / L2 spanners
  tab_spanner(
    label = "L1 (Dutch)",
    columns = c(L1_mean, L1_sd, L1_min, L1_max)
  ) %>%
  tab_spanner(
    label = "L2 (English)",
    columns = c(L2_mean, L2_sd, L2_min, L2_max)
  ) %>%
  
  # t-test column
  tab_spanner(
    label   = md("*t* Value (L1–L2)"),
    columns = `t [df]`
  ) %>%
  
  # Column labels
  cols_label(
    L1_mean = "Mean",
    L1_sd   = "SD",
    L1_min  = "Min",
    L1_max  = "Max",
    
    L2_mean = "Mean",
    L2_sd   = "SD",
    L2_min  = "Min",
    L2_max  = "Max",
    
    `t [df]` = md("*t* [df]")
  ) %>%
  
  # Styling
  cols_align(
    align = "center",
    columns = !Measure
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  tab_style(
    style = cell_text(size = px(15)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  tab_options(
    heading.align = "left"
  ) %>%
  
  opt_table_font(
    font = "Garamond",
    size = 15
  )

gt_table_5

gtsave(gt_table_5, "Table5.docx", "Figures/supplementary")


# comparison between corpora 
t_L1 <- t.test(skip_data$mean_SKIP[skip_data$LANGUAGE == "Dutch"],
               skip_data_G$mean_SKIP[skip_data_G$LANGUAGE == "Dutch"],
               paired = FALSE)

t_L2 <- t.test(skip_data$mean_SKIP[skip_data$LANGUAGE == "English"],
               skip_data_G$mean_SKIP[skip_data_G$LANGUAGE == "English"],
               paired = FALSE)
