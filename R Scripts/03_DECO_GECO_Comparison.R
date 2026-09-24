library("tidyverse")
library("lme4")
library("lmtest")
library("poolr")
library("gt")

# 1. Data ------------------------------------------------------

## DECO --------------------------------------------------------

df <- read_rds("ET Data/DECO.rds")
class <- sapply(df, class)

# convert columns to numeric
cols <- colnames(df)
num_cols <- cols[12:58]
df <- df %>%
  mutate(across(all_of(num_cols), ~ as.numeric(replace(as.character(.x), .x == ".", NA))))%>%
  mutate(WORD_TOTAL_READING_TIME = if_else(WORD_TOTAL_READING_TIME == 0, NA_real_, WORD_TOTAL_READING_TIME))
  
# prepare data for analysis
data <- df%>%
  rename(FFD = WORD_FIRST_FIXATION_DURATION, 
         GD = WORD_GAZE_DURATION, 
         TRT = WORD_TOTAL_READING_TIME, 
         GPT = WORD_GO_PAST_TIME,
         SKIP = WORD_SKIP)%>%
  mutate(SFD = if_else(WORD_FIXATION_COUNT == 1, FFD, NA_real_))%>% # create single fixation duration
  mutate(refix = as.integer(WORD_FIRST_RUN_FIXATION_COUNT > 1))%>%
  select(PART, TRIAL, WORD_ID_WITHIN_TRIAL, WORD_ID, LANGUAGE, LANGUAGE_RANK, PP_NR,
         WORD, FFD, SFD, GD, TRT, GPT, SKIP, refix)

# trimming
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

# drop pp 1 (low score on comprehension questions) 
trimdata  <- trimdata %>%
  filter(!PP_NR == 1)

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
            sd_refix = mean(refix, na.rm = TRUE))%>%
  ungroup()


## GECO --------------------------------------------------------

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

# trimming 
trimdata_G <- data_G%>%
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
  mutate(new_SKIP = if_else(SKIP == 1 & (is.na(TRT) | TRT == 0), 1, 0)) %>%
  group_by(PP_NR, LANGUAGE) %>%
  summarise(mean_SKIP  = mean(new_SKIP, na.rm = TRUE), 
            sd_SKIP    = sd(new_SKIP, na.rm = TRUE),
            mean_refix = mean(refix, na.rm = TRUE), 
            sd_refix   = sd(refix, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!(PP_NR == 'pp18'))


# 2. Statistics  ------------------------------------------------------

# check normality 
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

# measures
measures <- c("FFD_mean", "SFD_mean", "GD_mean", "TRT_mean", "GPT_mean")

# languages and labels
languages <- c("Dutch", "English")
lang_labels <- c("L1", "L2")

# function to calculate mean difference, SE, and t-test
calc_diff <- function(measure, lang, df1, df2) {
  x1 <- df1[[measure]][df1$LANGUAGE == lang]
  x2 <- df2[[measure]][df2$LANGUAGE == lang]
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  M_diff <- mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)
  SE_diff <- sqrt((sd(x1, na.rm = TRUE)^2 / n1) + (sd(x2, na.rm = TRUE)^2 / n2))
  
  t_res <- t.test(x1, x2, paired = FALSE)
  
  # confidence interval
  CI_str <- paste0("[", round(t_res$conf.int[1], 2), ", ", round(t_res$conf.int[2], 2), "]")
  
  # significance stars
  stars <- case_when(
    t_res$p.value < 0.001 ~ "***",
    t_res$p.value < 0.01  ~ "**",
    t_res$p.value < 0.05  ~ "*",
    TRUE                   ~ ""
  )
  
  # t-value with df and stars
  t_str <- paste0(round(t_res$statistic, 2), " [", round(t_res$parameter, 1), "]", stars)
  
  tibble(
    ET = sub("_mean", "", measure),
    M_diff = round(M_diff, 2),
    SE_diff = round(SE_diff, 2),
    CI_diff = CI_str,
    t_diff = t_str
  )
}

test <- calc_diff('FFD_mean', lang = 'Dutch', pp_avg, pp_avg_G)

result_L1 <- bind_rows(lapply(measures, calc_diff, lang = "Dutch", df1 = pp_avg, df2 = pp_avg_G)) %>%
  rename(M_diff_L1 = M_diff, SE_diff_L1 = SE_diff, CI_L1 = CI_diff, t_L1 = t_diff)

result_L2 <- bind_rows(lapply(measures, calc_diff, lang = "English", df1 = pp_avg, df2 = pp_avg_G)) %>%
  rename(M_diff_L2 = M_diff, SE_diff_L2 = SE_diff, CI_L2 = CI_diff, t_L2 = t_diff)

final_result <- left_join(result_L1, result_L2, by = "ET")%>%
  mutate(
    ET = recode(
      ET,
      FFD = "First-fixation duration",
      SFD = "Single-fixation duration",
      GD  = "Gaze duration",
      TRT = "Total reading time",
      GPT = "Go-past time"))


#### new
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
    SE_diff <- NA  # not defined in the same way for a rank-based test
    
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

print(final_result, n = Inf)

# 3. Table   ------------------------------------------------------

## Table 4: comparison DECO - GECO --------------------------------

gt_table_nested <- final_result %>%
  select(-SE_diff_L1, -SE_diff_L2) %>%
  gt(rowname_col = "ET") %>%
  tab_header(
    title = "Table 4",
    subtitle = md("Comparison of the reading times in the current DECO corpus and the previous GECO corpus 
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
gtsave(gt_table_nested, "Table4_revision.docx", "Figures")


## Table 5: comparison skipping ----------------------------------------

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
    subtitle = "Average skipping and refixation probability for L1 (Dutch) and L2 (English) reading for the current DECO corpus and the previous GECO corpus.
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

gtsave(gt_table_5, "Table5.docx", "Figures")

# comparison between corpora 
t_L1 <- t.test(skip_data$mean_SKIP[skip_data$LANGUAGE == "Dutch"],
               skip_data_G$mean_SKIP[skip_data_G$LANGUAGE == "Dutch"],
               paired = FALSE)

t_L2 <- t.test(skip_data$mean_SKIP[skip_data$LANGUAGE == "English"],
               skip_data_G$mean_SKIP[skip_data_G$LANGUAGE == "English"],
               paired = FALSE)

## L1-L2 difference -----------------------------------------

pp_diff <- pp_avg %>%
  select(PP_NR, LANGUAGE, ends_with("_mean")) %>%
  pivot_wider(
    names_from  = LANGUAGE,
    values_from = ends_with("_mean")
  ) %>%
  transmute(
    PP_NR,
    FFD_diff = FFD_mean_Dutch - FFD_mean_English,
    SFD_diff = SFD_mean_Dutch - SFD_mean_English,
    GD_diff  = GD_mean_Dutch  - GD_mean_English,
    TRT_diff = TRT_mean_Dutch - TRT_mean_English,
    GPT_diff = GPT_mean_Dutch - GPT_mean_English
  )


pp_diff_G <- pp_avg_G %>%
  select(PP_NR, LANGUAGE, ends_with("_mean")) %>%
  pivot_wider(
    names_from  = LANGUAGE,
    values_from = ends_with("_mean")
  ) %>%
  transmute(
    PP_NR,
    FFD_diff = FFD_mean_Dutch - FFD_mean_English,
    SFD_diff = SFD_mean_Dutch - SFD_mean_English,
    GD_diff  = GD_mean_Dutch  - GD_mean_English,
    TRT_diff = TRT_mean_Dutch - TRT_mean_English,
    GPT_diff = GPT_mean_Dutch - GPT_mean_English
  )

# comparison 
t_FFD <- t.test(abs(pp_diff$FFD_diff), abs(pp_diff_G$FFD_diff), paired = FALSE, alternative = "greater")
t_SFD <- t.test(abs(pp_diff$SFD_diff), abs(pp_diff_G$SFD_diff), paired = FALSE, alternative = "greater")
t_GD  <- t.test(abs(pp_diff$GD_diff),  abs(pp_diff_G$GD_diff),  paired = FALSE, alternative = "greater")
t_TRT <- t.test(abs(pp_diff$TRT_diff), abs(pp_diff_G$TRT_diff), paired = FALSE, alternative = "greater")
t_GPT <- t.test(abs(pp_diff$GPT_diff), abs(pp_diff_G$GPT_diff), paired = FALSE, alternative = "greater")

t_FFD
t_SFD
t_GD
t_TRT
t_GPT
