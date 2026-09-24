library("tidyverse")
library("tidytext")
library('readxl')
library("showtext")
library('gt')

font_add_google('Roboto Mono')
showtext_auto()

# 4. ANALYSIS ------------------------------------------------

## ---- Data DECO ----

data <- read_xlsx("PP Data/PPInfo - DECO.xlsx")


# Drop participant 1 (low comprehension scores)
data <- data%>%
  filter(!PP_NR == 1)

# Summary statistics
measures <- list(
  "LexTALE score (%)"            = c("LEXTALE_DUTCH", "LEXTALE_ENG"),
  "Spelling score (%)"           = c("SPELLING_DUTCH", "SPELLING_ENG"),
  "Lexical decision accuracy (%)"= c("LD_Dutch_Acc", "LD_English_Acc"),
  "Subjective exposure (%)"      = c("%EXP_DUTCH", "%EXP_ENG")
)

create_summary_table <- function(data, measures) {
  map_dfr(names(measures), function(m) {
    
    var_L1 <- measures[[m]][1]
    var_L2 <- measures[[m]][2]
    
    # Means and SDs
    mean_L1 <- mean(data[[var_L1]], na.rm = TRUE)
    sd_L1   <- sd(data[[var_L1]], na.rm = TRUE)
    max_L1 <- max(data[[var_L1]], na.rm = TRUE)
    min_L1 <- min(data[[var_L1]], na.rm = TRUE)
    
    mean_L2 <- mean(data[[var_L2]], na.rm = TRUE)
    sd_L2   <- sd(data[[var_L2]], na.rm = TRUE)
    max_L2 <- max(data[[var_L2]], na.rm = TRUE)
    min_L2 <- min(data[[var_L2]], na.rm = TRUE)
    
    # Paired t-test
    t_res <- t.test(data[[var_L1]], data[[var_L2]], paired = TRUE)
    t_val <- as.numeric(t_res$statistic)
    dfree <- t_res$parameter
    p_val <- t_res$p.value
    
    # Significance stars
    sig <- case_when(
      p_val < .001 ~ "***",
      p_val < .01  ~ "**",
      p_val < .05  ~ "*",
      TRUE         ~ "  "
    )
    
    tibble(
      Measure = m,
      L1_Mean_SD = sprintf("%.2f [%.2f]", mean_L1, sd_L1),
      L1_minmax = sprintf("[%.2f – %.2f]", min_L1, max_L1),
      L2_Mean_SD = sprintf("%.2f [%.2f]", mean_L2, sd_L2),
      L2_minmax = sprintf("[%.2f – %.2f]", min_L2, max_L2),
      t_Value_L1_L2 = sprintf("%.2f [%d] %s", t_val, dfree, sig)
    )
  })
}

summary_table <- create_summary_table(data, measures)

## ---- Data GECO ----

# GECO data 
data_GECO <- read_xlsx("PP Data/PPInfo_GECO.xlsx")

cols_to_numeric <- colnames(data_GECO)

df_GECO <- data_GECO %>%
  filter(GROUP == 'bilingual')%>%
  mutate(across(all_of(cols_to_numeric), ~ as.numeric(as.character(.x))))%>% # convert all columns to numeric
  rename(
    LD_Dutch_Acc   = LEX_DEC_ACC_DUTCH,
    LD_English_Acc = LEX_DEC_ACC_ENG
  )


summary_table_GECO <- create_summary_table(df_GECO, measures)

summary_total <- rbind(summary_table, summary_table_GECO)

summary_total <- summary_total %>%
  mutate(
    Corpus = ifelse(row_number() <= 4, "DECO", "GECO"))


## ---- Comparison DECO - GECO ----

sig_stars <- function(p) {
  if (p < .001) "***"
  else if (p < .01) "** "
  else if (p < .05) "*  "
  else "  "
}

results_df <- do.call(
  rbind,
  lapply(names(measures), function(measure_name) {
    
    cols <- measures[[measure_name]]
    col_L1 <- cols[1]  # Dutch
    col_L2 <- cols[2]  # English
    
    ## L1 (Dutch)
    t_L1 <- t.test(
      data[[col_L1]],
      df_GECO[[col_L1]],
      paired = FALSE
    )
    
    diff_L1 <- mean(data[[col_L1]], na.rm = TRUE) - mean(df_GECO[[col_L1]], na.rm = TRUE)  # mean(DECO) − mean(GECO)
    se_L1   <- unname(t_L1$stderr)
    ci_L1   <- unname(t_L1$conf.int)
    
    ## L2 (English)
    t_L2 <- t.test(
      data[[col_L2]],
      df_GECO[[col_L2]],
      paired = FALSE
    )
    
    diff_L2 <- mean(data[[col_L2]], na.rm = TRUE) - mean(df_GECO[[col_L2]], na.rm = TRUE)
    se_L2   <- unname(t_L2$stderr)
    ci_L2   <- unname(t_L2$conf.int)
    
    data.frame(
      Measure = measure_name,
      
      L1_Mean_SE = sprintf("%.2f [%.2f]", diff_L1, se_L1),
      L1_CI      = sprintf("[%.2f, %.2f]", ci_L1[1], ci_L1[2]),
      
      L2_Mean_SE = sprintf("%.2f [%.2f]", diff_L2, se_L2),
      L2_CI      = sprintf("[%.2f, %.2f]", ci_L2[1], ci_L2[2]),
      
      t_Value_L1 = sprintf(
        "%.2f [%d] %s",
        unname(t_L1$statistic),
        round(unname(t_L1$parameter)),
        sig_stars(t_L1$p.value)
      ),
      t_Value_L2 = sprintf(
        "%.2f [%d] %s",
        unname(t_L2$statistic),
        round(unname(t_L2$parameter)),
        sig_stars(t_L2$p.value)
      ),
      
      stringsAsFactors = FALSE
    )
  })
)

results_df <- results_df%>%
  select(Measure, L1_Mean_SE, L1_CI, t_Value_L1, L2_Mean_SE, L2_CI, t_Value_L2)

## ---- Table 1 & 2  ----

# Table 1
gt_table <- summary_table %>%
  gt(
    rowname_col = "Measure",
    groupname_col = "Section"
  ) %>%
  cols_label(
    L1_Mean_SD = "L1 (Dutch)",
    L1_minmax = 'Range L1',
    L2_Mean_SD = "L2 (English)",
    L2_minmax = 'Range L2',
    t_Value_L1_L2    = md("*t* Value")
  ) %>%
  fmt_markdown(columns = t_Value_L1_L2)%>%
  cols_width(
    t_Value_L1_L2    ~ px(250)   # reduce from default wide width
  )%>%
  tab_header(
    title = "Table 1",
    subtitle = md(paste(
      "Average percentage scores [standard deviations] on LexTALE, spelling,",
      "lexical decision accuracy and subjective exposure to the second language",
      "Statistical significance is indicated by asterisks: p < .05 (\\*), p < .01 (\\*\\*), and p < .001 (\\*\\*\\*)."
    )
    )) %>%
  tab_options(
    heading.align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_row_groups()
  )%>%
  tab_stub_indent(
    rows = everything(),
    indent = px(10)
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  opt_table_font(
    font = "Garamond",
    size = 15
  )
gt_table
gtsave(gt_table, "Table1.docx", "Figures")

# Table 2
gt_table <- results_df %>%
  gt(
    rowname_col = "Measure",
    groupname_col = "Section"
  ) %>%
  cols_move(
    columns = c(L1_Mean_SE, L1_CI, t_Value_L1, L2_Mean_SE, L2_CI, t_Value_L2),
    after = stub()
  ) %>%
  cols_label(
    L1_Mean_SE = "L1 (Dutch)",
    L1_CI = "95% CI L1",
    t_Value_L1 = md("*t* Value L1"),
    L2_Mean_SE = "L2 (English)",
    L2_CI = "95% CI L2",
    t_Value_L2 = md("*t* Value L2")
  ) %>%
  tab_header(
    title = "Table 2",
    subtitle = md(paste(
      "Comparison of the performance on the language tests in the DECO and GECO sample. Negative values indicate lower average performance in the DECO sample.",
      "Statistical significance is indicated by asterisks: p < .05 (\\*), p < .01 (\\*\\*), and p < .001 (\\*\\*\\*)."
    )
    )) %>%
  tab_options(
    heading.align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_row_groups()
  )%>%
  tab_stub_indent(
    rows = everything(),
    indent = px(10)
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  opt_table_font(
    font = "Garamond",
    size = 15
  )
gt_table
gtsave(gt_table, "Table2.docx", "Figures")

## ---- T-test comprehension scores ----

data[["COMPR_DUTCH"]]
data[["COMPR_ENG"]]

df_GECO[["COMPR_DUTCH"]]
df_GECO[["COMPR_ENG"]]

test1 <- t.test(data[["COMPR_DUTCH"]], df_GECO[["COMPR_DUTCH"]], paired = FALSE)
test2 <- t.test(data[["COMPR_ENG"]], df_GECO[["COMPR_ENG"]], paired = FALSE)