## Attrition table for the draft.
## This script is intentionally standalone: it reads the replication-package
## inputs and writes only draft-side outputs.

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(stringr)
  library(tidyr)
  library(xml2)
})

rp_dir <- "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Replication Package_Upload"
ra_dir <- "/Users/minseonp/Dropbox/RP/RA_Byunghun"
out_tex <- "tables_2025/table_attrition_balance.tex"
out_diag <- "tables_2025/table_attrition_balance_diagnostics.csv"

id_as_char <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, "\\.0$")
  ifelse(is.na(x) | x == "", NA_character_, x)
}

prefix_map <- c(
  "B" = "11", "P" = "12", "C" = "13", "F" = "14", "M" = "15", "R" = "16",
  "L" = "21", "Q" = "22", "J" = "23", "A" = "24", "K" = "25", "E" = "26"
)

convert_participant_label <- function(x) {
  x <- id_as_char(x)
  prefix <- toupper(str_sub(x, 1, 1))
  suffix <- str_sub(x, 2, -1)
  school <- unname(prefix_map[prefix])
  out <- ifelse(str_detect(x, "^[0-9]+$"), x, paste0(school, suffix))
  ifelse(is.na(school) & !str_detect(x, "^[0-9]+$"), NA_character_, out)
}

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

zero_to_na <- function(x) {
  x <- num(x)
  ifelse(x == 0, NA_real_, x)
}

fmt <- function(x) {
  x <- ifelse(abs(x) < 0.0005, 0, x)
  sprintf("%.3f", x)
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

rev4 <- function(x) {
  x <- zero_to_na(x)
  ifelse(is.na(x), NA_real_, 5 - x)
}

rev5 <- function(x) {
  x <- zero_to_na(x)
  ifelse(is.na(x), NA_real_, 6 - x)
}

row_mean_strict <- function(...) rowMeans(cbind(...), na.rm = FALSE)

correct_or_zero <- function(x, answer) ifelse(is.na(x), 0, as.numeric(x == answer))

score_math5 <- function(q1, q2, q3, q4, q5, answers) {
  correct_or_zero(q1, answers[1]) +
    correct_or_zero(q2, answers[2]) +
    correct_or_zero(q3, answers[3]) +
    correct_or_zero(q4, answers[4]) +
    correct_or_zero(q5, answers[5])
}

dedupe_id <- function(df, prefer_col = NULL) {
  if (!is.null(prefer_col) && prefer_col %in% names(df)) {
    df <- df %>% arrange(id, desc(.data[[prefer_col]]))
  } else {
    df <- df %>% arrange(id)
  }
  df %>%
    filter(!is.na(id), id != "") %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
}

xlsx_col_num <- function(ref) {
  letters <- str_extract(ref, "^[A-Z]+")
  vals <- utf8ToInt(letters) - utf8ToInt("A") + 1L
  Reduce(function(a, b) a * 26L + b, vals)
}

read_xlsx_first_sheet <- function(path) {
  ns <- c(d1 = "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
  
  shared <- character(0)
  if ("xl/sharedStrings.xml" %in% unzip(path, list = TRUE)$Name) {
    shared_xml <- read_xml(unz(path, "xl/sharedStrings.xml"))
    shared <- xml_find_all(shared_xml, ".//d1:si", ns) %>%
      lapply(function(si) paste(xml_text(xml_find_all(si, ".//d1:t", ns)), collapse = "")) %>%
      unlist(use.names = FALSE)
  }
  
  sheet_xml <- read_xml(unz(path, "xl/worksheets/sheet1.xml"))
  rows <- xml_find_all(sheet_xml, ".//d1:sheetData/d1:row", ns)
  
  parsed <- lapply(rows, function(row) {
    cells <- xml_find_all(row, "./d1:c", ns)
    if (length(cells) == 0) return(character(0))
    
    out <- rep(NA_character_, max(vapply(xml_attr(cells, "r"), xlsx_col_num, integer(1))))
    for (cell in cells) {
      ref <- xml_attr(cell, "r")
      typ <- xml_attr(cell, "t")
      v <- xml_text(xml_find_first(cell, "./d1:v", ns))
      if (identical(typ, "s") && !is.na(v) && v != "") {
        v <- shared[as.integer(v) + 1L]
      }
      out[xlsx_col_num(ref)] <- v
    }
    out
  })
  
  header <- make.names(parsed[[1]], unique = TRUE)
  body <- parsed[-1]
  max_cols <- length(header)
  body <- lapply(body, function(x) {
    length(x) <- max_cols
    x
  })
  
  as_tibble(do.call(rbind, body), .name_repair = "minimal") %>%
    setNames(header)
}

make_network_degree <- function(network_raw) {
  network_all <- network_raw %>%
    transmute(
      t = as.integer(t),
      id = id_as_char(id),
      obj_id = id_as_char(obj_id),
      best = num(best)
    ) %>%
    filter(t == 0, !is.na(id), id != "") %>%
    mutate(
      id_class = substr(id, 1, 5),
      obj_class = substr(obj_id, 1, 5),
      inclass = as.integer(!is.na(obj_id) & obj_id != "" & id_class == obj_class)
    )
  
  network_edges <- network_all %>%
    filter(!is.na(obj_id), obj_id != "", id != obj_id) %>%
    distinct(t, id, obj_id, .keep_all = TRUE)
  
  id_list <- bind_rows(
    network_all %>% select(id),
    network_edges %>% select(id = obj_id)
  ) %>%
    distinct(id) %>%
    filter(!is.na(id), id != "")
  
  id_list %>%
    left_join(
      network_edges %>%
        filter(inclass == 1) %>%
        group_by(id) %>%
        summarise(inclass_n_friends = n_distinct(obj_id), .groups = "drop"),
      by = "id"
    ) %>%
    left_join(
      network_edges %>%
        filter(inclass == 1) %>%
        group_by(obj_id) %>%
        summarise(inclass_popularity = n_distinct(id), .groups = "drop") %>%
        rename(id = obj_id),
      by = "id"
    ) %>%
    mutate(
      inclass_n_friends = ifelse(is.na(inclass_n_friends), 0L, inclass_n_friends),
      inclass_popularity = ifelse(is.na(inclass_popularity), 0L, inclass_popularity)
    )
}

block_diag <- function(mats) {
  nr <- sum(vapply(mats, nrow, integer(1)))
  nc <- sum(vapply(mats, ncol, integer(1)))
  out <- matrix(0, nr, nc)
  r <- 0L
  c <- 0L
  
  for (mat in mats) {
    out[(r + 1L):(r + nrow(mat)), (c + 1L):(c + ncol(mat))] <- mat
    r <- r + nrow(mat)
    c <- c + ncol(mat)
  }
  
  out
}

joint_suest_wald <- function(data, specs) {
  score_blocks <- list()
  bread_blocks <- list()
  coefs <- numeric(0)
  tested_idx <- integer(0)
  offset <- 0L
  
  for (i in seq_len(nrow(specs))) {
    f <- as.formula(paste(specs$y[i], "~", specs$x[i], "+ class_fe"))
    mf <- model.frame(f, data = data, na.action = na.omit)
    rows <- as.integer(rownames(mf))
    X0 <- model.matrix(f, mf)
    y <- model.response(mf)
    
    q <- qr(X0)
    keep <- q$pivot[seq_len(q$rank)]
    X <- X0[, keep, drop = FALSE]
    fit <- lm.fit(X, y)
    
    scores <- matrix(0, nrow(data), ncol(X))
    scores[rows, ] <- X * as.numeric(fit$residuals)
    
    score_blocks[[i]] <- scores
    bread_blocks[[i]] <- solve(crossprod(X))
    coefs <- c(coefs, fit$coefficients)
    tested_idx <- c(tested_idx, offset + match(specs$x[i], colnames(X)))
    offset <- offset + ncol(X)
  }
  
  Ainv <- block_diag(bread_blocks)
  scores <- do.call(cbind, score_blocks)
  V <- Ainv %*% crossprod(scores) %*% Ainv
  
  b <- coefs[tested_idx]
  Vb <- V[tested_idx, tested_idx]
  chi_sq <- as.numeric(t(b) %*% solve(Vb, b))
  
  c(statistic = chi_sq, df = length(b), p.value = pchisq(chi_sq, length(b), lower.tail = FALSE))
}

base_experiment <- read_dta(file.path(rp_dir, "data/riskpreference_pre.dta")) %>%
  mutate(id = id_as_char(id)) %>%
  group_by(id) %>%
  summarise(
    ccei = mean(ccei_ind, na.rm = TRUE),
    risk_aversion = {
      x_expensive <- ifelse(intercept_x < intercept_y, coord_x, coord_y)
      ra <- ifelse(coord_x + coord_y == 0, NA_real_, x_expensive / (coord_x + coord_y))
      mean(ra[round_number >= 1 & round_number <= 18], na.rm = TRUE)
    },
    .groups = "drop"
  )

baseline_ids <- base_experiment$id

male <- read_dta(file.path(rp_dir, "data/male.dta")) %>%
  transmute(
    id = id_as_char(id),
    male = case_when(num(male) == 1 ~ 1, num(male) %in% c(0, 2) ~ 0, TRUE ~ NA_real_)
  ) %>%
  dedupe_id()

height <- read_dta(file.path(rp_dir, "data/height.dta")) %>%
  transmute(id = id_as_char(id), height = num(height)) %>%
  dedupe_id()

network <- read_dta(file.path(rp_dir, "data/network_survey.dta")) %>%
  make_network_degree()

cog_pre <- read_xlsx_first_sheet(file.path(rp_dir, "data/Cognitive_raw_pre_full.xlsx")) %>%
  mutate(
    id = convert_participant_label(label),
    cq1 = zero_to_na(cq1),
    cq2 = zero_to_na(cq2),
    cq3 = zero_to_na(cq3),
    cq4 = zero_to_na(cq4),
    cq5 = zero_to_na(cq5),
    n_cq_nonmissing = rowSums(!is.na(cbind(cq1, cq2, cq3, cq4, cq5))),
    math_score = score_math5(cq1, cq2, cq3, cq4, cq5, answers = c(2, 4, 3, 5, 1))
  ) %>%
  filter(n_cq_nonmissing > 0) %>%
  select(id, math_score, n_cq_nonmissing) %>%
  dedupe_id(prefer_col = "n_cq_nonmissing") %>%
  select(id, math_score)

noncog_pre <- read_xlsx_first_sheet(file.path(rp_dir, "data/NonCognitive_raw_pre.xlsx")) %>%
  mutate(
    id = id_as_char(id_new),
    outgoing = row_mean_strict(zero_to_na(Player.q33), rev5(Player.q38)),
    agreeable = row_mean_strict(rev5(Player.q34), zero_to_na(Player.q39)),
    conscientious = row_mean_strict(zero_to_na(Player.q35), rev5(Player.q40)),
    emotional_stability = row_mean_strict(rev5(Player.q36), zero_to_na(Player.q41)),
    openness = row_mean_strict(zero_to_na(Player.q37), rev5(Player.q42))
  ) %>%
  select(id, outgoing, agreeable, conscientious, emotional_stability, openness) %>%
  dedupe_id()

all_baseline <- tibble(id = baseline_ids) %>%
  left_join(base_experiment, by = "id") %>%
  left_join(male, by = "id") %>%
  left_join(height, by = "id") %>%
  left_join(cog_pre, by = "id") %>%
  left_join(noncog_pre, by = "id") %>%
  left_join(network, by = "id")

balanced <- read_dta(file.path(rp_dir, "data/panel_individual.dta")) %>%
  filter(post == 0) %>%
  transmute(
    id = id_as_char(id),
    ccei = ccei_i,
    risk_aversion = RA_i,
    male = male_i,
    height = height_i,
    math_score = ifelse(num(mathscore_i_missing) == 1, NA_real_, mathscore_i),
    outgoing = ifelse(num(outgoing_i_missing) == 1, NA_real_, outgoing_i),
    agreeable = ifelse(num(agreeable_i_missing) == 1, NA_real_, agreeable_i),
    conscientious = ifelse(num(conscientious_i_missing) == 1, NA_real_, conscientious_i),
    emotional_stability = ifelse(num(stable_i_missing) == 1, NA_real_, stable_i),
    openness = ifelse(num(opened_i_missing) == 1, NA_real_, opened_i),
    inclass_n_friends = inclass_n_friends_i,
    inclass_popularity = inclass_popularity_i
  )

randomization_draw <- read_dta(file.path(ra_dir, "data/finalized_panel_final_251203.dta")) %>%
  transmute(group_id, random_1 = num(random_1), random_2 = num(random_2))

randomization_covariates <- all_baseline %>%
  select(
    id, ccei, risk_aversion, male, height, math_score,
    inclass_n_friends, inclass_popularity,
    outgoing, agreeable, conscientious, emotional_stability, openness
  )

member_1_covariates <- randomization_covariates %>%
  rename(id_1 = id) %>%
  rename_with(~ paste0(.x, "_1"), -id_1)

member_2_covariates <- randomization_covariates %>%
  rename(id_2 = id) %>%
  rename_with(~ paste0(.x, "_2"), -id_2)

randomization_raw <- read_dta(file.path(rp_dir, "data/panel_final.dta")) %>%
  left_join(randomization_draw, by = "group_id") %>%
  transmute(
    group_id,
    class_fe = factor(num(class)),
    random_1,
    random_2,
    id_1 = id_as_char(id_mover_base),
    id_2 = id_as_char(id_nonmover_base)
  ) %>%
  left_join(member_1_covariates, by = "id_1") %>%
  left_join(member_2_covariates, by = "id_2") %>%
  mutate(
    height_1 = pmin(pmax(num(height_1), 146), 182),
    height_2 = pmin(pmax(num(height_2), 146), 182)
  )

select_first <- randomization_raw$random_1 > randomization_raw$random_2

pick_selected <- function(var) {
  ifelse(select_first, num(randomization_raw[[paste0(var, "_1")]]), num(randomization_raw[[paste0(var, "_2")]]))
}

pick_partner <- function(var) {
  ifelse(select_first, num(randomization_raw[[paste0(var, "_2")]]), num(randomization_raw[[paste0(var, "_1")]]))
}

randomization_df <- data.frame(
  ccei_i = pick_selected("ccei"),
  ccei_j = pick_partner("ccei"),
  risk_aversion_i = pick_selected("risk_aversion"),
  risk_aversion_j = pick_partner("risk_aversion"),
  math_score_i = pick_selected("math_score"),
  math_score_j = pick_partner("math_score"),
  male_i = pick_selected("male"),
  male_j = pick_partner("male"),
  outgoing_i = pick_selected("outgoing"),
  outgoing_j = pick_partner("outgoing"),
  openness_i = pick_selected("openness"),
  openness_j = pick_partner("openness"),
  agreeable_i = pick_selected("agreeable"),
  agreeable_j = pick_partner("agreeable"),
  conscientious_i = pick_selected("conscientious"),
  conscientious_j = pick_partner("conscientious"),
  emotional_stability_i = pick_selected("emotional_stability"),
  emotional_stability_j = pick_partner("emotional_stability"),
  height_i = pick_selected("height"),
  height_j = pick_partner("height"),
  inclass_n_friends_i = pick_selected("inclass_n_friends"),
  inclass_n_friends_j = pick_partner("inclass_n_friends"),
  inclass_popularity_i = pick_selected("inclass_popularity"),
  inclass_popularity_j = pick_partner("inclass_popularity"),
  class_fe = randomization_raw$class_fe
)

vars <- tibble(
  var = c(
    "ccei", "risk_aversion",
    "male", "height", "math_score",
    "inclass_n_friends", "inclass_popularity",
    "outgoing", "agreeable", "conscientious", "emotional_stability", "openness"
  ),
  label = c(
    "Individual CCEI", "Risk aversion",
    "Male", "Height", "Math score",
    "In-class friends", "In-class popularity",
    "Outgoingness", "Agreeableness", "Conscientiousness", "Emotional stability", "Openness"
  ),
  panel = c(
    rep("experimental", 2),
    rep("survey", 10)
  )
)

randomization_specs <- vars %>%
  transmute(
    var,
    y = paste0(var, "_i"),
    x = paste0(var, "_j")
  )

summarise_randomization_var <- function(var, y, x) {
  fit <- lm(as.formula(paste(y, "~", x, "+ class_fe")), data = randomization_df)
  co <- summary(fit)$coefficients[x, ]
  
  tibble(
    var = var,
    `Randomization beta` = fmt(co[["Estimate"]]),
    `Randomization p-value` = fmt(co[["Pr(>|t|)"]]),
    `Randomization N` = nobs(fit)
  )
}

randomization_results <- bind_rows(Map(summarise_randomization_var, randomization_specs$var, randomization_specs$y, randomization_specs$x))
randomization_joint <- joint_suest_wald(randomization_df, randomization_specs)
randomization_joint_text <- sprintf(
  "$\\chi^{2}(%d) = %.2f$, $p = %.3f$",
  randomization_joint[["df"]], randomization_joint[["statistic"]], randomization_joint[["p.value"]]
)

summarise_var <- function(var, label) {
  x_all <- all_baseline[[var]]
  x_bal <- balanced[[var]]
  test <- tryCatch(t.test(x_all, x_bal), error = function(e) NULL)
  fmt_mean <- function(x) {
    if (identical(var, "height")) sprintf("%.1f", x) else fmt(x)
  }
  
  tibble(
    var = var,
    panel = vars$panel[match(var, vars$var)],
    Characteristic = label,
    `Baseline students` = fmt_mean(mean(x_all, na.rm = TRUE)),
    `Analysis sample` = fmt_mean(mean(x_bal, na.rm = TRUE)),
    Difference = fmt(mean(x_bal, na.rm = TRUE) - mean(x_all, na.rm = TRUE)),
    `p-value` = if (is.null(test)) "" else sprintf("%.3f", test$p.value)
  )
}

table_df <- bind_rows(Map(summarise_var, vars$var, vars$label)) %>%
  left_join(randomization_results, by = "var")

valid_value_vars <- c("math_score", "outgoing", "agreeable", "conscientious", "emotional_stability", "openness")
valid_baseline_counts <- sapply(all_baseline[valid_value_vars], function(x) sum(!is.na(x)))
valid_analysis_counts <- sapply(balanced[valid_value_vars], function(x) sum(!is.na(x)))
stopifnot(length(unique(valid_baseline_counts)) == 1, length(unique(valid_analysis_counts)) == 1)
valid_value_note <- paste0(
  "baseline $N=", fmt_n(unique(valid_baseline_counts)), "$; ",
  "analysis-sample $N=", fmt_n(unique(valid_analysis_counts)), "$"
)

joint_df <- all_baseline %>%
  mutate(in_balanced = as.integer(id %in% balanced$id)) %>%
  select(in_balanced, all_of(vars$var)) %>%
  drop_na()

joint_formula <- as.formula(paste("in_balanced ~", paste(vars$var, collapse = " + ")))
joint_fit <- lm(joint_formula, data = joint_df)
joint_test <- summary(joint_fit)$fstatistic
joint_p <- pf(joint_test[1], joint_test[2], joint_test[3], lower.tail = FALSE)
joint_text <- sprintf("$F(%d,%d) = %.2f$, $p = %.3f$", joint_test[2], joint_test[3], joint_test[1], joint_p)

make_rows <- function(df) {
  paste0(
    df$Characteristic, " & ",
    df$`Baseline students`, " & ",
    df$`Analysis sample`, " & ",
    df$Difference, " & ",
    df$`p-value`, " & ",
    df$`Randomization beta`, " & ",
    df$`Randomization p-value`, " \\\\"
  )
}

tex_lines <- c(
  "\\begin{table}[!ht]",
  "\\centering",
  "\\caption{Sample Attrition and Randomization Test}",
  "\\label{tab:attrition_balance}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "& \\multicolumn{4}{c}{Attrition} & \\multicolumn{2}{c}{Randomization Test} \\\\",
  "\\cmidrule(lr){2-5}\\cmidrule(lr){6-7}",
  "& \\multicolumn{2}{c}{Sample} & & & & \\\\ \\cmidrule(lr){2-3}",
  "& Baseline & Analysis & Diff. & $p$-value & $\\beta$ & $p$-value \\\\ \\midrule",
  " \\multicolumn{7}{l}{\\textbf{\\textit{Panel A: Experimental Measures}}} \\\\",
  make_rows(table_df %>% filter(panel == "experimental")),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\textit{\\textbf{Panel B: Survey Measures}}} \\\\",
  make_rows(table_df %>% filter(panel == "survey")),
  "\\midrule",
  paste0("Joint test: & \\multicolumn{4}{l}{\\textit{", joint_text, "}} & \\multicolumn{2}{l}{\\textit{", randomization_joint_text, "}} \\\\"),
  paste0("N & ", n_distinct(all_baseline$id), " & ", n_distinct(balanced$id), " & & & \\multicolumn{2}{c}{652} \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\medskip",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "{\\emph{Notes}: The baseline column reports means for all students who participated in the baseline decision-making experiment; ",
    "the analysis-sample column reports means for students in the analysis sample. ",
    "Differences equal analysis-sample mean minus baseline mean. ",
    "Attrition $p$-values come from two-sample $t$-tests. ",
    "For math score and Big Five personality measures, means and attrition $p$-values are computed among students with nonmissing valid values (", valid_value_note, "); all other rows use the full available samples reported in the $N$ row. ",
    "The same valid-value restriction is used for the corresponding randomization-test regressions. ",
    "The attrition joint test regresses an indicator for inclusion in the analysis sample on all listed characteristics and tests that all coefficients are jointly zero. ",
    "The randomization-test columns report the coefficient and $p$-value from regressions of one randomly selected pair member's baseline characteristic on the partner's corresponding baseline characteristic, controlling for class fixed effects. ",
    "The random member is selected using the fixed random draw stored in the finalized pair-level panel. ",
    "The randomization-test joint statistic is a Wald test that all twelve partner-characteristic coefficients are jointly zero.}"
  ),
  "\\end{minipage}",
  "\\end{table}"
)

dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
writeLines(tex_lines, out_tex)

diagnostics <- tibble(
  item = c(
    "baseline_ids_in_riskpreference_pre",
    "analysis_ids_in_panel_individual",
    "analysis_ids_absent_from_baseline_file",
    "joint_test_complete_cases",
    "joint_test_f",
    "joint_test_p",
    "randomization_pairs",
    "randomization_joint_chisq",
    "randomization_joint_p"
  ),
  value = c(
    n_distinct(all_baseline$id),
    n_distinct(balanced$id),
    sum(!unique(balanced$id) %in% unique(all_baseline$id)),
    nrow(joint_df),
    unname(joint_test[1]),
    joint_p,
    nrow(randomization_df),
    randomization_joint[["statistic"]],
    randomization_joint[["p.value"]]
  )
)

write.csv(diagnostics, out_diag, row.names = FALSE)

cat("Wrote ", out_tex, "\n", sep = "")
cat("Wrote ", out_diag, "\n", sep = "")
