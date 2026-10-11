library(dplyr)
library(haven)
library(stringr)
root <- "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code"
audit <- "/private/tmp/rp_panel_consolidation_ad3i8_4z"
setwd(root)
expressions <- parse(file = "2_3_build_panel_individual.R")
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("id_as_char"))) eval(expression)
}
reference <- read_dta(file.path(audit, "prepared_panel.dta"))
panel_individual <- read_dta(file.path(audit, "original_panel_individual.dta")) %>%
  left_join(select(reference, id, post, M_ccei, n_ccei_donors), by = c("id", "post"))
source_lines <- readLines("2_3_build_panel_individual.R")
first <- grep("^# 9. Prepare exact-choice", source_lines, fixed = FALSE)
last <- grep("^# 10. Validate member", source_lines, fixed = FALSE)
eval(parse(text = source_lines[first:(last - 1)]))
expected <- setdiff(names(reference), "I_ccei_cached")
stopifnot(setequal(names(panel_individual), expected),
          identical(as.character(panel_individual$id), as.character(reference$id)),
          identical(as.numeric(panel_individual$post), as.numeric(reference$post)))
for (name in expected) {
  stopifnot(isTRUE(all.equal(panel_individual[[name]], reference[[name]],
                            tolerance = 0, check.attributes = FALSE)))
}
write_dta(panel_individual[expected], file.path(audit, "checked_panel_individual.dta"))
cat("PASS: new 2_3 preparation matches all", length(expected), "canonical columns exactly on",
    nrow(panel_individual), "rows, including observation order.\n")
