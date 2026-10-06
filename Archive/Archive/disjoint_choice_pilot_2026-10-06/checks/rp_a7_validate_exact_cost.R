code <- "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code"
.libPaths(c(file.path(code, ".R-library"), .libPaths()))
source(file.path(code, "programs/calculate_rp_indices.R"))
dyn.load("/private/tmp/rp_a7_exact_cost.so")
native <- function(EX, side=NULL) .Call("rp_exact_cost", EX$E, side)
base <- rp_load_wave(file.path(code, "data/base_raw.dta"))
end <- rp_load_wave(file.path(code, "data/end_raw.dta"))
base$post <- 0L; end$post <- 1L
raw <- rbind(base,end)
pairs <- haven::read_dta(file.path(code,"data/panel_final.dta"))
raw <- raw[raw$group_id %in% pairs$group_id, ]
individual <- split(raw[raw$round_number<=18, ],
                    paste(raw$id[raw$round_number<=18],raw$post[raw$round_number<=18]))
group <- split(raw[raw$round_number>=19 & raw$mover==1, ],
               paste(raw$group_id[raw$round_number>=19 & raw$mover==1],
                     raw$post[raw$round_number>=19 & raw$mover==1]))
set.seed(20261006)
cases <- vector("list",2500)
for (k in seq_along(cases)) {
  one <- individual[[sample.int(length(individual),1)]]
  two <- individual[[sample.int(length(individual),1)]]
  gg <- group[[sample.int(length(group),1)]]
  n <- if (k %% 4 == 0) 18L else 9L
  one <- one[sample.int(18,n), ]; two <- two[sample.int(18,n), ]; gg <- gg[sample.int(18,n), ]
  if (k %% 5 == 0) {
    d <- one; side <- NULL
  } else {
    ii <- if (k %% 2 == 0) rbind(one,two) else one
    d <- rbind(ii,gg); side <- c(rep("I",nrow(ii)),rep("G",nrow(gg)))
  }
  cases[[k]] <- list(EX=rp_expenditure(d), side=side)
}
old_time <- system.time(reference <- vapply(cases,function(x) rp_cost_ccei(x$EX,x$side),numeric(1)))
new_time <- system.time(accelerated <- vapply(cases,function(x) native(x$EX,x$side),numeric(1)))
stopifnot(identical(reference, accelerated))
cat("PASS: 2,500 real-data individual/cross costs match bit for bit, using 9/18 choices.\n")
cat("R reference seconds:",old_time["elapsed"],"Native seconds:",new_time["elapsed"],"\n")

# Integer expenditure ties, all-equal choices, and coincident budgets.
set.seed(123)
for (k in 1:1000) {
  n <- sample(2:12,1)
  ix <- sample(c(10,20,30),n,TRUE); iy <- sample(c(10,20,30),n,TRUE)
  share <- sample(0:6,n,TRUE)/6
  d <- data.frame(coord_x=ix*share*6,coord_y=iy*(1-share)*6,
                  intercept_x=ix*6,intercept_y=iy*6)
  side <- sample(c("I","G"),n,TRUE)
  ex <- rp_expenditure(d)
  stopifnot(identical(rp_cost_ccei(ex),native(ex)),
            identical(rp_cost_ccei(ex,side),native(ex,side)))
}
cat("PASS: 2,000 synthetic costs, including exact expenditure ties.\n")
