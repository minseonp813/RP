code <- "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code"
.libPaths(c(file.path(code,".R-library"),.libPaths()))
source(file.path(code,"programs/calculate_rp_indices.R"))
pairs <- as.data.frame(haven::read_dta(file.path(code,"data/panel_final.dta")))
pairs <- pairs[order(pairs$group_id), ]
base <- rp_load_wave(file.path(code,"data/base_raw.dta")); base$post <- 0L
end <- rp_load_wave(file.path(code,"data/end_raw.dta")); end$post <- 1L
raw <- rbind(base,end)
raw <- raw[raw$group_id %in% pairs$group_id, ]
individual <- split(raw[raw$round_number<=18, ],
    paste(raw$id[raw$round_number<=18],raw$post[raw$round_number<=18]))
group <- split(raw[raw$round_number>=19 & raw$mover==1, ],
    paste(raw$group_id[raw$round_number>=19 & raw$mover==1],
          raw$post[raw$round_number>=19 & raw$mover==1]))
expected <- list()
for (post in 0:1) for (row in seq_len(nrow(pairs))) {
  wave <- if (post==0) "base" else "end"
  id1 <- pairs[[paste0("id_mover_",wave)]][row]
  id2 <- pairs[[paste0("id_nonmover_",wave)]][row]
  g <- pairs$group_id[row]
  choices <- list(one=individual[[paste(id1,post)]],two=individual[[paste(id2,post)]],
                  group=group[[paste(g,post)]])
  choices <- lapply(choices,function(d) d[order(d$round_number), ])
  set.seed(20260812L+100000L+post*nrow(pairs)+row)
  selected <- lapply(choices,function(d) sort(sample.int(18,9)))
  for (half in c("A","B")) {
    h <- Map(function(d,rows) d[if (half=="A") rows else setdiff(1:18,rows), ],choices,selected)
    cost <- function(i) rp_cost_ccei(rp_expenditure(rbind(i,h$group)),
                                    c(rep("I",nrow(i)),rep("G",9)))
    I <- rp_index(cost(h$one),cost(h$two),cost(rbind(h$one,h$two)))
    scores <- vapply(h[c("one","two")],function(d) 1-rp_cost_ccei(rp_expenditure(d)),numeric(1))
    corner <- vapply(h[c("one","two")],function(d) mean(d$coord_x==0|d$coord_y==0),numeric(1))
    mid <- vapply(h[c("one","two")],function(d) mean(d$coord_x==d$coord_y),numeric(1))
    expected[[length(expected)+1L]] <- data.frame(group_id=g,post=post,id=c(id1,id2),half=half,
      ccei=scores,partner_ccei=rev(scores),I=c(I,1-I),corner_share=corner,
      corner_diff=corner-rev(corner),mid_share=mid,mid_diff=mid-rev(mid))
  }
}
expected <- do.call(rbind,expected)
saveRDS(expected,"/private/tmp/rp_a7_reference_own_inputs.rds")
cat("Reference own inputs saved for",nrow(expected),"member-wave-half observations.\n")
