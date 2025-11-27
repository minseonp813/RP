rm(list = ls())

# 데이터 로드
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)

# 1. round_number == 2 기준, 유효한 쌍 필터링
data_filtered <- baseline_raw[
  baseline_raw$round_number == 2 &
    baseline_raw$partner_id != "0" &
    baseline_raw$id != baseline_raw$partner_id,
]

# 2. group_id 생성 (id, partner_id의 순서 일관성을 위해 pmax/pmin)
data_filtered$group_id <- paste0(
  pmax(data_filtered$id, data_filtered$partner_id),
  pmin(data_filtered$id, data_filtered$partner_id)
)

# 3. group_id 기준 중복 제거 (유일한 그룹만)
unique_groups <- data_filtered[!duplicated(data_filtered$group_id), ]

# 4. 초기 테이블 구성
MPI_final <- unique_groups[, c("id", "partner_id", "group_id", "mover")]

# 5. mover 정보 보정 (round_number == 19 기준)
raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

MPI_final <- merge(
  MPI_final, mover_info,
  by = "id", all.x = TRUE, suffixes = c("", "_from_raw")
)

# 6. 빈 mover 값 보정
MPI_final$mover <- ifelse(
  is.na(MPI_final$mover) | MPI_final$mover == "",
  MPI_final$mover_from_raw,
  MPI_final$mover
)

# 7. 임시 열 제거
MPI_final$mover_from_raw <- NULL

# 저장
save(MPI_final, file = "../results/MPI_final.RData")

################################################################

# 필요한 패키지
library(revpref)

# 데이터 로드
load("../data/endline_raw.RData")

# 문자열 처리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)

sub1 <- subset(
  endline_raw,
  game_type == "individual" &
    round_number %in% 1:18 &
    id == "1110111" &
    id != partner_id
)

# 가격과 소비 행렬 구성
p <- cbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
q <- cbind(sub1$coord_x, sub1$coord_y)

# 계산
ccei_val <- ccei(p, q)
mpi_val <- mpi(p, q)

min_mpi <- mpi_val[1]
max_mpi <- mpi_val[2]

# 결과 출력
cat("CCEI:", ccei_val, "\n")
cat("Minimum MPI:", min_mpi, "\n")
cat("Maximum MPI:", max_mpi, "\n")


#####################################################

rm(list = ls())

library(revpref)                    # MPI 계산 함수가 포함된 CRAN 패키지
load("../data/baseline_raw.RData")  # 실험 원자료
load("../results/MPI_final.RData")  # group_id, id, partner_id 등이 들어 있음

# 문자형 통일
baseline_raw$id         <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
MPI_final$id            <- as.character(MPI_final$id)
MPI_final$partner_id    <- as.character(MPI_final$partner_id)

indiv_raw <- subset(
  baseline_raw,
  game_type   == "individual" &
    round_number %in% 1:18       &
    id != partner_id               # 안전장치
)

MPI_final$min_mpi_1 <- NA
MPI_final$max_mpi_1 <- NA
MPI_final$min_mpi_2 <- NA
MPI_final$max_mpi_2 <- NA


for (i in seq_len(nrow(MPI_final))) {
  id1  <- MPI_final$id[i]
  dat1 <- indiv_raw[indiv_raw$id == id1, ]
  
  if (nrow(dat1) > 0) {
    p1 <- cbind(1 / dat1$intercept_x, 1 / dat1$intercept_y)   # 가격행렬 Tx2
    q1 <- cbind(dat1$coord_x,       dat1$coord_y)             # 소비행렬 Tx2
    mpi1 <- mpi(p1, q1)                                       # 길이 2 벡터 반환
    MPI_final$min_mpi_1[i] <- mpi1[1]
    MPI_final$max_mpi_1[i] <- mpi1[2]
  }
  
  id2  <- MPI_final$partner_id[i]
  dat2 <- indiv_raw[indiv_raw$id == id2, ]
  
  if (nrow(dat2) > 0) {
    p2 <- cbind(1 / dat2$intercept_x, 1 / dat2$intercept_y)
    q2 <- cbind(dat2$coord_x,       dat2$coord_y)
    mpi2 <- mpi(p2, q2)
    MPI_final$min_mpi_2[i] <- mpi2[1]
    MPI_final$max_mpi_2[i] <- mpi2[2]
  }
}

save(MPI_final, file = "../results/MPI_final.RData")

######################################################

rm(list = ls())

library(revpref)
load("../data/baseline_raw.RData")
load("../results/MPI_final.RData")

baseline_raw$id         <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
MPI_final$id            <- as.character(MPI_final$id)
MPI_final$partner_id    <- as.character(MPI_final$partner_id)

grp_raw <- subset(
  baseline_raw,
  round_number >= 19 & round_number <= 36 &
    id != partner_id &
    partner_id != "0"
)

id_list <- sort(unique(grp_raw$id))
res <- data.frame(
  id        = id_list,
  min_mpi_g = NA_real_,
  max_mpi_g = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(id_list)) {
  sub <- grp_raw[grp_raw$id == id_list[i], ]
  if (nrow(sub) == 0) next
  p <- cbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  q <- cbind(sub$coord_x,        sub$coord_y)
  v <- mpi(p, q)
  res$min_mpi_g[i] <- v[1]
  res$max_mpi_g[i] <- v[2]
}

MPI_final$min_mpi_g <- NA_real_
MPI_final$max_mpi_g <- NA_real_

for (i in seq_len(nrow(res))) {
  rows <- which(MPI_final$id == res$id[i] | MPI_final$partner_id == res$id[i])
  if (length(rows) == 0) next
  MPI_final$min_mpi_g[rows] <- res$min_mpi_g[i]
  MPI_final$max_mpi_g[rows] <- res$max_mpi_g[i]
}

save(MPI_final, file = "../results/MPI_final.RData")

############################################################3

library(haven)

rm(list = ls())

# 결과 파일 불러오기
load("../results/MPI_final.RData")   # group_id + 6개 MPI 변수 포함
load("../results/panel_final.RData") # group_id 기준 패널

# group_id로 머지
panel_final <- merge(
  panel_final,
  MPI_final[, c("group_id",
                "min_mpi_1", "max_mpi_1",
                "min_mpi_2", "max_mpi_2",
                "min_mpi_g", "max_mpi_g")],
  by = "group_id",
  all.x = TRUE, sort = FALSE
)

# 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")


###############################################################

### 0. 기본 세팅
rm(list = ls())
library(revpref)

load("../data/endline_raw.RData")
endline_raw$id         <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)

### 1. 페어 테이블 생성
tmp <- subset(endline_raw, round_number == 2 &
                partner_id != "0" & id != partner_id)
tmp$group_id <- paste0(pmax(tmp$id, tmp$partner_id),
                       pmin(tmp$id, tmp$partner_id))
MPI_endline_final <- tmp[!duplicated(tmp$group_id),
                         c("id","partner_id","group_id","mover")]

mov <- subset(endline_raw, round_number == 19 & id != partner_id,
              select = c(id, mover))
mov <- mov[!duplicated(mov$id), ]
MPI_endline_final <- merge(MPI_endline_final, mov,
                           by = "id", all.x = TRUE, suffixes = c("", "_x"))
MPI_endline_final$mover[is.na(MPI_endline_final$mover)] <-
  MPI_endline_final$mover_x[is.na(MPI_endline_final$mover)]
MPI_endline_final$mover_x <- NULL

### 2. 개인 게임(1–18) MPI
ind_raw <- subset(endline_raw, game_type == "individual" &
                    round_number %in% 1:18 & id != partner_id)

MPI_endline_final[c("min_mpi_1","max_mpi_1",
                    "min_mpi_2","max_mpi_2")] <- NA_real_

for (r in 1:nrow(MPI_endline_final)) {
  for (k in 1:2) {
    pid <- if (k==1) MPI_endline_final$id[r] else MPI_endline_final$partner_id[r]
    sub <- ind_raw[ind_raw$id == pid, ]
    if (nrow(sub)==0) next
    p <- cbind(1/sub$intercept_x, 1/sub$intercept_y)
    q <- cbind(sub$coord_x,        sub$coord_y)
    v <- mpi(p,q)
    MPI_endline_final[r, paste0(c("min_mpi_","max_mpi_"),k)] <- v
  }
}

### 3. 그룹 게임(19–36) MPI
grp_raw <- subset(endline_raw, round_number >= 19 & round_number <= 36 &
                    id != partner_id & partner_id != "0")

MPI_endline_final$min_mpi_g <- NA_real_
MPI_endline_final$max_mpi_g <- NA_real_

for (r in 1:nrow(MPI_endline_final)) {
  sub <- grp_raw[grp_raw$id == MPI_endline_final$id[r], ]
  if (nrow(sub)==0) next
  p <- cbind(1/sub$intercept_x, 1/sub$intercept_y)
  q <- cbind(sub$coord_x,        sub$coord_y)
  v <- mpi(p,q)
  MPI_endline_final$min_mpi_g[r] <- v[1]
  MPI_endline_final$max_mpi_g[r] <- v[2]
}

cols_old <- c("min_mpi_1", "max_mpi_1",
              "min_mpi_2", "max_mpi_2",
              "min_mpi_g", "max_mpi_g")
cols_new <- paste0(cols_old, "_end")
names(MPI_endline_final)[match(cols_old, names(MPI_endline_final))] <- cols_new


save(MPI_endline_final, file = "../results/MPI_endline_final.RData")


names(MPI_endline_final)

################################################################


library(haven)

rm(list = ls())

## 파일 불러오기
load("../results/MPI_endline_final.RData")   # group_id + *_end 변수
load("../results/panel_final.RData")         # 기존 패널 (id.x, partner_id.x 등 포함)

## 병합: group_id 기준으로 *_end 변수들 추가
panel_final <- merge(
  panel_final,
  MPI_endline_final[, c("group_id",
                        "min_mpi_1_end", "max_mpi_1_end",
                        "min_mpi_2_end", "max_mpi_2_end",
                        "min_mpi_g_end", "max_mpi_g_end")],
  by = "group_id",
  all.x = TRUE,
  sort = FALSE
)

## 🔁 flip 처리 (baseline 기준 id.x가 endline 기준 partner_id와 같을 경우)
flip_idx <- which(panel_final$id_x == panel_final$partner_id_y)

# 임시 저장 후 swap
tmp_min_1 <- panel_final$min_mpi_1_end[flip_idx]
tmp_max_1 <- panel_final$max_mpi_1_end[flip_idx]

panel_final$min_mpi_1_end[flip_idx] <- panel_final$min_mpi_2_end[flip_idx]
panel_final$max_mpi_1_end[flip_idx] <- panel_final$max_mpi_2_end[flip_idx]

panel_final$min_mpi_2_end[flip_idx] <- tmp_min_1
panel_final$max_mpi_2_end[flip_idx] <- tmp_max_1

## 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")



################################################################################

rm(list = ls())
library(revpref)

# 원자료 및 기존 결과 불러오기
load("../data/baseline_raw.RData")
load("../results/MPI_final.RData")

# 문자형 통일
baseline_raw$id         <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
MPI_final$id            <- as.character(MPI_final$id)
MPI_final$partner_id    <- as.character(MPI_final$partner_id)

# 게임 전체 사용
full_raw <- subset(baseline_raw, round_number %in% 1:36 & id != partner_id)

# 칼럼 초기화
MPI_final$min_mpi_1g <- NA_real_
MPI_final$max_mpi_1g <- NA_real_
MPI_final$min_mpi_2g <- NA_real_
MPI_final$max_mpi_2g <- NA_real_
MPI_final$min_mpi_hlg <- NA_real_
MPI_final$max_mpi_hlg <- NA_real_

# 확인용: 몇 개 선택으로 계산했는지
MPI_final$n_1g <- NA_integer_
MPI_final$n_2g <- NA_integer_
MPI_final$n_hlg <- NA_integer_

### 🔁 루프 돌면서 계산
for (i in seq_len(nrow(MPI_final))) {
  id1 <- MPI_final$id[i]
  id2 <- MPI_final$partner_id[i]
  
  ## --- MPI_1g
  sub_1g <- full_raw[full_raw$id == id1, ]
  if (nrow(sub_1g) > 0) {
    p <- cbind(1 / sub_1g$intercept_x, 1 / sub_1g$intercept_y)
    q <- cbind(sub_1g$coord_x,         sub_1g$coord_y)
    v <- mpi(p, q)
    MPI_final$min_mpi_1g[i] <- v[1]
    MPI_final$max_mpi_1g[i] <- v[2]
    MPI_final$n_1g[i] <- nrow(sub_1g)
  }
  
  ## --- MPI_2g
  sub_2g <- full_raw[full_raw$id == id2, ]
  if (nrow(sub_2g) > 0) {
    p <- cbind(1 / sub_2g$intercept_x, 1 / sub_2g$intercept_y)
    q <- cbind(sub_2g$coord_x,         sub_2g$coord_y)
    v <- mpi(p, q)
    MPI_final$min_mpi_2g[i] <- v[1]
    MPI_final$max_mpi_2g[i] <- v[2]
    MPI_final$n_2g[i] <- nrow(sub_2g)
  }
  
  ## --- MPI_hlg
  sub_1 <- full_raw[full_raw$id == id1 & full_raw$round_number %in% 1:18, ]
  sub_2 <- full_raw[full_raw$id == id2 & full_raw$round_number %in% 1:18, ]
  sub_g <- full_raw[full_raw$id == id1 & full_raw$round_number %in% 19:36, ]
  
  sub_hlg <- rbind(sub_1, sub_2, sub_g)
  if (nrow(sub_hlg) > 0) {
    p <- cbind(1 / sub_hlg$intercept_x, 1 / sub_hlg$intercept_y)
    q <- cbind(sub_hlg$coord_x,         sub_hlg$coord_y)
    v <- mpi(p, q)
    MPI_final$min_mpi_hlg[i] <- v[1]
    MPI_final$max_mpi_hlg[i] <- v[2]
    MPI_final$n_hlg[i] <- nrow(sub_hlg)
  }
}

save(MPI_final, file = "../results/MPI_final.RData")


##################################################################

### 🔄 초기화 및 데이터 로드
rm(list = ls())
library(revpref)

# 데이터 로드
load("../results/MPI_endline_final.RData")   # group_id 기준으로 존재하는 MPI_endline_final
load("../data/endline_raw.RData")            # 원시 데이터

# 문자형으로 변환
endline_raw$id         <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)

### ✅ MPI_1g, MPI_2g, MPI_hlg 계산을 위한 데이터 전처리
full_raw <- subset(endline_raw, round_number %in% 1:36 & id != partner_id)

# 새 칼럼 초기화
MPI_endline_final$min_mpi_1g <- NA_real_
MPI_endline_final$max_mpi_1g <- NA_real_
MPI_endline_final$min_mpi_2g <- NA_real_
MPI_endline_final$max_mpi_2g <- NA_real_
MPI_endline_final$min_mpi_hlg <- NA_real_
MPI_endline_final$max_mpi_hlg <- NA_real_

# 확인용 라운드 개수
MPI_endline_final$n_1g <- NA_integer_
MPI_endline_final$n_2g <- NA_integer_
MPI_endline_final$n_hlg <- NA_integer_

### 🔁 루프를 돌며 계산
for (r in 1:nrow(MPI_endline_final)) {
  ### --- MPI_1g
  sub_1g <- full_raw[full_raw$id == MPI_endline_final$id[r], ]
  if (nrow(sub_1g) > 0) {
    p <- cbind(1 / sub_1g$intercept_x, 1 / sub_1g$intercept_y)
    q <- cbind(sub_1g$coord_x,         sub_1g$coord_y)
    v <- mpi(p, q)
    MPI_endline_final$min_mpi_1g[r] <- v[1]
    MPI_endline_final$max_mpi_1g[r] <- v[2]
    MPI_endline_final$n_1g[r] <- nrow(sub_1g)
  }
  
  ### --- MPI_2g
  sub_2g <- full_raw[full_raw$id == MPI_endline_final$partner_id[r], ]
  if (nrow(sub_2g) > 0) {
    p <- cbind(1 / sub_2g$intercept_x, 1 / sub_2g$intercept_y)
    q <- cbind(sub_2g$coord_x,         sub_2g$coord_y)
    v <- mpi(p, q)
    MPI_endline_final$min_mpi_2g[r] <- v[1]
    MPI_endline_final$max_mpi_2g[r] <- v[2]
    MPI_endline_final$n_2g[r] <- nrow(sub_2g)
  }
  
  ### --- MPI_hlg (1번 개인 18개 + 2번 개인 18개 + 그룹 18개)
  sub_1 <- full_raw[full_raw$id == MPI_endline_final$id[r] &
                      full_raw$round_number %in% 1:18, ]
  sub_2 <- full_raw[full_raw$id == MPI_endline_final$partner_id[r] &
                      full_raw$round_number %in% 1:18, ]
  sub_g <- full_raw[full_raw$id == MPI_endline_final$id[r] &
                      full_raw$round_number %in% 19:36, ]
  
  sub_hlg <- rbind(sub_1, sub_2, sub_g)
  if (nrow(sub_hlg) > 0) {
    p <- cbind(1 / sub_hlg$intercept_x, 1 / sub_hlg$intercept_y)
    q <- cbind(sub_hlg$coord_x,         sub_hlg$coord_y)
    v <- mpi(p, q)
    MPI_endline_final$min_mpi_hlg[r] <- v[1]
    MPI_endline_final$max_mpi_hlg[r] <- v[2]
    MPI_endline_final$n_hlg[r] <- nrow(sub_hlg)
  }
}

### 💾 저장
save(MPI_endline_final, file = "../results/MPI_endline_final.RData")

#######################################################

# 필요한 패키지
library(revpref)

# 데이터 로드
load("../data/endline_raw.RData")

# 문자열 처리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)

### 대상자 설정
id_1 <- "1110102"  # 1g, hlg
id_2 <- "1110105"  # 2g, hlg

### --- MPI_1g 계산 (id_1의 1~36 라운드)
sub_1g <- subset(endline_raw,
                 id == id_1 &
                   round_number %in% 1:36 &
                   id != partner_id)
p_1g <- cbind(1 / sub_1g$intercept_x, 1 / sub_1g$intercept_y)
q_1g <- cbind(sub_1g$coord_x,         sub_1g$coord_y)
mpi_1g <- mpi(p_1g, q_1g)

### --- MPI_2g 계산 (id_2의 1~36 라운드)
sub_2g <- subset(endline_raw,
                 id == id_2 &
                   round_number %in% 1:36 &
                   id != partner_id)
p_2g <- cbind(1 / sub_2g$intercept_x, 1 / sub_2g$intercept_y)
q_2g <- cbind(sub_2g$coord_x,         sub_2g$coord_y)
mpi_2g <- mpi(p_2g, q_2g)

### --- MPI_hlg 계산 (id_1 개인 18 + id_2 개인 18 + id_1 그룹 18 = 54개)
sub_1_ind <- subset(endline_raw,
                    id == id_1 &
                      game_type == "individual" &
                      round_number %in% 1:18)
sub_2_ind <- subset(endline_raw,
                    id == id_2 &
                      game_type == "individual" &
                      round_number %in% 1:18)
sub_grp <- subset(endline_raw,
                  id == id_1 &
                    round_number %in% 19:36)

sub_hlg <- rbind(sub_1_ind, sub_2_ind, sub_grp)
p_hlg <- cbind(1 / sub_hlg$intercept_x, 1 / sub_hlg$intercept_y)
q_hlg <- cbind(sub_hlg$coord_x,         sub_hlg$coord_y)
mpi_hlg <- mpi(p_hlg, q_hlg)

### 결과 출력
cat("MPI_1g (id =", id_1, ")\n")
cat("  min:", mpi_1g[1], "max:", mpi_1g[2], "n =", nrow(sub_1g), "\n\n")

cat("MPI_2g (id =", id_2, ")\n")
cat("  min:", mpi_2g[1], "max:", mpi_2g[2], "n =", nrow(sub_2g), "\n\n")

cat("MPI_hlg (id1 =", id_1, ", id2 =", id_2, ")\n")
cat("  min:", mpi_hlg[1], "max:", mpi_hlg[2], "n =", nrow(sub_hlg), "\n")

################################################################3

### 초기화 및 로드
rm(list = ls())
library(haven)

# 데이터 로드
load("../results/panel_final.RData")
load("../results/MPI_final.RData")          # baseline
load("../results/MPI_endline_final.RData")  # endline

# 1️⃣ Baseline MPI 병합 (이름 그대로)
panel_final <- merge(
  panel_final,
  MPI_final[, c("group_id",
                "min_mpi_1g", "max_mpi_1g",
                "min_mpi_2g", "max_mpi_2g",
                "min_mpi_hlg", "max_mpi_hlg")],
  by = "group_id",
  all.x = TRUE,
  sort = FALSE
)

# 2️⃣ Endline MPI 병합 (_end 붙이기)
panel_final <- merge(
  panel_final,
  MPI_endline_final[, c("group_id",
                        "min_mpi_1g", "max_mpi_1g",
                        "min_mpi_2g", "max_mpi_2g",
                        "min_mpi_hlg", "max_mpi_hlg")],
  by = "group_id",
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("", "_end")
)

# 3️⃣ Flip 처리 (endline 기준 id_x == partner_id_y)
flip_idx <- which(panel_final$id_x == panel_final$partner_id_y)

# min/max_mpi_1g_end <-> min/max_mpi_2g_end swap
tmp_min_1g <- panel_final$min_mpi_1g_end[flip_idx]
tmp_max_1g <- panel_final$max_mpi_1g_end[flip_idx]

panel_final$min_mpi_1g_end[flip_idx] <- panel_final$min_mpi_2g_end[flip_idx]
panel_final$max_mpi_1g_end[flip_idx] <- panel_final$max_mpi_2g_end[flip_idx]

panel_final$min_mpi_2g_end[flip_idx] <- tmp_min_1g
panel_final$max_mpi_2g_end[flip_idx] <- tmp_max_1g

# ✅ 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

#############################################################################

### 초기화 및 로드
rm(list = ls())
library(haven)

load("../results/panel_final.RData")

### 1. 기존 잘못된 열 삭제 (중요한 new2는 보존하고, 지정된 12개만 삭제)
bad_cols <- c(
  # 평균 관련 열
  "mean_mpi_1g","mean_mpi_2g","mean_mpi_hlg",
  "mean_mpi_1g_end","mean_mpi_2g_end","mean_mpi_hlg_end",
  # high 판단 및 정렬 열
  "min_mpi_high","min_mpi_high_end","max_mpi_high","max_mpi_high_end",
  "mean_mpi_high","mean_mpi_high_end",
  "min_mpi_hg","min_mpi_lg","min_mpi_hg_end","min_mpi_lg_end",
  "max_mpi_hg","max_mpi_lg","max_mpi_hg_end","max_mpi_lg_end",
  "mean_mpi_hg","mean_mpi_lg","mean_mpi_hg_end","mean_mpi_lg_end",
  # 보조 열
  grep("^one_minus_", names(panel_final),  value = TRUE),
  # ❗ 딱 지정한 new2_* 열만 삭제
  "new2_min_MPI_hg",      "new2_min_MPI_lg",
  "new2_min_MPI_hg_end",  "new2_min_MPI_lg_end",
  "new2_max_MPI_hg",      "new2_max_MPI_lg",
  "new2_max_MPI_hg_end",  "new2_max_MPI_lg_end",
  "new2_mean_MPI_hg",     "new2_mean_MPI_lg",
  "new2_mean_MPI_hg_end", "new2_mean_MPI_lg_end"
)
panel_final <- panel_final[, !names(panel_final) %in% bad_cols]

### 2. 평균 MPI 계산
panel_final$mean_mpi_1g      <- rowMeans(panel_final[, c("min_mpi_1g",      "max_mpi_1g")],      na.rm = TRUE)
panel_final$mean_mpi_2g      <- rowMeans(panel_final[, c("min_mpi_2g",      "max_mpi_2g")],      na.rm = TRUE)
panel_final$mean_mpi_hlg     <- rowMeans(panel_final[, c("min_mpi_hlg",     "max_mpi_hlg")],     na.rm = TRUE)
panel_final$mean_mpi_1g_end  <- rowMeans(panel_final[, c("min_mpi_1g_end",  "max_mpi_1g_end")],  na.rm = TRUE)
panel_final$mean_mpi_2g_end  <- rowMeans(panel_final[, c("min_mpi_2g_end",  "max_mpi_2g_end")],  na.rm = TRUE)
panel_final$mean_mpi_hlg_end <- rowMeans(panel_final[, c("min_mpi_hlg_end", "max_mpi_hlg_end")], na.rm = TRUE)

### 3. high 그룹 식별
prefixes <- c("min", "max", "mean")

for (p in prefixes) {
  # baseline
  cond1 <- panel_final[[paste0(p, "_mpi_1")]] < panel_final[[paste0(p, "_mpi_2")]]
  cond2 <- panel_final[[paste0(p, "_mpi_1")]] == panel_final[[paste0(p, "_mpi_2")]] &
    panel_final[[paste0(p, "_mpi_1g")]] < panel_final[[paste0(p, "_mpi_2g")]]
  
  panel_final[[paste0(p, "_mpi_high")]] <- ifelse(cond1 | cond2, 1, 0)
  
  # endline
  cond1e <- panel_final[[paste0(p, "_mpi_1_end")]] < panel_final[[paste0(p, "_mpi_2_end")]]
  cond2e <- panel_final[[paste0(p, "_mpi_1_end")]] == panel_final[[paste0(p, "_mpi_2_end")]] &
    panel_final[[paste0(p, "_mpi_1g_end")]] < panel_final[[paste0(p, "_mpi_2g_end")]]
  
  panel_final[[paste0(p, "_mpi_high_end")]] <- ifelse(cond1e | cond2e, 1, 0)
}

### 4. hg / lg 그룹 값 할당
for (p in prefixes) {
  # baseline
  panel_final[[paste0(p, "_mpi_hg")]] <- ifelse(panel_final[[paste0(p, "_mpi_high")]] == 1,
                                                panel_final[[paste0(p, "_mpi_1g")]],
                                                panel_final[[paste0(p, "_mpi_2g")]])
  
  panel_final[[paste0(p, "_mpi_lg")]] <- ifelse(panel_final[[paste0(p, "_mpi_high")]] == 1,
                                                panel_final[[paste0(p, "_mpi_2g")]],
                                                panel_final[[paste0(p, "_mpi_1g")]])
  
  # endline
  panel_final[[paste0(p, "_mpi_hg_end")]] <- ifelse(panel_final[[paste0(p, "_mpi_high_end")]] == 1,
                                                    panel_final[[paste0(p, "_mpi_1g_end")]],
                                                    panel_final[[paste0(p, "_mpi_2g_end")]])
  
  panel_final[[paste0(p, "_mpi_lg_end")]] <- ifelse(panel_final[[paste0(p, "_mpi_high_end")]] == 1,
                                                    panel_final[[paste0(p, "_mpi_2g_end")]],
                                                    panel_final[[paste0(p, "_mpi_1g_end")]])
}

### 5. 안전한 나눗셈 함수 정의
safe_ratio <- function(num, den) {
  ifelse(abs(den) < 1e-10, NA, num / den)
}

### 6. new2 Index 계산
for (p in prefixes) {
  for (sfx in c("", "_end")) {
    g    <- panel_final[[paste0(p, "_mpi_g",    sfx)]]
    hg   <- panel_final[[paste0(p, "_mpi_hg",   sfx)]]
    lg   <- panel_final[[paste0(p, "_mpi_lg",   sfx)]]
    hlg  <- panel_final[[paste0(p, "_mpi_hlg",  sfx)]]
    
    Ihg1 <- safe_ratio(hg  - g, hlg - g)
    Ilg1 <- 1 - Ihg1
    
    Ilg2 <- safe_ratio(lg  - g, hlg - g)
    Ihg2 <- 1 - Ilg2
    
    panel_final[[paste0("new2_", p, "_MPI_hg", sfx)]] <- rowMeans(cbind(Ihg1, Ihg2), na.rm = TRUE)
    panel_final[[paste0("new2_", p, "_MPI_lg", sfx)]] <- rowMeans(cbind(Ilg1, Ilg2), na.rm = TRUE)
  }
}

### 7. 검산: new2_hg + new2_lg == 1
for (p in prefixes) {
  for (sfx in c("", "_end")) {
    ihg <- panel_final[[paste0("new2_", p, "_MPI_hg", sfx)]]
    ilg <- panel_final[[paste0("new2_", p, "_MPI_lg", sfx)]]
    check <- abs(ihg + ilg - 1) < 1e-8
    cat(sprintf("✅ Check passed for new2_%s_MPI%s: %s\n", p, sfx, all(check, na.rm = TRUE)))
  }
}

### 8. 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")



# 결과 저장용 벡터 초기화
prefixes  <- c("min", "max", "mean")
suffixes  <- c("", "_end")
thresh    <- 0.001               # ← 0.1 이하를 잡아낼 임계값

for (p in prefixes) {
  for (sfx in suffixes) {
    g_var   <- paste0(p, "_mpi_g",   sfx)
    hlg_var <- paste0(p, "_mpi_hlg", sfx)
    
    # |g - hlg| ≤ 0.1 인 관측치 수 셈
    count <- sum(abs(panel_final[[g_var]] - panel_final[[hlg_var]]) <= thresh,
                 na.rm = TRUE)
    
    cat(sprintf("📌 분모 ≤ %.3f (%s%s): %d건\n", thresh, p, sfx, count))
  }
}


#############################################33

load("../results/panel_final.RData")  # panel_final 객체 불러오기

names(panel_final)

# 해당 ID
target_id <- 2210315

# id_x 또는 partner_id_x 중 하나라도 해당 ID인 행만 필터링
subset_panel <- subset(panel_final, id_x == target_id | partner_id_x == target_id)

# 결과 확인
print(subset_panel)
