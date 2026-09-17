# cross-partition index e^x(S) / Ihat — 구현과 검증 결과

공저자 코드(`~/Desktop/group/`, R)를 확인하고 $e^\times(S)$ 계산 코드를 작성·검증함.

## 기존 코드 구조 (파악 완료)
- 데이터: `coord_x/y` = 선택 $x$, `intercept_x/y` = 예산선 절편, 가격 $p = 1/\text{intercept}$ (소득 1 정규화).
  round 1–18 = 개인, 19–36 = 그룹(공통 선택이라 `mover==1`만 사용).
- CCEI: `garp()`(weak/strict 관계행렬) → `warshall()`(이행폐쇄) → 위반 판정, `ccei_garp()`가 이분탐색(tol 1e-6).
- 파이프라인: 파트1(1–210행)이 `riskpreference_pre/post.dta` → `base_raw`/`end_raw` 정제,
  파트2(240행~)의 `compute_ccei_wave_measures()`가 CCEI와 `I_hg`/`I_lg`(= 노트의 $I_{ig}$) 계산.
  `den = ccei_g − ccei_hlg` $= c(\{i,j\})$, 두 시퀀스 평균 = Shapley 평균. 노트 공식과 동일 확인.

## 새 코드 (작성 완료)
`~/Desktop/group/programs/ex_cross.R` (새 파일):
- `cross_garp(p,x,side,e)` — 기존 `garp`의 판정부만 교체: "위반 존재?" → "**개인측·그룹측 관측치를
  모두 포함하고 내부에 strict 간선을 가진 SCC 존재?**". `warshall`은 그대로 재사용. 사이클 열거 없음.
- `ex_cross(p,x,side)` — `ccei_garp`와 동일한 이분탐색 래퍼.
- `compute_ex_from_subsets(sub_ind, sub_grp)`, `ihat_from_ex(ex_h, ex_l, ex_hl)` — 편의 함수.

`~/Desktop/group/programs/ex_integration_guide.R` — `01_calculate_ccei.R`에 `Ihat_hg`/`Ihat_lg`
컬럼을 추가하는 삽입 블록(원본은 미수정; 삽입 위치·재사용 서브셋 명시).

## 검증 (실데이터) — 6가지 독립 테스트, 전부 PASS
검증 스위트: `~/Desktop/group/programs/ex_cross_tests.R`
공저자 재현: 프로젝트 루트에서 `Rscript programs/ex_cross_tests.R` (읽기 전용, 파일 미생성).

| 테스트 | 내용 | 결과 |
|---|---|---|
| **T1 손계산 앵커** | 4관측 데이터, 손계산 $e^\times = 0.90$ | Warshall·igraph 모두 0.9000 ✓ |
| **T2 알고리즘 3중 대조** | `cross_garp`(Warshall) vs igraph 강연결성분 vs DFS 완전탐색, e 격자별 판정 일치 | 450 비교 중 **31개가 실제 cross 위반**, 불일치 0 ✓ |
| **T3 $e^\times$ 대조** | `ex_cross`(Warshall) vs igraph 구현, 60그룹 | max diff 0.00e+00 ✓ |
| **T4 구조 항등식** | 범위 $(0,1]$·연합 단조성 $e^\times(ij)\le\min$·efficiency $\hat I_i{+}\hat I_j{=}1$·Lemma 1 | 전부 위반 0 ✓ |
| **T5 $e^\times$ 고립** | $ccei(D_S){=}ccei(D_g){=}1$인 40그룹에서 $ccei(D_{Sg}){=}e^\times(S)$ (기존코드와 등호) | max diff 0.00e+00 ✓ |
| **T6 불변성** | 개인/그룹 라벨 swap·관측치 순서 셔플에 $e^\times$ 불변 | 위반 0 ✓ |

핵심: **T2**는 알고리즘이 완전히 다른 3개 구현(Warshall 도달성 / igraph Tarjan / DFS 사이클열거)이 실제
cross 위반 케이스 포함 모든 판정에서 일치 → 공통버그 아닌 한 정확. **T5**는 min의 다른 두 항을 1로
강제해 $e^\times$를 홀로 기존 CCEI 코드와 등호 대조 → Lemma 1의 약점(최솟값 아닐 때 미검증) 보완.

추가로 합성 400쌍·실데이터 80그룹(Lemma 1) 모두 잔차 0. 실행시간 80그룹 ~48초, 전체 ≈ 13분 예상.

## Ihat 첫 신호 (80그룹 샘플, higher-CCEI 멤버 관점)
| | I (원지표) | Ihat (신지표) | 헤드라인(전표본 I) |
|---|---|---|---|
| higher-CCEI 멤버 평균 거리 | 0.354 | 0.384 | 0.278 |
| within-pair gap (low−high) | 0.292 | **0.232** | — |
| Ihat_high < 0.5 비율 | — | 63.5% | — |
| Ihat_high vs 0.5 | — | t=−3.29, p=0.0015 | — |

→ **헤드라인 방향성(higher-CCEI 멤버가 그룹에 더 가까움 = 더 큰 영향력)은 Ihat에서도 유의하게 유지**되나,
gap이 약 20% 축소(0.292 → 0.232). 검토 결론(지표 교체가 기계적 성분을 *부분* 제거)과 정합.
정식 결론은 전표본 + HigherCCEI 회귀 + placebo 벤치마크 필요.

## 적대적 재검증 (57 에이전트) 및 발견된 버그 수정

6개 테스트 통과 후, **놓친 결함**을 찾는 57-에이전트 적대적 검증(41 finder × 12차원 + 16 verifier,
각자 실제 R로 반례 실행)을 수행. 결과: CONFIRMED 5 / PARTIAL 6 / REFUTED 5, 그리고 5개 차원
(comp-warshall, edge-cases, integration, ccei-convention, numerical-ties) 완전 clean.

**실질 버그 1개 (수정 완료):** `ihat_from_ex`의 미정의 가드(`chat_hl <= 1e-9`)가 이분탐색
해상도(`~2^-20 ≈ 9.5e-7`)보다 3자릿수 작아, **정확한 예산 tie**에서 실제로는 불일치가 없어
`Ihat=NA`여야 할 케이스가 거대한 가짜 값(예: 78643)을 반환 → 평균/집계 오염 위험.

**진단:** `cross_garp`의 SCC(closed-walk) 판정 자체는 정확 — 기존 `garp.R`·Lemma 1과 정합하고,
tie에서의 1e-6 오차는 기존 `ccei_garp`와 동일한 sup-미도달 관행(새 버그 아님). 문제는 그 미도달
값이 downstream 가드를 통과한 것.

**수정 (`ex_cross.R`):**
1. `ex_cross`에 **sup-미도달 snap**: 유일한 cross 위반이 닫힌 끝점 e=1(정확한 tie)에만 있으면
   e^x를 sup=1로 스냅(candidate ratio 최댓값 위로 올라갔는지로 판별).
2. `ihat_from_ex` 가드를 이분탐색 tolerance에 맞춤(`1e-9` → `1e-6`).
3. `cross_garp`에 closed-walk 의미 명시 주석.
4. 테스트 스위트에 **T7**(exact-tie 회귀 가드) 추가 + T2/T3 oracle 주석
   (DFS는 simple-cycle이라 tie에서 약함, igraph SCC가 신뢰 oracle).

**수정 검증:** 두 CONFIRMED 반례 모두 `ex_cross=1.0`, `Ihat=NA`로 교정. 기존 T1–T6 + 새 T7
전부 PASS (regression 없음). 손계산 앵커 0.90 유지.

**실데이터 영향 (전수 스캔, 1,524 group-waves):**
- guard-defeat band `chat_ij ∈ (1e-9, 1e-6]`: **0 그룹** — tie 발현 케이스 전무.
- true-undefined `chat_ij ≤ 1e-9`: 27 그룹 — 완전 정렬(불일치 없음), 옛/새 가드 모두 NA로 동일 처리.
- 옛 가드에서 Ihat이 [0,1] 밖: **0 그룹**. Ihat 요약 mean 0.493, sd 0.319, min 0, max 1 (정상).
- **결론: 이 버그는 실데이터의 e^x/Ihat 값을 바꾸지 않는다** (수정은 코드 견고성 확보 = 예방적).
  버그 자체는 진짜(재현됨)이나 이 데이터셋에선 발현 조건(e=1 정확한 cross-only tie)이 없었음.

## 산출물 (전달 묶음, 모두 `programs/`)
- `README_ex_cross.md` (안내), `ex_cross.R` (함수), `ex_cross_tests.R` (T1–T7),
  `ex_integration_guide.R` (통합법), `clean_pair_raw.R` (테스트 자립용), `ex_cross_validation.md` (본 문서)
- 검증에 쓴 중간 스크립트(합성검증·실데이터 스캔·전수 tie 스캔)는 작업용 임시 파일로 전달 묶음에는 미포함.

## 남은 작업
1. 전표본으로 `01_calculate_ccei.R` 통합 실행 (haven 설치됨; 원본에 삽입 블록 넣고 재실행).
2. Table 2/4를 Ihat으로 재추정 (HigherCCEI 계수).
3. 재매칭 placebo: $D_i$를 같은 교실·웨이브 다른 쌍의 $D_g$와 결합 → 기계적 계수 벤치마크.
4. fgarp(FOSD) 버전도 동일 패턴으로 `cross_fgarp` 확장 가능.
