# Cross-partition index e^x(S) / Ihat — README

노트의 cross-partition index를 기존 CCEI 파이프라인에 붙이기 위한 코드입니다.
기존 `01_calculate_ccei.R`은 **수정하지 않았습니다**. 아래 4개 파일만 추가했습니다.

## 파일 구조

| 파일 | 내용 |
|---|---|
| `ex_cross.R` | 핵심 함수. `cross_garp()`, `ex_cross()`, `compute_ex_from_subsets()`, `ihat_from_ex()` |
| `ex_cross_tests.R` | 검증 스위트 (T1–T7). 실행하면 전부 PASS. |
| `ex_integration_guide.R` | `01_calculate_ccei.R`에 `Ihat` 컬럼을 넣는 삽입 블록 + 위치 안내 |
| `clean_pair_raw.R` | 테스트가 원자료를 자립적으로 불러오도록 분리한 정제 함수 (01에서 추출) |

## 무엇을 계산하나

- `ex_cross(p, x, side)` = e^x(S) = sup{ e : 병합 데이터 D_Sg 에 cross e-violation 없음 }.
  `side`는 각 관측이 개인측("I")인지 그룹측("G")인지 라벨. 기존 CCEI 코드(`garp.R`,
  `warshall.R`, `ccei_garp.R`)를 재사용하고, 위반 판정을 "양측을 지나는 위반이 있는가"로만 바꿉니다.
- `ihat_from_ex(ex_h, ex_l, ex_hl)` = Ihat = 1/2 + (chat_h − chat_l)/(2·chat_hl),  chat(S)=1−e^x(S).
  불일치가 없으면(chat_hl≈0) NA.

## 실행 방법 (검증)

프로젝트 루트(= `01_calculate_ccei.R`가 있는 폴더)에서:

```
Rscript programs/ex_cross_tests.R
```

읽기 전용이며 파일을 만들지 않습니다. 필요 패키지: `haven`, `dplyr`, `igraph`.
`data/base_raw.dta`가 있으면 그걸 쓰고, 없으면 `data/riskpreference_pre.dta`에서 정제해 씁니다.

기대 출력: T1–T7 모두 `[PASS]`.
- T1 손계산 앵커(e^x=0.90), T2 세 알고리즘 판정 일치, T3 Warshall vs igraph 동일,
  T4 구조 항등식+Lemma 1, T5 e^x 고립 대조, T6 불변성, T7 exact-tie 회귀 가드.

핵심 정합성 근거는 **Lemma 1**: `ccei(D_Sg) = min( ccei(D_S), ccei(D_g), e^x(S) )`.
좌변은 기존 코드, 우변의 e^x만 새 코드 — 검증한 그룹(T4d/T5)에서 잔차 0으로 확인됨.

## 기존 데이터에 붙이는 방법 (Ihat 컬럼 생성)

`ex_integration_guide.R`에 상세히 있습니다. 요약:

1. `01_calculate_ccei.R` 상단 `source(...)` 묶음에 한 줄 추가:
   `source("programs/ex_cross.R")`
2. `compute_ccei_wave_measures()` 안에서 `I_hg`/`I_lg`를 만드는 블록(약 642–651행) **바로 뒤에**
   `ex_integration_guide.R`의 블록을 붙여넣기. 그 블록은 함수가 이미 만든 서브셋
   (`data_group`, `data_high`, `data_low`, `data_indiv`)을 재사용해 두 컬럼을 추가합니다:
   - `Ihat_hg_<suffix>`, `Ihat_lg_<suffix>`  (기존 `I_hg_*`/`I_lg_*`와 평행)
3. 실행하면 `panel_final`에 위 컬럼이 붙어 `data/panel_final.dta`로 저장됩니다.
   이후 회귀(Table 2/4)에서 `I_hg`를 `Ihat_hg`로 바꿔 재추정하면 됩니다.
   (원하면 `compute_fgarp_wave_measures`에도 같은 패턴으로 확장 가능.)

## 참고: 검증에서 고친 것 하나

적대적 재검증에서 `ihat_from_ex`의 미정의 가드가 이분탐색 해상도보다 지나치게 작아,
드문 예산 tie에서 Ihat이 NA 대신 큰 값을 낼 수 있는 결함을 찾아 수정했습니다.
**전체 1,524 group-waves 전수 스캔 결과 이 케이스는 0건이라, 실데이터의 e^x/Ihat 값은
수정 전후 동일합니다** (예방적 수정). 자세한 배경은 함께 보내는 `ex_cross_validation.md` 참고.
