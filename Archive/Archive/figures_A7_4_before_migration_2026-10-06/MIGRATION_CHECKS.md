# Figure migration checks, 2026-10-06

The original affected sources and manuscript A6/A7 images are preserved with
SHA256.json. The current manuscript images and text were not updated.

- Main 07 completed in an isolated Code/Overleaf output folder; its Figure 4
  image matched the selected-section preview byte for byte.
- Appendix 09 completed using Code/data. Older blocks now use the canonical
  higher-CCEI flag; the legacy paired RA test excludes ties because ties lack
  a higher/lower ranking. Missing legacy CSV/pattern dependencies are reported
  as skips, while the current Figure A7 is generated from 08's canonical CSV.
- Figure A7 used Table 3 Column (3), N=2,512 and 64 classes, with exact shares,
  M, individual effects, and no risk-aversion controls. Overall R-squared and
  within R-squared matched; both panel shares sum to 100. The exact four-block
  allocation matched shapley2 within 1e-7; that command's internal svmat export
  stores coalition statistics as floats. The new allocation retains doubles.
- Figure 4 checked all 651 non-own donors per target, matched their averages
  to the adopted M within 1e-10, excluded self-matches, shared one donor across
  both members, and retained the same 2,560 plotted student-waves as Figure 3.
  Seed 20260812 was fixed before reading the resulting descriptive gap.
- The review exporter's Figure 3 loop was tested with stale review images and
  confirmed to copy the canonical images produced by 07.
- Only requested figure previews and migrated current Figure 3/A6 outputs
  were retained; unrelated generated image changes from testing were restored.
- No full benchmark, index calculation, or 500-partition simulation ran.

Current generation: Figure A7 section of 08_Tables_Appendix.do, then
09_Figures_Appendix.R; Figure 4 and current Figure 3 are in 07_Figures_Main.R.
The former review plotter 11 was archived; its Figure A6 code is in 09.
