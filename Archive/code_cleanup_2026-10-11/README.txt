Code cleanup, 2026-10-11

Retired 99_20_appendix_mover.do and 99_21_appendix_robustness.do from Code.
The current mover Table A3 and alternative-measure Table A5 are generated
directly by 08_Tables_Appendix.do; its calls to these scripts were removed.
The retired scripts are preserved here as they stood at removal.

Remaining active 99_* analysis groups were renumbered in numeric order:
6 -> 1; 10 -> 2; 11 -> 3; 12 -> 4; 13 -> 5; 17 -> 6;
24 -> 7; 25 -> 8; 26 -> 9. Companion scripts share a number.
Historical logs and saved hash records retain their original code names.

Moved loose logs into Code/Logs without altering their contents:
Code/11_2_collective_quality_linear.log -> Code/Logs/11_2_collective_quality_linear_batch.log
Code/11_collective_quality.log -> Code/Logs/11_collective_quality_batch.log
Code/99_25_within_wave_median.log -> Code/Logs/99_8_within_wave_median.log
Code/99_26_figure8_pair_fe.log -> Code/Logs/99_9_figure8_pair_fe.log
Code/99_26_figure8_pair_fe_linear.log -> Code/Logs/99_9_figure8_pair_fe_linear.log
Logs/08_alternative_full.log -> Code/Logs/08_alternative_full_project_root.log
Logs/09_export_adopted_specification.log -> Code/Logs/09_export_adopted_specification_project_root.log
Logs/99_23_new_indices.log -> Code/Logs/99_23_new_indices.log
