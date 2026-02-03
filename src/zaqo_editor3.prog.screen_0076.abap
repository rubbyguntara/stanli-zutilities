
PROCESS BEFORE OUTPUT.
  MODULE pbo_076.

PROCESS AFTER INPUT.
  CHAIN.
    FIELD: zsaqo3_general_info-prev_value_cnt,
           zsaqo3_general_info-menu_mode,
           zsaqo3_general_info-description.
    MODULE pai_076.
  ENDCHAIN.
