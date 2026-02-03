
PROCESS BEFORE OUTPUT.
  MODULE pbo_070.
  CALL SUBSCREEN tabs_area
    INCLUDING g_tabs-prog g_tabs-subscreen.

PROCESS AFTER INPUT.
  MODULE pai_exit AT EXIT-COMMAND.
  CALL SUBSCREEN tabs_area.
  MODULE pai_070.

PROCESS ON VALUE-REQUEST.
  FIELD zsaqo3_general_info-package_id MODULE f4_free_search.
