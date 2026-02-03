*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTA_BCDT001.....................................*
DATA:  BEGIN OF STATUS_ZTA_BCDT001                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTA_BCDT001                   .
CONTROLS: TCTRL_ZTA_BCDT001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTA_BCDT001                   .
TABLES: ZTA_BCDT001                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
