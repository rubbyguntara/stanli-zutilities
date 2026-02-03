class ZSAPLINK_TOOLS definition
  public
  create private .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ty_e071,
             obj_name TYPE e071-obj_name,
             trkorr   TYPE e071-trkorr,
             activity TYPE e071-activity,
             used     TYPE abap_bool.
    TYPES: END OF ty_e071 .
  types:
    ty_t_e071 TYPE STANDARD TABLE OF ty_e071 .
  types:
    BEGIN OF ty_e071k,
               trkorr     TYPE e071k-trkorr,
               activity   TYPE e071k-activity,
               mastername TYPE e071k-mastername,
               viewname   TYPE e071k-viewname,
               objname    TYPE e071k-objname,
               tabkey     TYPE e071k-tabkey.
    TYPES:   END OF ty_e071k .
  types:
    ty_t_e071k TYPE STANDARD TABLE OF ty_e071k .

  constants C_MODE_OBJECT type C value 'O' ##NO_TEXT.
  constants C_MODE_REQUEST type C value 'R' ##NO_TEXT.
  constants C_MODE_OTHER type C value '?' ##NO_TEXT.
  constants C_MEM_ID_SELOPS type CHAR30 value 'ZSAPLINK_FILTERS_SELOPS' ##NO_TEXT.
  constants C_MEM_ID_SELOP type CHAR30 value 'ZSAPLINK_FILTERS_SELOP_' ##NO_TEXT.
  constants C_BEHAVIOR_ASK type C value ' ' ##NO_TEXT.
  constants C_BEHAVIOR_EXPORT type C value 'Y' ##NO_TEXT.
  constants C_BEHAVIOR_DONT type C value 'N' ##NO_TEXT.

  class-methods FORGET_REQUEST_NUMBER .
  class-methods ADD_TO_REQUEST
    importing
      !IT_E071K type TREDT_KEYS optional
      !IS_KO200 type KO200
    returning
      value(RV_RESULT) type SY-SUBRC .
  class-methods INIT_PLUGIN
    importing
      !IV_PGMID type E071-PGMID default 'R3TR'
      !IV_OBJECT type E071-OBJECT default 'TABU'
      !IV_MASTERTYPE type E071-OBJECT optional
    exporting
      !ET_E071 type TY_T_E071
      !ET_E071K type TY_T_E071K
    changing
      !CV_MODE type C
    raising
      ZCX_SAPLINK .
  class-methods GET_RANDOM_STRING
    importing
      !NUMBER_CHARS type I
      !GENERATE_DIGITS type ABAP_BOOL default ABAP_TRUE
      !GENERATE_UPPERCASE type ABAP_BOOL default ABAP_TRUE
      !GENERATE_LOWERCASE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RANDOM_STRING) type STRING .
  class-methods EXPORT_SELOPS
    importing
      value(IV_REPID) type SY-REPID .
  class-methods GET_FILTERED_TABLE
    importing
      value(IV_CDAT) type DDOBJNAME optional
      value(IV_VDAT_TDAT) type DDOBJNAME optional
      value(IV_IS_TDAT) type XFELD optional
      value(IV_TABNAME) type DDOBJNAME
      value(IV_IS_LAST) type ABAP_BOOL optional
    exporting
      value(ET_RESULTS) type STANDARD TABLE
    raising
      ZCX_SAPLINK .
protected section.
private section.

  class-data GV_BEHAVIOR type C .
ENDCLASS.



CLASS ZSAPLINK_TOOLS IMPLEMENTATION.


method ADD_TO_REQUEST.

  DATA: ls_ko200   LIKE is_ko200,
        lt_e071k   LIKE it_e071k,
        lv_korrnum TYPE trkorr.

  ls_ko200 = is_ko200.
  lt_e071k[] = it_e071k[].

  CALL FUNCTION 'TR_OBJECT_CHECK'
    EXPORTING
      wi_ko200                = ls_ko200
    TABLES
      wt_e071k                = lt_e071k
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    rv_result = sy-subrc.
    EXIT.
  ENDIF.

  IMPORT trkorr TO lv_korrnum FROM MEMORY ID 'ZSAPLINK_TRKORR'.

  CALL FUNCTION 'TR_OBJECT_INSERT'
    EXPORTING
      wi_order                = lv_korrnum
      wi_ko200                = ls_ko200
      iv_no_standard_editor   = abap_true
      iv_no_show_option       = abap_true
    IMPORTING
      we_task                 = lv_korrnum
    TABLES
      wt_e071k                = lt_e071k
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2
      OTHERS                  = 3.

  rv_result = sy-subrc.

  IF rv_result = 0.
    EXPORT trkorr FROM lv_korrnum TO MEMORY ID 'ZSAPLINK_TRKORR'.
  ENDIF.

endmethod.


method EXPORT_SELOPS.

  DATA: lt_scr_info  TYPE STANDARD TABLE OF scr_info,
        lt_processed TYPE STANDARD TABLE OF string,
        lo_regex     TYPE REF TO cl_abap_regex,
        lo_matcher   TYPE REF TO cl_abap_matcher,
        lv_str       TYPE string,
        lv_str2      TYPE string,
        lt_selops    TYPE STANDARD TABLE OF dd03p-fieldname,
        lv_mem_id    TYPE c LENGTH 60,
        lv_noexp     TYPE abap_bool,
        lv_noshow    TYPE abap_bool,
        lv_notab     TYPE abap_bool.

  FIELD-SYMBOLS: <fs_scr_info> LIKE LINE OF lt_scr_info,
                 <fs_t_selop>  TYPE STANDARD TABLE,
                 <fs_param>    TYPE any,
                 <fs_selops>   LIKE LINE OF lt_selops.

* Get information about selection screen SELECT-OPTIONS
  CALL FUNCTION 'RS_SELSCREEN_INFO'
    EXPORTING
      report              = iv_repid
    TABLES
      field_info          = lt_scr_info
    EXCEPTIONS
      no_selections       = 1
      report_not_existent = 2
      subroutine_pool     = 3
      OTHERS              = 4.

  CHECK sy-subrc = 0.

* Leave SELECT-OPTIONS for table fields only
  DELETE lt_scr_info WHERE kind    =  'S' AND
                           dbfield IS INITIAL.

* Create a REGEX object for SELECT-OPTIONS name pattern matching
  CREATE OBJECT lo_regex
    EXPORTING
      pattern = `^SO_\d\d\d\d\d-(LOW)|(HIGH)$`.

* Get all SELECT-OPTIONS filled by the user
  LOOP AT lt_scr_info ASSIGNING <fs_scr_info>.

    IF <fs_scr_info>-kind = 'S'.

* Check if SELECT-OPTIONS is relevant
      lo_matcher = lo_regex->create_matcher( text = <fs_scr_info>-name ).
      CHECK lo_matcher->match( ) = abap_true.

* Get the name of SELECT-OPTIONS
      SPLIT <fs_scr_info>-name AT '-' INTO lv_str lv_str2.

* Check that we have not processed this SELECT-OPTIONS yet
      READ TABLE lt_processed TRANSPORTING NO FIELDS WITH KEY table_line = lv_str.
      CHECK sy-subrc <> 0.
      APPEND lv_str TO lt_processed.

* Get contents of SELECT-OPTIONS
      CONCATENATE `(` iv_repid ')' lv_str `[]` INTO lv_str.
      ASSIGN (lv_str) TO <fs_t_selop>.
      CHECK sy-subrc = 0 AND <fs_t_selop>[] IS NOT INITIAL.

* One more non-empty SELECT-OPTIONS
      APPEND INITIAL LINE TO lt_selops ASSIGNING <fs_selops>.
      SPLIT <fs_scr_info>-dbfield AT '-' INTO lv_str <fs_selops>.

* Export SELECT-OPTIONS to be used as a filter
      CONCATENATE c_mem_id_selop <fs_selops> INTO lv_mem_id.
      EXPORT selop FROM <fs_t_selop>[] TO MEMORY ID lv_mem_id.

    ELSEIF <fs_scr_info>-kind = 'P'.

      CONCATENATE `(` iv_repid ')' <fs_scr_info>-name INTO lv_str.
      ASSIGN (lv_str) TO <fs_param>.
      CHECK sy-subrc = 0.

      IF <fs_scr_info>-name = 'P_NOEXP'.
        lv_noexp = <fs_param>.
      ELSEIF <fs_scr_info>-name = 'P_NOSHOW'.
        lv_noshow = <fs_param>.
      ELSEIF <fs_scr_info>-name = 'P_NOTAB'.
        lv_notab = <fs_param>.
      ENDIF.

    ENDIF.

  ENDLOOP.

* Export the list of field names to build the SELECT-OPTIONS LOW and HIGH fields
  EXPORT noexp  FROM lv_noexp
         noshow FROM lv_noshow
         notab  FROM lv_notab
         selops FROM lt_selops[] TO MEMORY ID c_mem_id_selops.

endmethod.


method FORGET_REQUEST_NUMBER.

  DELETE FROM MEMORY ID 'ZSAPLINK_TRKORR'.

endmethod.


METHOD get_filtered_table.

  DATA: lr_t_selop         TYPE REF TO data,
        lv_str             TYPE string,
        lv_str2            TYPE string,
        lt_selops          TYPE STANDARD TABLE OF dd03p-fieldname,
        lv_mem_id          TYPE c LENGTH 60,
        lv_noexp           TYPE abap_bool,
        lv_noshow          TYPE abap_bool,
        lv_notab           TYPE abap_bool,
        ls_dd02v           TYPE dd02v,
        lt_dd03p           TYPE STANDARD TABLE OF dd03p,
        lt_prog            TYPE STANDARD TABLE OF text255,
        lt_prog2           TYPE STANDARD TABLE OF text255,
        lv_num_keyfields   TYPE i,
        lv_num_lines       TYPE i,
        lv_tabix           TYPE n LENGTH 5,
        lv_idx             TYPE c LENGTH 6,
        lv_progname        TYPE programm,
        lv_transfer_filter TYPE abap_bool,
        lv_skipped         TYPE abap_bool,
        lv_only_clnt       TYPE abap_bool,
        lt_textpool        TYPE table_of_textpool,
        lt_fields          TYPE cl_abap_structdescr=>component_table,
        lt_fields_base     TYPE cl_abap_structdescr=>component_table,
        lo_low_high        TYPE REF TO cl_abap_elemdescr,
        lo_selop_wa        TYPE REF TO cl_abap_structdescr,
        lo_selop           TYPE REF TO cl_abap_tabledescr.

  FIELD-SYMBOLS: <fs_dd03p>      LIKE LINE OF lt_dd03p,
                 <fs_textpool>   LIKE LINE OF lt_textpool,
                 <fs_selops>     LIKE LINE OF lt_selops,
                 <fs_fields>     LIKE LINE OF lt_fields,
                 <fs_t_selop>    TYPE STANDARD TABLE,
                 <fs_clnt_field> TYPE ANY TABLE.

* Initialize return parameter
  REFRESH et_results.

* Check if user still wants tables contents to be exported
  CHECK gv_behavior <> c_behavior_dont.

* Get information about table fields
  REFRESH lt_dd03p.
  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = iv_tabname
      langu         = sy-langu
    IMPORTING
      dd02v_wa      = ls_dd02v
    TABLES
      dd03p_tab     = lt_dd03p
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR
     ls_dd02v IS INITIAL OR
     lt_dd03p[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = `Table does not exist in the active state`.
  ENDIF.

* Get all table records and exit if user wants contents of all tables to be exported
  IF gv_behavior = c_behavior_export.
    SELECT *
      INTO TABLE et_results
      FROM (iv_tabname).
    EXIT.
  ENDIF.

* Check if database table contains at least one record
  SELECT *
    UP TO 1 ROWS
    INTO TABLE et_results
    FROM (iv_tabname).
  CHECK sy-subrc = 0.

* Initialize return parameter again
  REFRESH et_results.

* Remove all pseudo fields, such as .INCLUDEs and .APPENDs
  DELETE lt_dd03p WHERE fieldname(1) = '.'.

* Count number of key fields
  LOOP AT lt_dd03p TRANSPORTING NO FIELDS WHERE keyflag = abap_true.
    ADD 1 TO lv_num_keyfields.
  ENDLOOP.

* Build popup window title (program name)
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'R'.
  CONCATENATE 'Choose filters for table' iv_tabname INTO <fs_textpool>-entry
    SEPARATED BY space.

* Build name for parameter P_CDAT
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id     = 'S'.
  <fs_textpool>-key    = 'P_CDAT'.
  <fs_textpool>-entry  = '        CDAT object'.

* Build name for parameter P_XDAT
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id     = 'S'.
  <fs_textpool>-key    = 'P_XDAT'.
  IF iv_is_tdat = abap_true.
    <fs_textpool>-entry  = '        TDAT object'.
  ELSE.
    <fs_textpool>-entry  = '        VDAT object'.
  ENDIF.

* Build name for parameter P_TABU
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '001'.
  <fs_textpool>-entry = 'Table name'.

* Build name for parameter P_NAME
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '002'.
  <fs_textpool>-entry = ls_dd02v-ddtext.

* Build name for parameter P_NOEXP
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '003'.
  <fs_textpool>-entry = 'Don''t export any data from this table'.

* Build name for parameter P_NOSHOW
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '004'.
  <fs_textpool>-entry = 'Don''t show this filter screen anymore'.

* Build name for parameter P_YESTAB
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '005'.
  <fs_textpool>-entry = 'and export all records from all remaining tables'.

* Build name for parameter P_NOTAB
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = '006'.
  <fs_textpool>-entry = 'and ignore all remaining tables altogether'.

* Build block B02 frame title
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = 'B02'.
  <fs_textpool>-entry = 'Object hierarchy'.

* Build block B03 frame title
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = 'B03'.
  <fs_textpool>-entry = 'General selections'.

* Build block B04 frame title
  APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
  <fs_textpool>-id    = 'I'.
  <fs_textpool>-key   = 'B04'.
  <fs_textpool>-entry = 'Filters'.

* Generate a ramdom name for program
  lv_str = zsaplink_tools=>get_random_string( number_chars = 29 generate_lowercase = abap_false ).
  CONCATENATE `Y` lv_str INTO lv_progname.

* Build the filter program
  CONCATENATE `REPORT` lv_progname `.` INTO lv_str SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  APPEND `TYPE-POOLS: abap, icon.` TO lt_prog.
  APPEND `TABLES sscrfields.` TO lt_prog.
  CONCATENATE `TABLES` iv_tabname `.` INTO lv_str SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF SCREEN 2000 AS WINDOW.` TO lt_prog.
  APPEND `SELECTION-SCREEN FUNCTION KEY 2.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF BLOCK b01.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.` TO lt_prog.
  CONCATENATE `'` iv_cdat `'` INTO lv_str.
  CONCATENATE `  PARAMETER p_cdat TYPE DDOBJNAME DEFAULT` lv_str `MODIF ID pcd.` INTO lv_str
    SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  CONCATENATE `'` iv_vdat_tdat `'` INTO lv_str.
  CONCATENATE `  PARAMETER p_xdat TYPE DDOBJNAME DEFAULT` lv_str `MODIF ID pxd.` INTO lv_str
    SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (31) text-001 FOR FIELD p_tabu.` TO lt_prog.
  CONCATENATE `'` iv_tabname `'` INTO lv_str.
  CONCATENATE `  PARAMETER p_tabu TYPE DDOBJNAME DEFAULT` lv_str `MODIF ID pta.` INTO lv_str
    SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (60) text-002.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF BLOCK b02.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO lt_prog.
  APPEND `  PARAMETER p_noexp AS CHECKBOX USER-COMMAND noexp.` TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (50) text-003 FOR FIELD p_noexp.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO lt_prog.
  APPEND `  PARAMETER p_noshow AS CHECKBOX USER-COMMAND noshow MODIF ID sho.` TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (50) text-004 FOR FIELD p_noshow MODIF ID sho.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN POSITION 3.` TO lt_prog.
  APPEND `  PARAMETER p_yestab RADIOBUTTON GROUP g01 DEFAULT 'X' MODIF ID sh2.` TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (50) text-005 FOR FIELD p_yestab MODIF ID sh2.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN POSITION 3.` TO lt_prog.
  APPEND `  PARAMETER p_notab  RADIOBUTTON GROUP g01 MODIF ID sh2.` TO lt_prog.
  APPEND `SELECTION-SCREEN COMMENT (50) text-006 FOR FIELD p_notab MODIF ID sh2.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF LINE.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF BLOCK b03.` TO lt_prog.
  APPEND `SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.` TO lt_prog.

* Number of fixed lines for the popup window
  lv_idx = 1.
  lv_num_lines = 12.
  lv_skipped = abap_false.
  LOOP AT lt_dd03p ASSIGNING <fs_dd03p>.

    lv_tabix = sy-tabix.

    lv_transfer_filter = abap_true.

* For client-dependant tables, the client field is not shown as a filter
    AT FIRST.
      IF <fs_dd03p>-datatype = 'CLNT'.
        IF lv_num_keyfields = 1.
          lv_transfer_filter = abap_false.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDAT.

* Put one blank line separating key fields from non key fields
    IF <fs_dd03p>-keyflag = abap_false AND
       lv_skipped = abap_false.
      APPEND `  SELECTION-SCREEN SKIP 1.` TO lt_prog.
      lv_skipped = abap_true.
      ADD 1 TO lv_num_lines.
    ENDIF.

* Build name of each select-options
    CONCATENATE `SO_` lv_tabix INTO lv_str.

* Skip undesired data types
    CHECK <fs_dd03p>-datatype <> 'LCHR' AND
          <fs_dd03p>-datatype <> 'LRAW'.

* Build code to transfer each filter to the SELECTION-SCREEN of transaction SE16
    IF lv_transfer_filter = abap_true.
      CONCATENATE `    LOOP AT` lv_str `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      APPEND `      APPEND INITIAL LINE TO gt_seltab ASSIGNING <fs_seltab>.` TO lt_prog2.
      CONCATENATE `'I` lv_idx `'` INTO lv_str2.
      CONDENSE lv_str2 NO-GAPS.
      CONCATENATE `      <fs_seltab>-selname =` lv_str2 `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      APPEND `      <fs_seltab>-kind    = 'S'.` TO lt_prog2.
      CONCATENATE lv_str `-sign` INTO lv_str2.
      CONCATENATE `      <fs_seltab>-sign    =` lv_str2 `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      CONCATENATE lv_str `-option` INTO lv_str2.
      CONCATENATE `      <fs_seltab>-option  =` lv_str2 `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      CONCATENATE lv_str `-low` INTO lv_str2.
      CONCATENATE `      <fs_seltab>-low     =` lv_str2 `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      CONCATENATE lv_str `-high` INTO lv_str2.
      CONCATENATE `      <fs_seltab>-high    =` lv_str2 `.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      APPEND `    ENDLOOP.` TO lt_prog2.
      APPEND `    IF sy-subrc = 0.` TO lt_prog2.
      CONCATENATE `      READ TABLE` lv_str `INDEX 1.` INTO lv_str2 SEPARATED BY space.
      APPEND lv_str2 TO lt_prog2.
      APPEND `    ENDIF.` TO lt_prog2.
      ADD 1 TO lv_idx.
    ELSE.
      lv_only_clnt = abap_true.
    ENDIF.

    APPEND INITIAL LINE TO lt_textpool ASSIGNING <fs_textpool>.
    <fs_textpool>-id    = 'S'.
    <fs_textpool>-key   = lv_str.
    <fs_textpool>-entry = 'D'.

    CONCATENATE  iv_tabname `-` <fs_dd03p>-fieldname INTO lv_str2.

    CONCATENATE `  SELECT-OPTIONS` lv_str `FOR` lv_str2 `MODIF ID b04.` INTO lv_str
      SEPARATED BY space.
    APPEND lv_str TO lt_prog.

    ADD 1 TO lv_num_lines.

  ENDLOOP.

  APPEND `SELECTION-SCREEN END OF BLOCK b04.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF BLOCK b01.` TO lt_prog.
  APPEND `SELECTION-SCREEN END OF SCREEN 2000.` TO lt_prog.
  IF iv_is_last = abap_false.
    APPEND `CONSTANTS: gc_is_last TYPE abap_bool VALUE abap_false.` TO lt_prog.
  ELSE.
    APPEND `CONSTANTS: gc_is_last TYPE abap_bool VALUE abap_true.` TO lt_prog.
  ENDIF.
  APPEND `DATA: gv_button TYPE smp_dyntxt,` TO lt_prog.
  APPEND `      gt_seltab TYPE STANDARD TABLE OF rsparams.` TO lt_prog.
  APPEND `FIELD-SYMBOLS: <fs_seltab> LIKE LINE OF gt_seltab.` TO lt_prog.
  APPEND `INITIALIZATION.` TO lt_prog.

* Maximum line number
  IF lv_num_lines > 24.
    lv_num_lines = 24.
  ENDIF.

  lv_str = lv_num_lines.
  CONCATENATE `  CALL SELECTION-SCREEN '2000' STARTING AT 15 1 ENDING AT 132` lv_str `.`
    INTO lv_str SEPARATED BY space.
  APPEND lv_str TO lt_prog.

  APPEND `  IF sy-subrc = 0.` TO lt_prog.
  APPEND '    zsaplink_tools=>export_selops( sy-repid ).' TO lt_prog.
  APPEND `  ENDIF.` TO lt_prog.

  APPEND `AT SELECTION-SCREEN OUTPUT.` TO lt_prog.
  APPEND `  CHECK sy-dynnr = '2000'.` TO lt_prog.
  APPEND `  CLEAR gv_button.` TO lt_prog.
  APPEND `  IF p_noexp = abap_false.` TO lt_prog.
  APPEND `    gv_button-text      = 'SE16'.` TO lt_prog.
  APPEND `    gv_button-icon_id   = icon_inspection_method.` TO lt_prog.
  APPEND `    gv_button-icon_text = 'Browse filtered table records'.` TO lt_prog.
  APPEND `    gv_button-quickinfo = 'Browse table via transaction SE16 using specified filters'.` TO lt_prog.
  APPEND `  ENDIF.` TO lt_prog.
  APPEND `  sscrfields-functxt_02 = gv_button.` TO lt_prog.
  APPEND `  LOOP AT SCREEN.` TO lt_prog.
  APPEND `    CASE screen-group1.` TO lt_prog.
  APPEND `      WHEN 'B04'.` TO lt_prog.
  APPEND `        IF p_noexp = abap_true.` TO lt_prog.
  APPEND `          screen-active = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-active = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `      WHEN 'PCD'.` TO lt_prog.
  APPEND `        screen-input = '0'.` TO lt_prog.
  APPEND `        IF p_cdat IS INITIAL.` TO lt_prog.
  APPEND `          screen-active = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-active = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `      WHEN 'PXD'.` TO lt_prog.
  APPEND `        screen-input = '0'.` TO lt_prog.
  APPEND `        IF p_xdat IS INITIAL.` TO lt_prog.
  APPEND `          screen-active = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-active = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `      WHEN 'PTA'.` TO lt_prog.
  APPEND `        screen-input = '0'.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `      WHEN 'SHO'.` TO lt_prog.
  APPEND `        IF gc_is_last = abap_true.` TO lt_prog.
  APPEND `          screen-active = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-active = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `      WHEN 'SH2'.` TO lt_prog.
  APPEND `        IF gc_is_last = abap_true.` TO lt_prog.
  APPEND `          screen-active = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-active = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        IF p_noshow IS INITIAL.` TO lt_prog.
  APPEND `          screen-input = '0'.` TO lt_prog.
  APPEND `        ELSE.` TO lt_prog.
  APPEND `          screen-input = '1'.` TO lt_prog.
  APPEND `        ENDIF.` TO lt_prog.
  APPEND `        MODIFY SCREEN.` TO lt_prog.
  APPEND `    ENDCASE.` TO lt_prog.
  APPEND `  ENDLOOP.` TO lt_prog.
  APPEND `AT SELECTION-SCREEN.` TO lt_prog.
  APPEND `  CHECK sy-dynnr = '2000'.` TO lt_prog.
  APPEND `  IF sscrfields-ucomm = 'FC02'.` TO lt_prog.
  IF lv_only_clnt = abap_true.
    APPEND `    IF SO_00001[] IS NOT INITIAL.` TO lt_prog.
    CONCATENATE ```` `Table Contents Browser ignores client field filters when it is the only table key field` ```` INTO lv_str.
    CONCATENATE `      MESSAGE` lv_str `TYPE if_xo_const_message=>info.` INTO lv_str
      SEPARATED BY space.
    APPEND lv_str TO lt_prog.
    APPEND `    ENDIF.` TO lt_prog.
  ENDIF.
  APPEND `    REFRESH gt_seltab.` TO lt_prog.
  APPEND LINES OF lt_prog2 TO lt_prog.
  APPEND `    IF gt_seltab[] IS INITIAL.` TO lt_prog.
  APPEND `      APPEND INITIAL LINE TO gt_seltab ASSIGNING <fs_seltab>.` TO lt_prog.
  APPEND `      <fs_seltab>-selname = 'I1'.` TO lt_prog.
  APPEND `      <fs_seltab>-kind    = 'S'.` TO lt_prog.
  APPEND `      <fs_seltab>-sign    = 'I'.` TO lt_prog.
  APPEND `      <fs_seltab>-option  = 'CP'.` TO lt_prog.
  APPEND `      <fs_seltab>-low     = '*'.` TO lt_prog.
  APPEND `    ENDIF.` TO lt_prog.
  APPEND `    CALL FUNCTION 'RS_TABLE_LIST_CREATE'` TO lt_prog.
  APPEND `      EXPORTING` TO lt_prog.
  CONCATENATE `'` iv_tabname `'` INTO lv_str.
  CONCATENATE `        TABLE_NAME =` lv_str INTO lv_str SEPARATED BY space.
  APPEND lv_str TO lt_prog.
  APPEND `      TABLES` TO lt_prog.
  APPEND `        SELTAB = gt_seltab.` TO lt_prog.
  APPEND `  ENDIF.` TO lt_prog.

* Create temporary program in database
  INSERT REPORT lv_progname
    FROM lt_prog
    PROGRAM TYPE '1'
    UNICODE ENABLING abap_true.
  CHECK sy-subrc = 0.

* Insert program text elements
  INSERT TEXTPOOL lv_progname
    FROM lt_textpool
    LANGUAGE sy-langu.

* Activate the dynamically generated program
  GENERATE REPORT lv_progname.

* Execute temporary program to ask user for the filters
  SUBMIT (lv_progname) AND RETURN.

* And delete the temporary program aftwards
  DELETE REPORT lv_progname.

* Get filter names
  CLEAR: lv_noexp, lv_noshow, lv_notab.
  REFRESH lt_selops.
  IMPORT noexp  TO lv_noexp
         noshow TO lv_noshow
         notab  TO lv_notab
         selops TO lt_selops[] FROM MEMORY ID c_mem_id_selops.
  CHECK sy-subrc = 0.

* Delete imported buffer
  DELETE FROM MEMORY ID c_mem_id_selops.

* Save user choice about seeing this filter dialog for next tables
  IF lv_noshow = abap_true.
    IF lv_notab = abap_false.
      gv_behavior = c_behavior_export.
    ELSE.
      gv_behavior = c_behavior_dont.
    ENDIF.
  ENDIF.

* Check if user wants to export the contents of this table
  CHECK lv_noexp = abap_false.

* Get all table records
  SELECT *
    INTO TABLE et_results
    FROM (iv_tabname).

* Check if user informed any filters
  CHECK lt_selops[] IS NOT INITIAL.

* Build the SIGN field of the RANGE
  APPEND INITIAL LINE TO lt_fields_base ASSIGNING <fs_fields>.
  <fs_fields>-name = 'SIGN'.
  <fs_fields>-type = cl_abap_elemdescr=>get_c( 1 ).

* Build the OPTION field of the RANGE
  APPEND INITIAL LINE TO lt_fields_base ASSIGNING <fs_fields>.
  <fs_fields>-name = 'OPTION'.
  <fs_fields>-type = cl_abap_elemdescr=>get_c( 2 ).

* Get all filters and do the filtering
  LOOP AT lt_selops ASSIGNING <fs_selops>.

* Get the SIGN and OPTION table field types of the RANGE
    lt_fields[] = lt_fields_base[].

* Name of the LOW/HIGH fields of the RANGE
    CONCATENATE iv_tabname `-` <fs_selops> INTO lv_str.

* Create the type for the LOW and HIGH fields of the RANGE
    lo_low_high ?= cl_abap_elemdescr=>describe_by_name( lv_str ).

* Build the type for the LOW field of the RANGE
    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name = 'LOW'.
    <fs_fields>-type = lo_low_high.

* Build the type for the HIGH field of the RANGE
    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name = 'HIGH'.
    <fs_fields>-type = lo_low_high.

* Create a work area type for the RANGE table
    lo_selop_wa = cl_abap_structdescr=>create( lt_fields ).

* Create the RANGE table type
    lo_selop = cl_abap_tabledescr=>create( lo_selop_wa ).

* And finally create the RANGE table itself
    CREATE DATA lr_t_selop TYPE HANDLE lo_selop.
    ASSIGN lr_t_selop->* TO <fs_t_selop>.

* Import the filter to be used
    CONCATENATE c_mem_id_selop <fs_selops> INTO lv_mem_id.
    IMPORT selop TO <fs_t_selop>[] FROM MEMORY ID lv_mem_id.
    CHECK sy-subrc = 0.

* Delete imported buffer
    DELETE FROM MEMORY ID lv_mem_id.

* Delete all unwanted entries
    CONCATENATE <fs_selops> `NOT IN <FS_T_SELOP>`
      INTO lv_str SEPARATED BY space.
    DELETE et_results WHERE (lv_str).

  ENDLOOP.

* Get data from table
ENDMETHOD.


method GET_RANDOM_STRING.

* ASCII codes
*0-9: 48-57
*A-Z: 65-90
*a-z: 97-122

  TYPE-POOLS: abap.

  DATA: lv_types   TYPE string,
        lv_seed    TYPE i,
        lo_rndtype TYPE REF TO cl_abap_random_int,
        lo_rndnumb TYPE REF TO cl_abap_random_int,
        lo_rnduppe TYPE REF TO cl_abap_random_int,
        lo_rndlowe TYPE REF TO cl_abap_random_int,
        lo_conv    TYPE REF TO cl_abap_conv_in_ce,
        lv_len     TYPE i,
        lv_type    TYPE i,
        lv_asc     TYPE x,
        lv_ascstr  TYPE xstring.

  CLEAR random_string.

* Random generator for digits
  IF generate_digits = abap_true.
    CONCATENATE lv_types '1' INTO lv_types.
    lv_seed = cl_abap_random=>seed( ).
    lo_rndnumb = cl_abap_random_int=>create( seed = lv_seed min = 48 max = 57 ).
  ENDIF.

* Random generator for uppercase letters
  IF generate_uppercase = abap_true.
    CONCATENATE lv_types '2' INTO lv_types.
    lv_seed = cl_abap_random=>seed( ).
    lo_rnduppe = cl_abap_random_int=>create( seed = lv_seed min = 65 max = 90 ).
  ENDIF.

* Random generator for lowercase letters
  IF generate_lowercase = abap_true.
    CONCATENATE lv_types '3' INTO lv_types.
    lv_seed = cl_abap_random=>seed( ).
    lo_rndlowe = cl_abap_random_int=>create( seed = lv_seed min = 97 max = 122 ).
  ENDIF.

  CHECK lv_types IS NOT INITIAL.

* Random generator for the specified character types (digit, uppercase or lowercase)
  lv_len = strlen( lv_types ) - 1.
  lv_seed = cl_abap_random=>seed( ).
  lo_rndtype = cl_abap_random_int=>create( seed = lv_seed min = 0 max = lv_len ).

  DO number_chars TIMES.
    lv_type = lo_rndtype->get_next( ).
    CASE lv_types+lv_type(1).
      WHEN '1'.
        lv_asc = lo_rndnumb->get_next( ).
      WHEN '2'.
        lv_asc = lo_rnduppe->get_next( ).
      WHEN '3'.
        lv_asc = lo_rndlowe->get_next( ).
    ENDCASE.
    CONCATENATE lv_ascstr lv_asc INTO lv_ascstr IN BYTE MODE.
  ENDDO.

  lo_conv = cl_abap_conv_in_ce=>create( input = lv_ascstr ).
  lo_conv->read( IMPORTING data = random_string ).

endmethod.


method INIT_PLUGIN.

*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/


* Plugin developed by Lino Lopes (lino@hr.com.br)

  TYPES: ty_r_trkorr TYPE RANGE OF e071-trkorr.

  TYPES: BEGIN OF ty_trfunction,
           trkorr     TYPE e070-trkorr,
           trfunction TYPE e070-trfunction.
  TYPES: END OF ty_trfunction.

  DATA: lv_str        TYPE string,
        lt_trkorr     TYPE STANDARD TABLE OF e070-trkorr,
        lt_trfunction TYPE STANDARD TABLE OF ty_trfunction.

  FIELD-SYMBOLS: <fs_nugr>       TYPE c,
                 <fs_nuga>       TYPE c,
                 <fs_r_reqnugg>  TYPE ty_r_trkorr,
                 <fs_trfunction> LIKE LINE OF lt_trfunction,
                 <fs_trkorr>     LIKE LINE OF lt_trkorr.

* Only processes first time
  CHECK cv_mode IS INITIAL.

* Get the "Add Objects to Nugget" flag from the calling program's selection screen
  CONCATENATE '(' sy-cprog ')NUGA' INTO lv_str.
  ASSIGN (lv_str) TO <fs_nuga>.
  IF sy-subrc <> 0.
    lv_str = `"Add Objects to Nugget" flag not found`.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_str.
  ENDIF.

* Get the "Add Objects from a Transport" flag from the calling program's selection screen
  CONCATENATE '(' sy-cprog ')NUGR' INTO lv_str.
  ASSIGN (lv_str) TO <fs_nugr>.
  IF sy-subrc <> 0.
    lv_str = `"Add Objects from a Transport" flag not found`.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_str.
  ENDIF.

  CLEAR cv_mode.
  REFRESH: et_e071, et_e071k.

  IF <fs_nuga> = abap_true.
* Add from object mode
    cv_mode = c_mode_object.
  ELSEIF <fs_nugr> = abap_true.

* Add from request mode
    cv_mode = c_mode_request.

    IF iv_mastertype IS SUPPLIED AND
       iv_mastertype IS NOT INITIAL AND
       ( et_e071[] IS SUPPLIED OR et_e071k[] IS SUPPLIED ).

* Get the request number from selection screen
      CONCATENATE '(' sy-cprog ')REQNUGG[]' INTO lv_str.
      ASSIGN (lv_str) TO <fs_r_reqnugg>.
      IF sy-subrc <> 0.
        lv_str = `List of requests not found`.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_str.
      ENDIF.

* At least one request must be supplied
      IF <fs_r_reqnugg>[] IS INITIAL.
        lv_str = `No requests supplied` .
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_str.
      ENDIF.

* Get type of all requests supplied
      SELECT  trkorr trfunction
        INTO  TABLE lt_trfunction
        FROM  e070
        WHERE trkorr IN <fs_r_reqnugg>.

* Loop thru all requests supplied, looking for their tasks
      LOOP AT lt_trfunction ASSIGNING <fs_trfunction>.

* If it's a task, collect it
        IF <fs_trfunction>-trfunction CO 'SRQ'. "Development/correction, Repair or Customizing

          APPEND INITIAL LINE TO lt_trkorr ASSIGNING <fs_trkorr>.
          <fs_trkorr> = <fs_trfunction>-trkorr.

* If it's a request, search for its tasks
        ELSEIF <fs_trfunction>-trfunction CO 'KW'. " Workbench or Customizing

* Get all tasks of the request
          SELECT  trkorr
            APPENDING TABLE lt_trkorr
            FROM  e070
            WHERE strkorr = <fs_trfunction>-trkorr.

* Otherwise it's an invalid request type
        ELSE.

          lv_str = `Unknown request type`.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_str.

        ENDIF.

      ENDLOOP.

      IF lt_trkorr[] IS NOT INITIAL.

        IF et_e071[] IS SUPPLIED.

* Load all R3TR VDAT objects and their keys for all tasks of the request
          SELECT  obj_name trkorr activity
            INTO  TABLE et_e071
            FROM  e071
            FOR ALL ENTRIES IN lt_trkorr
            WHERE trkorr = lt_trkorr-table_line AND
                  pgmid  = iv_pgmid AND
                  object = iv_mastertype.

          IF sy-subrc <> 0.
            lv_str = `Request is empty`.
            RAISE EXCEPTION TYPE zcx_saplink
              EXPORTING
                textid = zcx_saplink=>error_message
                msg    = lv_str.
          ENDIF.

          SORT et_e071 BY obj_name used trkorr activity.

        ENDIF.

        IF et_e071k[] IS SUPPLIED.

* Get all object keys from all tasks
          SELECT  trkorr activity mastername viewname objname tabkey
            INTO  TABLE et_e071k
            FROM  e071k
            FOR ALL ENTRIES IN lt_trkorr
            WHERE trkorr = lt_trkorr-table_line AND
                  pgmid      = iv_pgmid AND
                  object     = iv_object AND
                  mastertype = iv_mastertype.

          SORT et_e071k BY table_line.
          DELETE ADJACENT DUPLICATES FROM et_e071k
            COMPARING table_line.

        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.
* Add from other sources
    cv_mode = c_mode_other.
  ENDIF.

endmethod.
ENDCLASS.
