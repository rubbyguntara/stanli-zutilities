*&---------------------------------------------------------------------*
*& Report ZRE_AB001_INTERFACE_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zre_ab001_interface_monitor.

TYPE-POOLS: icon, slis, vrm.

TABLES : sscrfields.

TYPES : BEGIN OF ty_report.
TYPES :  excep       TYPE char5.
         INCLUDE TYPE zta_bcdt002.
         TYPES : description TYPE text100,
         butreq      TYPE char16,
         butres      TYPE char16,
         butdet      TYPE char16.
TYPES : END OF ty_report.

DATA : gi_bcdt001   TYPE STANDARD TABLE OF zta_bcdt001,
       gw_bcdt001   TYPE zta_bcdt001,
       gi_report    TYPE TABLE OF ty_report,
       gi_reportsel TYPE TABLE OF ty_report,
       gw_report    TYPE ty_report.

CLASS lcl_eventhandler DEFINITION DEFERRED.

DATA : log   TYPE REF TO zcl_logger,
       runid TYPE char50,
       extnr TYPE char50,
       dummy.

DATA : gv_okcode TYPE sy-ucomm,
       gv_repid  TYPE sy-repid.

"Listbox
DATA : gv_name TYPE vrm_id,
       gi_list TYPE vrm_values,
       gw_list LIKE LINE OF gi_list.

DATA : gc_handler TYPE REF TO lcl_eventhandler.

DATA : gi_fieldcat TYPE lvc_t_fcat, "Fieldcatalog
       gw_fieldcat TYPE lvc_s_fcat. "Fieldcatalog

DATA : gw_layout           TYPE          lvc_s_layo, "Layout
       gw_variant          TYPE          disvariant,
       gc_custom_container TYPE REF TO   cl_gui_custom_container.

DATA : gc_log   TYPE REF TO zcl_logger,
       gv_extnr TYPE char50.

DATA : gv_container TYPE REF TO cl_gui_custom_container,
       gv_html      TYPE REF TO cl_gui_html_viewer,
       gv_ucomm     TYPE sy-ucomm.

DATA : g_grid_i TYPE REF TO cl_gui_alv_grid.

CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.
    METHODS:

      button_click         FOR EVENT button_click
                  OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no.


ENDCLASS.                    "event_responder DEFINITION

CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD button_click.
    DATA : lv_xstring TYPE xstring,
           lv_len     TYPE i,
           li_content TYPE soli_tab,
           lv_string  TYPE string.

    CASE es_col_id-fieldname.
      WHEN 'BUTDET'.
        READ TABLE gi_report INTO gw_report INDEX es_row_no-row_id.
        IF sy-subrc EQ 0.
          PERFORM fm_display_log USING gw_report-if_code gw_report-extnr gw_report-balnr.
        ENDIF.
      WHEN 'BUTREQ'.
        READ TABLE gi_report INTO gw_report INDEX es_row_no-row_id.
        IF sy-subrc EQ 0.
          REFRESH : li_content.

          IF gw_report-req_payload IS INITIAL.
            MESSAGE i000(00) WITH 'No payload data available.'.
            RETURN.
          ENDIF.

          TRY.
              CALL TRANSFORMATION sjson2html
                SOURCE XML gw_report-req_payload
                RESULT XML DATA(lvc_html).

              cl_abap_browser=>show_html(
                title = 'JSON Request Log'
                html_string = cl_abap_codepage=>convert_from( lvc_html ) ).

            CATCH cx_transformation_error INTO DATA(lo_error).
              MESSAGE i000(00) WITH 'JSON transformation error:' lo_error->get_text( ).
          ENDTRY.

        ENDIF.
      WHEN 'BUTRES'.
        READ TABLE gi_report INTO gw_report INDEX es_row_no-row_id.
        IF sy-subrc EQ 0.
          REFRESH : li_content.

          IF gw_report-res_payload IS INITIAL.
            MESSAGE i000(00) WITH 'No payload data available.'.
            RETURN.
          ENDIF.

          TRY.
              CALL TRANSFORMATION sjson2html
                SOURCE XML gw_report-res_payload
                RESULT XML lvc_html.

              cl_abap_browser=>show_html(
                title = 'JSON Response Log'
                html_string = cl_abap_codepage=>convert_from( lvc_html ) ).

            CATCH cx_transformation_error INTO lo_error.
              MESSAGE i000(00) WITH 'JSON transformation error:' lo_error->get_text( ).
          ENDTRY.

        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.                    "event_responder IMPLEMENTATION

DATA : gw_bcdt002 TYPE zta_bcdt002.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS : so_budat FOR gw_bcdt002-erdat,
                 so_ifcod  FOR gw_bcdt002-if_code NO INTERVALS.
PARAMETERS     : pa_categ TYPE paocf_int_type OBLIGATORY AS LISTBOX VISIBLE LENGTH 15 DEFAULT 'I',
                 pa_stat  TYPE bapi_mtype OBLIGATORY AS LISTBOX VISIBLE LENGTH 10 DEFAULT 'E'.
SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_listbox_categ.
  PERFORM fm_listbox_status.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ifcod-low.
  PERFORM fm_f4_if_code.

START-OF-SELECTION.
  PERFORM fm_get_data.

END-OF-SELECTION.
  PERFORM fm_display_data.

*&---------------------------------------------------------------------*
*& Form fm_f4_if_code
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_f4_if_code .

  TYPES : BEGIN OF ty_bcdt001,
            if_group    TYPE zde_if_group,
            if_code     TYPE zde_if_code,
            description TYPE text100,
            direction   TYPE zde_if_direct,
            if_type     TYPE zde_if_type,
          END OF ty_bcdt001.

  DATA : li_bcdt001 TYPE STANDARD TABLE OF ty_bcdt001,
         lw_bcdt001 TYPE ty_bcdt001.

  REFRESH li_bcdt001.
  SELECT if_group if_code description direction if_type
    FROM zta_bcdt001
    INTO TABLE li_bcdt001
    WHERE is_active EQ abap_true.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      retfield        = 'IF_CODE'
      window_title    = 'Interface Code'
      dynprofield     = 'SO_IFCOD-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = li_bcdt001
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_listbox_categ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_listbox_categ .
  REFRESH : gi_list.

  gv_name = 'PA_CATEG'.

  gw_list-key  = 'O'.
  gw_list-text = 'OUTBOUND'.
  APPEND gw_list TO gi_list.

  gw_list-key  = 'I'.
  gw_list-text = 'INBOUND'.
  APPEND gw_list TO gi_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gv_name
      values = gi_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_listbox_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_listbox_status .
  REFRESH : gi_list.

  gv_name = 'PA_STAT'.

  gw_list-key  = 'E'.
  gw_list-text = 'Error'.
  APPEND gw_list TO gi_list.

  gw_list-key  = 'S'.
  gw_list-text = 'Success'.
  APPEND gw_list TO gi_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gv_name
      values = gi_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_get_Data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_get_data .

  SELECT * FROM zta_bcdt001 INTO TABLE gi_bcdt001
    WHERE if_code IN so_ifcod
      AND direction EQ pa_categ.

  SELECT * FROM zta_bcdt002 INTO CORRESPONDING FIELDS OF TABLE gi_report
                                WHERE direction = pa_categ
                                  AND erdat IN so_budat
                                  AND if_code IN so_ifcod
                                  AND status EQ pa_stat.

  LOOP AT gi_report ASSIGNING FIELD-SYMBOL(<fs_report>).
    <fs_report>-excep   = SWITCH #( <fs_report>-status
                                    WHEN 'E' THEN icon_red_light
                                    ELSE icon_green_light ).

    <fs_report>-description = VALUE #( gi_bcdt001[ if_code = <fs_report>-if_code ]-description OPTIONAL ).
    <fs_report>-if_type     = COND #( WHEN <fs_report>-if_type IS INITIAL
                                        THEN VALUE #( gi_bcdt001[ if_code = <fs_report>-if_code ]-if_type OPTIONAL )
                                        ELSE <fs_report>-if_type ).
    <fs_report>-butdet      = |{ icon_dangerous_good_check } Log Detail|.
    <fs_report>-butreq      = |{ icon_task } Request Payload|.
    <fs_report>-butres      = |{ icon_task } Response Payload|.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_display_data .

  IF gi_report[] IS NOT INITIAL.
    PERFORM fm_list.
  ELSE.
    MESSAGE i597(c0).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form fm_build_fieldcatalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_build_fieldcatalog .

  REFRESH gi_fieldcat.

  DATA: lc_tabledescr_ref TYPE REF TO cl_abap_tabledescr,
        lc_descr_ref      TYPE REF TO cl_abap_structdescr,
        lw_table          TYPE abap_compdescr,
        lw_fcat           TYPE lvc_s_fcat.

  lc_tabledescr_ref ?= cl_abap_typedescr=>describe_by_data( gi_report ).
  lc_descr_ref ?= lc_tabledescr_ref->get_table_line_type( ).

  LOOP AT lc_descr_ref->components INTO lw_table .
    CLEAR : lw_fcat.

    CASE lw_table-name.
      WHEN 'EXCEP'.
        lw_fcat-fieldname = 'EXCEP'.
        lw_fcat-coltext   = 'Stats'.
        lw_fcat-icon      = abap_true.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'BUTDET'.
        lw_fcat-fieldname = 'BUTDET'.
        lw_fcat-style     = cl_gui_alv_grid=>mc_style_button.
        lw_fcat-coltext   = 'Log Detail'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'BUTREQ'.
        lw_fcat-fieldname = 'BUTREQ'.
        lw_fcat-style     = cl_gui_alv_grid=>mc_style_button.
        lw_fcat-coltext   = 'Request Payload'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'BUTRES'.
        lw_fcat-fieldname = 'BUTRES'.
        lw_fcat-style     = cl_gui_alv_grid=>mc_style_button.
        lw_fcat-coltext   = 'Response Payload'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'REQ_PAYLOAD'.
        lw_fcat-fieldname = 'REQ_PAYLOAD'.
        lw_fcat-coltext   = 'Request Payload'.
        lw_fcat-hotspot   = 'X'.
        lw_fcat-tech      = 'X'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'RES_PAYLOAD'.
        lw_fcat-fieldname = 'RES_PAYLOAD'.
        lw_fcat-coltext   = 'Response Payload'.
        lw_fcat-hotspot   = 'X'.
        lw_fcat-tech      = 'X'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'LOG_UUID'.
        lw_fcat-fieldname = 'LOG_UUID'.
        lw_fcat-coltext   = 'UUID'.
        lw_fcat-tech      = ''.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'IF_GROUP'.
        lw_fcat-fieldname = 'IF_GROUP'.
        lw_fcat-coltext   = 'Interface Group'.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'IF_CODE'.
        lw_fcat-fieldname = 'IF_CODE'.
        lw_fcat-coltext   = 'Interface Code'.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'DESCRIPTION'.
        lw_fcat-fieldname = 'DESCRIPTION'.
        lw_fcat-coltext   = 'Interface Desc'.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'DIRECTION'.
        lw_fcat-fieldname = 'DIRECTION'.
        lw_fcat-coltext   = 'Direction'.
        lw_fcat-key       = abap_true.
        lw_fcat-convexit  = 'DIREC'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'IF_TYPE'.
        lw_fcat-fieldname = 'IF_TYPE'.
        lw_fcat-coltext   = 'Interface Type'.
        lw_fcat-key       = abap_true.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'PATH'.
        lw_fcat-fieldname = 'PATH'.
        lw_fcat-coltext   = 'Path'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'EXTNR'.
        lw_fcat-fieldname = 'EXTNR'.
        lw_fcat-coltext   = 'External No'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'RETRY_NO'.
        lw_fcat-fieldname = 'RETRY_NO'.
        lw_fcat-coltext   = 'Retries No'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'FILENAME'.
        lw_fcat-fieldname = 'FILENAME'.
        lw_fcat-coltext   = 'Filename'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'ERNAM'.
        lw_fcat-fieldname = 'ERNAM'.
        lw_fcat-coltext   = 'Created by'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'ERDAT'.
        lw_fcat-fieldname = 'ERDAT'.
        lw_fcat-coltext   = 'Created Date'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'ERZET'.
        lw_fcat-fieldname = 'ERZET'.
        lw_fcat-coltext   = 'Created at'.
        APPEND lw_fcat TO gi_fieldcat.

      WHEN 'MESSAGE'.
        lw_fcat-fieldname = 'MESSAGE'.
        lw_fcat-coltext   = 'Message'.
        APPEND lw_fcat TO gi_fieldcat.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_build_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_build_layout .
  gw_layout-col_opt         = 'X'.
  gw_layout-cwidth_opt      = 'X'.
  gw_layout-zebra           = 'X'.
  gw_layout-sel_mode        = 'A'.
  gw_layout-no_toolbar      = ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_display_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_display_log USING fu_ifcod TYPE zde_if_code
                          fu_extnr TYPE char50
                          fu_balnr TYPE balognr.

  DATA: lt_log_header   TYPE balhdr_t WITH HEADER LINE,
        ls_fil_objnr    TYPE bal_s_obj,
        ls_fil_subobjnr TYPE bal_s_sub,
        ls_fil_extnr    TYPE bal_s_extn,
        ls_fil_lognr    TYPE bal_s_logn,
        ls_log_filter   TYPE bal_s_lfil,
        lt_log_handle   TYPE bal_t_logh.

  DATA : lw_prof  TYPE  bal_s_prof.

  gw_bcdt001 = VALUE #( gi_bcdt001[ if_code = fu_ifcod ] OPTIONAL ).

  CONCATENATE 'IEQ' gw_bcdt001-balobj INTO ls_fil_objnr.
  APPEND ls_fil_objnr TO ls_log_filter-object.

  CONCATENATE 'IEQ' gw_bcdt001-balsubobj INTO ls_fil_subobjnr.
  APPEND ls_fil_subobjnr TO ls_log_filter-subobject.

  IF fu_balnr IS NOT INITIAL.
    ls_fil_lognr-sign     = 'I'.
    ls_fil_lognr-option   = 'EQ'.
    ls_fil_lognr-low      = fu_balnr.
    APPEND ls_fil_lognr TO ls_log_filter-lognumber.
  ELSE.
    ls_fil_extnr-sign     = 'I'.
    ls_fil_extnr-option   = 'EQ'.
    ls_fil_extnr-low      = fu_extnr.
    APPEND ls_fil_extnr TO ls_log_filter-extnumber.
  ENDIF.


  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter     = ls_log_filter
    IMPORTING
      e_t_log_header     = lt_log_header[]
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_log_header[]
      IMPORTING
        e_t_log_handle     = lt_log_handle[]
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
*     I_S_DISPLAY_PROFILE  = lw_prof
      i_t_log_handle       = lt_log_handle[]
      i_s_log_filter       = ls_log_filter
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form fm_reprocessing
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_reprocessing .

  DATA : lw_reportsel LIKE LINE OF gi_reportsel.

  LOOP AT gi_reportsel INTO lw_reportsel.
    CASE lw_reportsel-if_code.
      WHEN 'INT_HR_001'.  " Create Loan Infotype 0045
        PERFORM fm_reprocess_create_loan USING lw_reportsel.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form fm_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_list .

  DATA: li_evts TYPE slis_t_event,
        lw_evts LIKE LINE OF li_evts.

  PERFORM fm_build_fieldcatalog.
  PERFORM fm_build_layout.

  lw_evts-name    = slis_ev_pf_status_set.
  lw_evts-form    = 'FM_PFSTATUS'.
  APPEND lw_evts TO li_evts.

  lw_evts-name    = slis_ev_top_of_page.
  lw_evts-form    = 'FM_TOP_OF_PAGE'.
  APPEND lw_evts TO li_evts.

  lw_evts-name    = slis_ev_user_command.
  lw_evts-form    = 'FM_USERCOMMAND'.
  APPEND lw_evts TO li_evts.

  lw_evts-name    = slis_ev_caller_exit_at_start.
  lw_evts-form    = 'FM_CALLEREXIT'.
  APPEND lw_evts TO li_evts.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid                   " Name of the calling program
      is_layout_lvc      = gw_layout                  " List Layout Specifications
      it_fieldcat_lvc    = gi_fieldcat
      it_events          = li_evts                    " Field Catalog with Field Descriptions
    TABLES
      t_outtab           = gi_report                  " Table with data to be displayed
    EXCEPTIONS
      program_error      = 1                          " Program Errors
      OTHERS             = 2.
ENDFORM.

FORM fm_callerexit USING ir_grid TYPE slis_data_caller_exit.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid_i.

  " ---Register carriage return event
  CALL METHOD g_grid_i->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gc_handler.
  SET HANDLER gc_handler->button_click FOR g_grid_i.

ENDFORM .

FORM fm_top_of_page.
  DATA : lt_header  TYPE slis_t_listheader,
         lwa_header TYPE slis_listheader.

  DATA : lv_count TYPE c LENGTH 5.

  DESCRIBE TABLE gi_report LINES lv_count.

  lwa_header-typ = 'H'. " Header type (bold text)
  lwa_header-info = 'Interface Monitoring Program'.
  APPEND lwa_header TO lt_header.
  CLEAR lwa_header.

  lwa_header-typ  = 'S'. " Selection type (key in bold, info normal)
  lwa_header-info = |Total number of lines: { lv_count }|.
  APPEND lwa_header TO lt_header.
  CLEAR lwa_header.

  lwa_header-typ  = 'S'. " Selection type (key in bold, info normal)
  lwa_header-info = cl_abap_char_utilities=>newline.
  APPEND lwa_header TO lt_header.
  CLEAR lwa_header.

  lwa_header-typ  = 'S'. " Selection type (key in bold, info normal)
  lwa_header-info = |Generated by: { sy-uname }|.
  APPEND lwa_header TO lt_header.
  CLEAR lwa_header.

  lwa_header-typ  = 'S'. " Selection type (key in bold, info normal)
  lwa_header-info = |Generate on: { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
  APPEND lwa_header TO lt_header.
  CLEAR lwa_header.

  APPEND lwa_header TO lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header
*     i_logo             = 'ENJOYSAP_LOGO' " Optional: add a standard SAP logo
    .
ENDFORM.

FORM fm_pfstatus USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STATUS01'.
ENDFORM.

FORM fm_usercommand USING r_ucomm LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.


  DATA : lv_refresh TYPE flag.
  DATA : li_rows TYPE lvc_t_row,
         lw_rows TYPE lvc_s_row.

  DATA : lv_xstring TYPE xstring,
         lv_len     TYPE i,
         li_content TYPE soli_tab,
         lv_string  TYPE string.

  DATA : lv_ans TYPE c.

  DATA : lw_stable TYPE lvc_s_stbl.

  lw_stable-row = 'X'.
  lw_stable-col = 'X'.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid_i.

  IF g_grid_i IS NOT INITIAL.
    lv_refresh = 'X'.

    CALL METHOD g_grid_i->check_changed_data
      CHANGING
        c_refresh = lv_refresh.
  ENDIF.

  CASE r_ucomm.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&IC1'.

    WHEN 'REPR'.
      REFRESH : gi_reportsel.

      CALL METHOD g_grid_i->get_selected_rows
        IMPORTING
          et_index_rows = li_rows.

      CALL METHOD cl_gui_cfw=>flush.

      LOOP AT li_rows INTO lw_rows.
        READ TABLE gi_report INTO gw_report INDEX lw_rows-index.
        IF gw_report-status = 'E'.
          APPEND gw_report TO gi_reportsel.
        ENDIF.
      ENDLOOP.

      IF gi_reportsel[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar       = 'Reprocessing Error Data'
            text_question  = 'Are you sure to reprocessing these data'
            text_button_1  = 'Yes'
            text_button_2  = 'No'
          IMPORTING
            answer         = lv_ans
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF lv_ans = '1'.
          PERFORM fm_reprocessing.
        ENDIF.
      ELSE.
        MESSAGE 'Please select error data first' TYPE 'I'.
      ENDIF.

  ENDCASE.

  CALL METHOD g_grid_i->refresh_table_display
    EXPORTING
      is_stable = lw_stable.                 " With Stable Rows/Columns

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_REPROCESS_CREATE_LOAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FU_REPORTSEL  text
*----------------------------------------------------------------------*
FORM fm_reprocess_create_loan  USING fu_reportsel TYPE ty_report.
  DATA : lw_p0045    TYPE p0045,
         lw_p0078    TYPE p0078,
         lw_key      TYPE bapipakey,
         lw_return   TYPE bapireturn1,
         lw_bapiret2 TYPE bapiret2.

  DATA : lw_data       TYPE zst_hr_loan_create,
         lw_return_log TYPE zst_hr_return_loan_create,
         lw_bcdt002    TYPE zta_bcdt002.

  DATA : lw_msg       TYPE bal_s_msg,
         lw_callback  TYPE bal_s_clbk,
         li_param_val TYPE bal_t_par,
         lw_param_val TYPE bal_s_par,
         lw_param     TYPE bal_s_parm.

  DATA : lv_action   TYPE pspar-actio VALUE 'INS',
         lv_message  TYPE string,
         lv_nocommit TYPE bapi_stand-no_commit VALUE IS INITIAL.

  DATA : lo_log TYPE REF TO zif_logger.

  DATA : lv_req_payload TYPE string.

  CLEAR : lw_p0045, lw_p0078, lw_return_log, lw_return, lw_key, lw_callback, lw_param_val, li_param_val[].

  lv_req_payload = cl_abap_codepage=>convert_from( source = fu_reportsel-req_payload ).

  /ui2/cl_json=>deserialize(
        EXPORTING
          json    = lv_req_payload
*       pretty_name = /ui2/cl_json=>pretty_mode-camel_case " Optional: Use for camelCase JSON keys
        CHANGING
          data = lw_data
      ).

  "----------------------- set log header -------------------------
  gw_bcdt001 = VALUE #( gi_bcdt001[ if_code = fu_reportsel-if_code ] OPTIONAL ).

  lo_log = zcl_logger=>new( object  = gw_bcdt001-balobj
                            subobject = gw_bcdt001-balsubobj
                            desc = |Reprocess Create Loan Personnel { lw_data-pernr ALPHA = IN }| ).

  lw_callback-userexitt = 'F'.
  lw_callback-userexitp = sy-repid.
  lw_callback-userexitf = 'ZFM_DISPLAY_JSON_PAYLOAD'.

  lw_param_val-parname  = 'LOG_UUID'.
  lw_param_val-parvalue = fu_reportsel-log_uuid.
  APPEND lw_param_val TO li_param_val.

  "----------------------- populate infotype 0045 -------------------------
  lw_p0045-pernr  = |{ lw_data-pernr ALPHA = IN }|.   "personnel number
  lw_p0045-infty  = '0045'.                           "infotype
  lw_p0045-subty  = lw_data-subty.                    "subtype
  lw_p0045-begda  = lw_data-begda.                    "begin date
  lw_p0045-endda  = lw_data-endda.                    "end date
  lw_p0045-dlart  = lw_data-subty.                    "loan type
  lw_p0045-extdl  = lw_data-extdl.                    "external ref no
  lw_p0045-datbw  = lw_data-datbw.                    "approval date
  lw_p0045-darbt  = lw_data-darbt.                    "loan amount
  lw_p0045-dbtcu  = lw_data-dbtcu.                    "currency
  lw_p0045-dlend  = lw_data-dlend.                    "end of loan
  lw_p0045-dkond  = |{ lw_data-dkond ALPHA = IN }|.   "loan condition
  lw_p0045-indin  = lw_data-indin.                    "individual Interest Rate
  lw_p0045-effin  = lw_data-effin.                    "effective Interest Rate
  lw_p0045-tilbg  = lw_data-tilbg.                    "repayment start
  lw_p0045-tilbt  = lw_data-tilbt.                    "repayment installment
  lw_p0045-tilcu  = lw_data-tilcu.                    "currency

  CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = lw_p0045-pernr
    IMPORTING
      return = lw_return.

  IF lw_return IS INITIAL.
    CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = lw_p0045-infty
        number        = lw_p0045-pernr
        subtype       = lw_p0045-subty
        validityend   = lw_p0045-endda
        validitybegin = lw_p0045-begda
        record        = lw_p0045
        operation     = lv_action
        dialog_mode   = '0'
        nocommit      = lv_nocommit
      IMPORTING
        return        = lw_return
        key           = lw_key.
  ENDIF.

  IF lw_return IS NOT INITIAL.
    lw_return_log = CORRESPONDING #( lw_return ).
    lw_return_log-pernr = lw_p0045-pernr.

    lv_message = |Personnel { lw_p0045-pernr } : { lw_return-message }|.

    CALL METHOD lo_log->e
      EXPORTING
        obj_to_log    = lv_message
        callback_form = lw_callback-userexitt
        callback_prog = lw_callback-userexitp
        callback_fm   = lw_callback-userexitf
        param_values  = li_param_val.

  ELSE.
    lv_message = lv_message = |Personnel { lw_p0045-pernr } : Infotype 0045 created successfully|.

    CALL METHOD lo_log->s
      EXPORTING
        obj_to_log    = lv_message
        callback_form = lw_callback-userexitt
        callback_prog = lw_callback-userexitp
        callback_fm   = lw_callback-userexitf
        param_values  = li_param_val.

    "----------------------- populate infotype 0078 -------------------------
    lw_p0078-pernr  = lw_p0045-pernr.                   "personnel number
    lw_p0078-infty  = '0078'.                           "infotype
    lw_p0078-subty  = |{ lw_data-zahla ALPHA = IN }|.   "subtype
    lw_p0078-begda  = lw_p0045-tilbg.                   "begin date
    lw_p0078-endda  = lw_p0045-tilbg.                   "end date
    lw_p0078-zahla  = |{ lw_data-zahla ALPHA = IN }|.   "payment type
    lw_p0078-dlart  = lw_p0045-dlart.                   "loan type
    lw_p0078-objid  = lw_key-objectid.                  "object id
    lw_p0078-betrg  = lw_p0045-darbt.                   "amount
    lw_p0078-btrcu  = lw_p0045-dbtcu.                   "currency

    CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = lw_p0078-infty
        number        = lw_p0078-pernr
        subtype       = lw_p0078-subty
        validityend   = lw_p0078-endda
        validitybegin = lw_p0078-begda
        record        = lw_p0078
        operation     = lv_action
        dialog_mode   = '0'
        nocommit      = lv_nocommit
      IMPORTING
        return        = lw_return.

    IF lw_return IS NOT INITIAL.
      lw_return_log = CORRESPONDING #( lw_return ).
      lw_return_log-pernr = lw_p0045-pernr.

      lv_message = |Personnel { lw_p0045-pernr } : { lw_return-message }|.

      CALL METHOD lo_log->e
        EXPORTING
          obj_to_log    = lv_message
          callback_form = lw_callback-userexitt
          callback_prog = lw_callback-userexitp
          callback_fm   = lw_callback-userexitf
          param_values  = li_param_val.

    ELSE.
      lw_return_log-pernr   = lw_p0045-pernr.
      lw_return_log-type    = 'S'.
      lw_return_log-message = |Personnel { lw_p0045-pernr } : Loan created successfully|.

      lv_message = |Personnel { lw_p0045-pernr } : Infotype 0078 created successfully|.

      CALL METHOD lo_log->s
        EXPORTING
          obj_to_log    = lv_message
          callback_form = lw_callback-userexitt
          callback_prog = lw_callback-userexitp
          callback_fm   = lw_callback-userexitf
          param_values  = li_param_val.

    ENDIF.
  ENDIF.

  IF lo_log IS BOUND.
    lw_bcdt002-log_uuid     = fu_reportsel-log_uuid.
    lw_bcdt002-if_group     = fu_reportsel-if_group.
    lw_bcdt002-if_code      = fu_reportsel-if_code.
    lw_bcdt002-direction    = fu_reportsel-direction.
    lw_bcdt002-if_type      = fu_reportsel-if_type.
    lw_bcdt002-retry_no     = fu_reportsel-retry_no + 1.
    lw_bcdt002-extnr        = fu_reportsel-extnr.
    lw_bcdt002-balnr        = lo_log->db_number.

    DATA(lv_req_string)    = /ui2/cl_json=>serialize(
                                data        = lw_data
                                compress    = abap_false                            " Optional: ABAP_TRUE to remove whitespace
                                pretty_name = /ui2/cl_json=>pretty_mode-camel_case  " Optional: Convert field names to camelCase
                              ).

    lw_bcdt002-req_payload  = cl_abap_codepage=>convert_to( source = lv_req_string ).


    DATA(lv_res_string)    = /ui2/cl_json=>serialize(
                                data        = lw_return_log
                                compress    = abap_false                            " Optional: ABAP_TRUE to remove whitespace
                                pretty_name = /ui2/cl_json=>pretty_mode-camel_case  " Optional: Convert field names to camelCase
                              ).

    lw_bcdt002-res_payload  = cl_abap_codepage=>convert_to( source = lv_res_string ).

    lw_bcdt002-ernam        = fu_reportsel-ernam.
    lw_bcdt002-erdat        = fu_reportsel-erdat.
    lw_bcdt002-erzet        = fu_reportsel-erzet.
    lw_bcdt002-aenam        = sy-uname.
    lw_bcdt002-aedat        = sy-datum.
    lw_bcdt002-aezet        = sy-uzeit.
    lw_bcdt002-status       = lw_return_log-type.
    lw_bcdt002-message      = lw_return_log-message.

    MODIFY zta_bcdt002 FROM lw_bcdt002.
    COMMIT WORK AND WAIT.
  ENDIF.

  READ TABLE gi_report INTO gw_report WITH KEY log_uuid = fu_reportsel-log_uuid.
  IF sy-subrc EQ 0.
    DATA(lv_index) = sy-tabix.
    MOVE-CORRESPONDING lw_bcdt002 TO gw_report.
    gw_report-excep   = SWITCH #( gw_report-status
                                  WHEN 'E' THEN icon_red_light
                                  ELSE icon_green_light ).
    MODIFY gi_report FROM gw_report
    INDEX lv_index.
  ENDIF.

ENDFORM.
