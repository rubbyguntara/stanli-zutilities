*&---------------------------------------------------------------------*
*& Include          ZIN_X_ALV_INCLUDE
*&---------------------------------------------------------------------*
*To add new function on GUI Status, you can copy GUI Status
*#SALV_TABLE_STANDARD#
*from program SAPLSALV_METADATA_STATUS to your program.

TYPE-POOLS:
  icon.

*&---------------------------------------------------------------------*
*&      Form  FM_BUILD_STATIC_OBJECT
*&---------------------------------------------------------------------*
*       The function to create static object
*----------------------------------------------------------------------*
*      <--co_alv_table    ALV table object
*      <--PTA_RESULT      ALV Result Internal table
*----------------------------------------------------------------------*
FORM fm_build_object CHANGING co_alv_table TYPE REF TO cl_salv_table
                                ct_data TYPE STANDARD TABLE. "#EC CALLED

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = co_alv_table
        CHANGING
          t_table      = ct_data[].
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM. "FM_BUILD_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FM_BUILD_STATIC_OBJECT
*&---------------------------------------------------------------------*
*       The function to create static object
*----------------------------------------------------------------------*
*      <--co_alv_table    ALV table object
*      <--PTA_RESULT      ALV Result Internal table
*----------------------------------------------------------------------*
FORM fm_bld_object_cont USING uo_cont TYPE REF TO cl_gui_container
                       CHANGING  co_alv_table  TYPE REF TO cl_salv_table
                                 ct_data TYPE STANDARD TABLE. "#EC CALLED

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = uo_cont
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = co_alv_table
        CHANGING
          t_table      = ct_data[].
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM. "FM_BUILD_OBJECT

*&---------------------------------------------------------------------*
*&      Form  FM_SET_SCREEN_STATUS
*&---------------------------------------------------------------------*
*       The function to create static object and set screen status
*----------------------------------------------------------------------*
*      <--co_alv_table     ALV table object
*----------------------------------------------------------------------*
FORM fm_set_screen_status USING uv_pf_status TYPE sypfkey
                          CHANGING   co_alv_table  TYPE REF TO cl_salv_table. "#EC CALLED

  DATA: lv_message TYPE string ##NEEDED,
        lo_cx_root TYPE REF TO cx_root.

  TRY.
      CALL METHOD co_alv_table->set_screen_status
        EXPORTING
          report        = sy-repid
          pfstatus      = uv_pf_status
          set_functions = co_alv_table->c_functions_all.

    CATCH cx_root INTO lo_cx_root ##CATCH_ALL.
      lv_message = lo_cx_root->get_text( ).

  ENDTRY.

ENDFORM. "FM_SET_SCREEN_STATUS

*&---------------------------------------------------------------------*
*&      Form  FM_SET_HEADER
*&---------------------------------------------------------------------*
*       The function to set header value
*----------------------------------------------------------------------*
*      -->uv_title         Program Title
*      -->uv_header1       Header Info Line 1
*      -->uv_header2       Header Info Line 2
*      -->uv_header3       Header Info Line 3
*      -->uv_header4       Header Info Line 4
*      -->uv_header5       Header Info Line 5
*      <--co_alv_table     ALV table object
*----------------------------------------------------------------------*
FORM fm_set_header USING uv_title TYPE string
                          uv_header1 TYPE string
                          uv_header2 TYPE string
                          uv_header3 TYPE string
                          uv_header4 TYPE string
                          uv_header5 TYPE string
               CHANGING   co_alv_table  TYPE REF TO cl_salv_table.
                                                            "#EC CALLED

  DATA:
    lo_header      TYPE REF TO cl_salv_form_layout_grid,
*        l_o_h_label     TYPE REF TO cl_salv_form_label,    "#EC NEEDED
    lo_header_info TYPE REF TO cl_salv_form_header_info ,   "#EC NEEDED
    lo_action_info TYPE REF TO cl_salv_form_action_info .   "#EC NEEDED

* Create Header object
  CREATE OBJECT lo_header.

* Specify the target row and column number where we need to set up the
*output

* Information in Bold
  CALL METHOD lo_header->create_header_information
    EXPORTING
      row     = 1
      column  = 1
      colspan = 2
      text    = uv_title
    RECEIVING
      r_value = lo_header_info.
  IF uv_header1 NE ''.
    CALL METHOD lo_header->create_action_information
      EXPORTING
        row     = 2
        column  = 1
        text    = uv_header1
      RECEIVING
        r_value = lo_action_info.
  ENDIF.
  IF uv_header2 NE ''.
    CALL METHOD lo_header->create_action_information
      EXPORTING
        row     = 3
        column  = 1
        text    = uv_header2
      RECEIVING
        r_value = lo_action_info.
  ENDIF.
  IF uv_header3 NE ''.
    CALL METHOD lo_header->create_action_information
      EXPORTING
        row     = 4
        column  = 1
        text    = uv_header3
      RECEIVING
        r_value = lo_action_info.
  ENDIF.
  IF uv_header4 NE ''.
    CALL METHOD lo_header->create_action_information
      EXPORTING
        row     = 5
        column  = 1
        text    = uv_header4
      RECEIVING
        r_value = lo_action_info.
  ENDIF.
  IF uv_header5 NE ''.
    CALL METHOD lo_header->create_action_information
      EXPORTING
        row     = 6
        column  = 1
        text    = uv_header5
      RECEIVING
        r_value = lo_action_info.
  ENDIF.
* Set the top of list using the header
  co_alv_table->set_top_of_list( lo_header ).

* Set the top of list using the header for Print
  co_alv_table->set_top_of_list_print( lo_header ).
ENDFORM. "FM_SET_HEADER

*&---------------------------------------------------------------------*
*&      Form  FM_BUILD_SORT
*&---------------------------------------------------------------------*
*       Set sort bahaviour for alv report
*----------------------------------------------------------------------*
*      <--co_alv_table   ALV table object
*----------------------------------------------------------------------*
FORM fm_build_sort USING uv_sort1 TYPE lvc_fname
                        uv_sort2 TYPE lvc_fname
                        uv_sort3 TYPE lvc_fname
                        uv_sort4 TYPE lvc_fname
               CHANGING co_alv_table  TYPE REF TO cl_salv_table. "#EC CALLED

  DATA:
      lo_alv_sorts             TYPE REF TO cl_salv_sorts.

  CALL METHOD co_alv_table->get_sorts
    RECEIVING
      value = lo_alv_sorts.

  TRY.
      CALL METHOD lo_alv_sorts->add_sort
        EXPORTING
          columnname = uv_sort1
          position   = 1
          sequence   = if_salv_c_sort=>sort_up.
    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
    CATCH cx_salv_existing .                            "#EC NO_HANDLER
    CATCH cx_salv_data_error .                          "#EC NO_HANDLER
  ENDTRY.
  IF uv_sort2 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort2
            position   = 2
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
        "
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
  IF uv_sort3 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort3
            position   = 3
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
  IF uv_sort4 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort4
            position   = 4
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
        "
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

ENDFORM. " FM_BUILD_SORT

* Sort with subtotal
*&---------------------------------------------------------------------*
*&      Form  FM_BUILD_SORT
*&---------------------------------------------------------------------*
*       Set sort bahaviour for alv report
*----------------------------------------------------------------------*
*      <--co_alv_table   ALV table object
*----------------------------------------------------------------------*
FORM fm_build_sort_sbtl USING uv_sort1 TYPE lvc_fname
                             uv_sbtl1 TYPE abap_bool
                             uv_sort2 TYPE lvc_fname
                             uv_sbtl2 TYPE abap_bool
                             uv_sort3 TYPE lvc_fname
                             uv_sbtl3 TYPE abap_bool
                             uv_sort4 TYPE lvc_fname
                             uv_sbtl4 TYPE abap_bool
                    CHANGING co_alv_table  TYPE REF TO cl_salv_table. "#EC CALLED

  DATA:
      lo_alv_sorts             TYPE REF TO cl_salv_sorts.

  CALL METHOD co_alv_table->get_sorts
    RECEIVING
      value = lo_alv_sorts.

  TRY.
      CALL METHOD lo_alv_sorts->add_sort
        EXPORTING
          columnname = uv_sort1
          position   = 1
          subtotal   = uv_sbtl1
          sequence   = if_salv_c_sort=>sort_up.
    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
    CATCH cx_salv_existing .                            "#EC NO_HANDLER
    CATCH cx_salv_data_error .                          "#EC NO_HANDLER
  ENDTRY.
  IF uv_sort2 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort2
            position   = 2
            subtotal   = uv_sbtl2
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
        "
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
  IF uv_sort3 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort3
            position   = 3
            subtotal   = uv_sbtl3
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
  IF uv_sort4 NE ''.
    TRY.
        CALL METHOD lo_alv_sorts->add_sort
          EXPORTING
            columnname = uv_sort4
            position   = 4
            subtotal   = uv_sbtl4
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
        "
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

ENDFORM. " FM_BUILD_SORT

*&---------------------------------------------------------------------*
*&      Form  FM_MODIFY_FIELD
*&---------------------------------------------------------------------*
*       Modify Field
*----------------------------------------------------------------------*
*      <--co_alv_table              ALV table object
*----------------------------------------------------------------------*
FORM fm_modify_field USING uv_columname TYPE lvc_fname
                          uv_method TYPE any
                          uv_value TYPE any
                 CHANGING co_alv_table  TYPE REF TO cl_salv_table.
                                                            "#EC CALLED

  DATA:
    lo_alv_alv_columns_table TYPE REF TO cl_salv_columns_table,
    lo_alv_alv_column_table  TYPE REF TO cl_salv_column_table,
    lo_alv_alv_column        TYPE REF TO cl_salv_column.

  DATA:
    lx_salv_not_found TYPE REF TO cx_salv_not_found,
    lw_color          TYPE lvc_s_colo,
    lv_error_message  TYPE string.

  CALL METHOD co_alv_table->get_columns
    RECEIVING
      value = lo_alv_alv_columns_table.

* Modify field section code text
  IF uv_method EQ 'SET_KEY'.
    TRY .
        " Get reference for ALV column object.
        lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_columname ).

        " Set new key to the table. PRICE is not key in SFLIGHT, but will appear as one now (it's just for test, ok?)
        " Observe that we use another class to manipulate the table key
        lo_alv_alv_columns_table->set_key_fixation( if_salv_c_bool_sap=>true ).
        lo_alv_alv_column_table->set_key( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found INTO lx_salv_not_found.
        lv_error_message = lx_salv_not_found->get_text( ).
        WRITE lv_error_message.
    ENDTRY.
  ELSEIF uv_method EQ 'SET_HOTSPOT'.
    TRY .
        " Get reference for ALV column object.
        lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_columname ).

        CALL METHOD lo_alv_alv_column_table->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.

      CATCH cx_salv_not_found INTO lx_salv_not_found.
        lv_error_message = lx_salv_not_found->get_text( ).
        WRITE lv_error_message.
    ENDTRY.
  ELSEIF uv_method EQ 'SET_COLOR_COLUMN'.
    TRY .
        " Get reference for ALV column object.
        lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_columname ).
        lw_color-col  = uv_value.

        CALL METHOD lo_alv_alv_column_table->set_color
          EXPORTING
            value = lw_color.

      CATCH cx_salv_not_found INTO lx_salv_not_found.
        lv_error_message = lx_salv_not_found->get_text( ).
        WRITE lv_error_message.
    ENDTRY.
  ELSEIF uv_method EQ 'SET_ICON'.
    TRY .
        " Get reference for ALV column object.
        lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_columname ).

        CALL METHOD lo_alv_alv_column_table->set_icon
          EXPORTING
            value = if_salv_c_bool_sap=>true.

        CALL METHOD lo_alv_alv_column_table->set_alignment
          EXPORTING
            value = if_salv_c_alignment=>centered.

      CATCH cx_salv_not_found INTO lx_salv_not_found.
        lv_error_message = lx_salv_not_found->get_text( ).
        WRITE lv_error_message.
    ENDTRY.
  ELSEIF uv_method EQ 'SET_CHECKBOX'.
    TRY .
        " Get reference for ALV column object.
        lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_columname ).

        CALL METHOD lo_alv_alv_column_table->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>checkbox_hotspot.

      CATCH cx_salv_not_found INTO lx_salv_not_found.
        lv_error_message = lx_salv_not_found->get_text( ).
        WRITE lv_error_message.
    ENDTRY.
  ELSE.
    TRY.
        CALL METHOD lo_alv_alv_columns_table->get_column
          EXPORTING
            columnname = uv_columname
          RECEIVING
            value      = lo_alv_alv_column.
      CATCH cx_salv_not_found  .                        "#EC NO_HANDLER
    ENDTRY.
  ENDIF.


  IF lo_alv_alv_column IS NOT INITIAL.
    CALL METHOD lo_alv_alv_column->(uv_method)
      EXPORTING
        value = uv_value.
  ENDIF.

ENDFORM. " FM_MODIFY_FIELD

*&---------------------------------------------------------------------*
*&      Form  FM_SET_HOTSPOT
*&---------------------------------------------------------------------*
*       Modify Field
*----------------------------------------------------------------------*
*      <--co_alv_table              ALV table object
*----------------------------------------------------------------------*
FORM fm_set_hotspot_checkbox USING uv_value TYPE any
                                  uv_hotspot   TYPE any
                                  uv_checkbox  TYPE any
                         CHANGING co_alv_table TYPE REF TO cl_salv_table.
                                                            "#EC CALLED

  DATA: lv_set_cell_typ_val TYPE salv_de_celltype,
        lv_exref            TYPE REF TO cx_root,
        lv_err_msg          TYPE string ##NEEDED.

  CASE abap_true.
    WHEN uv_hotspot.
      lv_set_cell_typ_val = if_salv_c_cell_type=>hotspot.
    WHEN uv_checkbox.
      lv_set_cell_typ_val = if_salv_c_cell_type=>checkbox_hotspot.
    WHEN OTHERS.
  ENDCASE.

  DATA:
    lo_alv_alv_columns_table TYPE REF TO cl_salv_columns_table,
    lo_alv_alv_column_table  TYPE REF TO cl_salv_column_table.

  CALL METHOD co_alv_table->get_columns
    RECEIVING
      value = lo_alv_alv_columns_table.

  TRY.
      lo_alv_alv_column_table ?= lo_alv_alv_columns_table->get_column( uv_value ).
    CATCH cx_salv_not_found INTO lv_exref.
      lv_err_msg = lv_exref->get_text( ).
  ENDTRY.

  CALL METHOD lo_alv_alv_column_table->set_cell_type
    EXPORTING
      value = lv_set_cell_typ_val.

ENDFORM. " FM_SET_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  FM_OPTIMIZE_FIELD
*&---------------------------------------------------------------------*
*       Optimize Field
*----------------------------------------------------------------------*
*      <--P_O_ALV_TABLE  ALV Table Object
*----------------------------------------------------------------------*
FORM fm_optimize_field CHANGING
  co_alv_table TYPE REF TO cl_salv_table.                   "#EC CALLED

  DATA lo_alv_alv_columns_table TYPE REF TO cl_salv_columns_table.

  CALL METHOD co_alv_table->get_columns
    RECEIVING
      value = lo_alv_alv_columns_table.

  lo_alv_alv_columns_table->set_optimize( abap_true ).
ENDFORM. " FM_OPTIMIZE_FIELD

*&---------------------------------------------------------------------*
*&      Form  FM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       Set layout
*----------------------------------------------------------------------*
*      <--co_alv_table          ALV table object
*----------------------------------------------------------------------*
FORM fm_set_layout_cl USING uv_variant TYPE slis_vari
                   CHANGING co_alv_table TYPE REF TO cl_salv_table. "#EC CALLED

  DATA:
    lo_alv_layout TYPE REF TO cl_salv_layout,
    lw_layout_key TYPE salv_s_layout_key.


  CALL METHOD co_alv_table->get_layout
    RECEIVING
      value = lo_alv_layout.

  lw_layout_key-report = sy-repid.

  CALL METHOD lo_alv_layout->set_key
    EXPORTING
      value = lw_layout_key.

  CALL METHOD lo_alv_layout->set_initial_layout
    EXPORTING
      value = uv_variant.

  CALL METHOD lo_alv_layout->set_save_restriction
    EXPORTING
      value = if_salv_c_layout=>restrict_none.

ENDFORM. " FM_SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  FM_STRIPPED_LAYOUT
*&---------------------------------------------------------------------*
*       Set stripped layout
*----------------------------------------------------------------------*
*      -->co_alv_table  ALV Table Object
*----------------------------------------------------------------------*
FORM fm_stripped_layout CHANGING
  co_alv_table TYPE REF TO cl_salv_table.                   "#EC CALLED

  DATA lo_alv_display TYPE REF TO cl_salv_display_settings.

  TRY.
      CALL METHOD co_alv_table->get_display_settings
        RECEIVING
          value = lo_alv_display.

      lo_alv_display->set_striped_pattern( abap_true ).
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.

ENDFORM. "FM_STRIPPED_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FM_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       Display alv report
*----------------------------------------------------------------------*
*      <--co_alv_table              ALV table object
*----------------------------------------------------------------------*
FORM fM_display_report CHANGING co_alv_table TYPE REF TO cl_salv_table. "#EC CALLED

  CALL METHOD co_alv_table->display.

ENDFORM. " FM_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  FM_SET_AGGREGATIONS
*&---------------------------------------------------------------------*
*       To set aggregate function in ALV Object
*----------------------------------------------------------------------*
*      <--P_O_ALV_TABLE  text
*----------------------------------------------------------------------*
FORM fm_set_aggregations USING uv_columname TYPE lvc_fname
                       CHANGING  co_alv_table  TYPE REF TO cl_salv_table. "#EC CALLED
  DATA:
      lo_alv_aggregate TYPE REF TO cl_salv_aggregations.

  CALL METHOD co_alv_table->get_aggregations
    RECEIVING
      value = lo_alv_aggregate.

  TRY.
      CALL METHOD lo_alv_aggregate->add_aggregation
        EXPORTING
          columnname  = uv_columname
          aggregation = if_salv_c_aggregation=>total.
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
    CATCH cx_salv_existing.                             "#EC NO_HANDLER
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

ENDFORM. " FM_SET_AGGREGATIONS


*&---------------------------------------------------------------------*
*&      Form  fm_modify_fields_alv
*&---------------------------------------------------------------------*
*       modify ALV fields
*----------------------------------------------------------------------*
*      -->co_alv_table  text
*----------------------------------------------------------------------*
FORM fm_modify_field_column
  USING uv_column_name TYPE lvc_fname
        uv_key      TYPE abap_bool
        uv_stext    TYPE scrtext_s
        uv_mtext    TYPE scrtext_m
        uv_ltext    TYPE scrtext_l
        uv_hide     TYPE abap_bool
        uv_tech     TYPE abap_bool
        uv_hotspot  TYPE abap_bool
        uv_curr     TYPE lvc_fname
        uv_uom      TYPE lvc_fname
        uv_color    TYPE c
        uv_icon     TYPE abap_bool
        uv_check    TYPE abap_bool
  CHANGING co_alv_table TYPE REF TO cl_salv_table.          "#EC CALLED

  DATA:
        uv_hide_negate TYPE abap_bool.

  IF uv_key IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_KEY'
                                    uv_key
                           CHANGING co_alv_table.
  ENDIF.

  IF uv_stext IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_SHORT_TEXT'
                                    uv_stext
                           CHANGING co_alv_table.
  ENDIF.

  IF uv_mtext IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_MEDIUM_TEXT'
                                    uv_mtext
                           CHANGING co_alv_table.
  ENDIF.

  IF uv_ltext IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_LONG_TEXT'
                                    uv_ltext
                           CHANGING co_alv_table.
  ENDIF.

  PERFORM fm_modify_field USING    uv_column_name
                                  'SET_TECHNICAL'
                                  uv_tech
                         CHANGING co_alv_table.

  IF uv_icon IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_ICON'
                                    uv_icon
                           CHANGING co_alv_table.
  ENDIF.

  IF uv_check IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_CHECKBOX'
                                    uv_icon
                           CHANGING co_alv_table.
  ENDIF.


  IF uv_hide EQ abap_true.
    uv_hide_negate = space.
  ELSE.
    uv_hide_negate = abap_true.
  ENDIF.

  IF uv_hotspot IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_HOTSPOT'
                                    uv_hotspot
                           CHANGING co_alv_table.
  ENDIF.

  IF uv_color IS NOT INITIAL.
    PERFORM fm_modify_field USING    uv_column_name
                                    'SET_COLOR_COLUMN'
                                    uv_color
                           CHANGING co_alv_table.
  ENDIF.

  PERFORM fm_modify_field USING    uv_column_name
                                  'SET_VISIBLE'
                                  uv_hide_negate
                         CHANGING co_alv_table.

  PERFORM fm_modify_field USING    uv_column_name
                                  'SET_CURRENCY_COLUMN'
                                  uv_curr
                         CHANGING co_alv_table.

  PERFORM fm_modify_field USING    uv_column_name
                                  'SET_QUANTITY_COLUMN'
                                  uv_uom
                         CHANGING co_alv_table.

ENDFORM. "fm_modify_fields

*&---------------------------------------------------------------------*
*&      Form  fm_set_color_column
*&---------------------------------------------------------------------*
*       Set Color Column
*----------------------------------------------------------------------*
*      -->uv_col_name  text
*----------------------------------------------------------------------*
FORM fm_set_color_column
  USING uv_col_name TYPE lvc_fname
  CHANGING co_alv_table TYPE REF TO cl_salv_table.          "#EC CALLED

  DATA:
        lo_columns TYPE REF TO cl_salv_columns_table.

  lo_columns = co_alv_table->get_columns( ).
  TRY.
      CALL METHOD lo_columns->set_color_column
        EXPORTING
          value = uv_col_name.
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

ENDFORM. "fm_set_color_column

FORM fm_set_selection USING uv_sel_mode TYPE salv_de_constant
                     CHANGING  co_alv_table TYPE REF TO cl_salv_table
                               co_selection TYPE REF TO cl_salv_selections. "#EC CALLED

  co_selection = co_alv_table->get_selections( ).
  co_selection->set_selection_mode( uv_sel_mode ).

ENDFORM.
