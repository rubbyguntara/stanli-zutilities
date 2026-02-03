FUNCTION zfm_display_json_payload.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_T_PARAMS STRUCTURE  SPAR
*"----------------------------------------------------------------------

  DATA : ls_param   LIKE LINE OF i_t_params,
         ls_bcdt002 TYPE zta_bcdt002.

  DATA : lt_options    TYPE TABLE OF spopli,
         ls_option     TYPE spopli,
         lv_answer     TYPE c,
         lv_reqpayload TYPE string,
         lv_respayload TYPE string,
         lv_payload    TYPE xstring.

  " Populate options for popup
  ls_option-varoption = 'JSON Request'.
  APPEND ls_option TO lt_options.
  ls_option-varoption = 'JSON Response'.
  APPEND ls_option TO lt_options.

  LOOP AT i_t_params INTO ls_param.
    CASE ls_param-param.
      WHEN 'LOG_UUID'.
        " Display popup
        CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
          EXPORTING
            textline1 = 'Please select an option:'
            titel     = 'JSON Log'
          IMPORTING
            answer    = lv_answer
          TABLES
            t_spopli  = lt_options
          EXCEPTIONS
            OTHERS    = 1.

        IF sy-subrc <> 0 OR lv_answer IS INITIAL.
          MESSAGE i000(00) WITH 'No option selected or error occurred.'.
          RETURN.
        ENDIF.

        SELECT SINGLE * FROM zta_bcdt002
          INTO ls_bcdt002
          WHERE log_uuid EQ ls_param-value.

        IF sy-subrc <> 0.
          MESSAGE i000(00) WITH 'LOG_UUID' ls_param-value 'not found.'.
          RETURN.
        ENDIF.

        " Display selected payload
        lv_payload = SWITCH #( lv_answer
                                WHEN '1' THEN ls_bcdt002-req_payload
                                WHEN '2' THEN ls_bcdt002-res_payload ).

        IF lv_payload IS INITIAL.
          MESSAGE i000(00) WITH 'No payload data available.'.
          RETURN.
        ENDIF.

        TRY.
            CALL TRANSFORMATION sjson2html
              SOURCE XML lv_payload
              RESULT XML DATA(lvc_html).

            DATA(lv_title) = COND cl_abap_browser=>title(
              WHEN lv_answer = '1' THEN 'JSON Request Log'
              ELSE 'JSON Response Log' ).

            cl_abap_browser=>show_html(
              title = lv_title
              html_string = cl_abap_codepage=>convert_from( lvc_html ) ).

          CATCH cx_transformation_error INTO DATA(lo_error).
            MESSAGE i000(00) WITH 'JSON transformation error:' lo_error->get_text( ).
        ENDTRY.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.





ENDFUNCTION.
