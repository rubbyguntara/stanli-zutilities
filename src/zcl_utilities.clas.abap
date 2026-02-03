class ZCL_UTILITIES definition
  public
  final
  create public .

public section.

  types TY_RANGE type EBA_RANGES .
  types:
    ty_r_range TYPE STANDARD TABLE OF ty_range .
  types:
    ty_t_tvarvc TYPE STANDARD TABLE OF tvarvc .

  constants CO_TYPE_PARAMETER type TVARVC-TYPE value 'P' ##NO_TEXT.
  constants CO_TYPE_SELECT_OPTION type TVARVC-TYPE value 'S' ##NO_TEXT.

  class-methods GET_TVARV_PARAMETER
    importing
      value(IM_NAME) type TVARVC-NAME
    exporting
      value(EX_VALUE) type CLIKE
    exceptions
      NOT_FOUND .
  class-methods GET_TVARV_SELECT_OPTION
    importing
      value(IM_NAME) type TVARVC-NAME
    exporting
      value(EX_VALUE) type ZCL_UTILITIES=>TY_R_RANGE
    exceptions
      NOT_FOUND .
  class-methods GET_TVARVC
    importing
      value(IM_NAME) type TVARVC-NAME
      value(IM_TYPE) type TVARVC-TYPE
    exporting
      value(EX_TVARVC) type TY_T_TVARVC
    exceptions
      NOT_FOUND .
  class-methods REMOVE_SPECIAL_CHAR
    importing
      !IM_STRING type STRING
    exporting
      !EX_STRING type STRING .
  class-methods SEND_EMAIL
    importing
      !IM_SENDER type STRING optional
      !IM_RECIPIENT type STRING
      !IM_CC type STRING optional
      !IM_SUBJECT type STRING
      !IM_DO_COMMIT type OS_BOOLEAN default ABAP_TRUE
      !IM_ATTACHMENT_TYPE type SOODK-OBJTP optional
      !IM_ATTACHMENT_SUBJECT type SOOD-OBJDES optional
      !IM_ATTACHMENT_SIZE type SOOD-OBJLEN optional
      !IM_BODY type BCSY_TEXT
      !IM_ATTACHMENT type SOLIX_TAB optional
    returning
      value(RE_RESULT) type OS_BOOLEAN .
  class-methods GET_ATTACH_PDF
    importing
      value(IM_OTF) type TSFOTF
    exporting
      !EX_CONTEXT_HEX type SOLIX_TAB
      !EX_LEN type SO_OBJ_LEN .
  class-methods CONVERT_STR_TO_XSTRING
    importing
      !IM_STR type STRING
    exporting
      !EX_XSTR type XSTRING .
  class-methods CONVERT_XSTRING_TO_STR
    importing
      !IM_XSTRING type XSTRING
    exporting
      !EX_STRING type STRING .
  class-methods CONVERT_TO_LOCAL_CURR
    importing
      !IM_AMOUNT_INPUT type ANY
      !IM_RATE type BAPI1093_1-RATE_TYPE
      !IM_DATE type DATUM
      !IM_FROM_CURR type WAERS
      !IM_TO_CURR type WAERS
    exporting
      !EX_AMOUNT_OUTPUT type ANY
    exceptions
      NOT_FOUND
      BAPI_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UTILITIES IMPLEMENTATION.


  METHOD convert_str_to_xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = im_str
      IMPORTING
        buffer = ex_xstr.
  ENDMETHOD.


  METHOD convert_to_local_curr.
    DATA: lwa_exch_rate TYPE bapi1093_0,
          lwa_return    TYPE bapiret1,
          lv_curr_ext   TYPE bapicurr-bapicurr,
          lv_curr_int   TYPE bapicurr-bapicurr,
          lv_amount     TYPE wertv13.

    CONSTANTS: c_usd    TYPE waers  VALUE 'USD'.

    IF im_amount_input IS NOT INITIAL
      AND im_from_curr NE c_usd. "USD
      lv_curr_int = im_amount_input.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = im_from_curr
          amount_internal = lv_curr_int
        IMPORTING
          amount_external = lv_curr_ext.

      lv_amount = lv_curr_ext.
    ENDIF.

    "Get Exchange Rate
    CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
      EXPORTING
        rate_type  = im_rate
        from_curr  = im_from_curr
        to_currncy = im_to_curr
        date       = im_date
      IMPORTING
        exch_rate  = lwa_exch_rate
        return     = lwa_return.

    IF lwa_return IS NOT INITIAL
      AND lwa_return-type NE fiehc_con_msgty_s. "S
      MESSAGE ID lwa_return-id
        TYPE fiehc_con_msgty_e "lwa_return-type
        NUMBER lwa_return-number
        WITH lwa_return-message_v1
             lwa_return-message_v2
             lwa_return-message_v3
             lwa_return-message_v4
        RAISING bapi_error.

    ELSEIF lwa_exch_rate-exch_rate IS INITIAL
      AND lwa_exch_rate-exch_rate_v IS INITIAL.
      "No exchange rate available for & & & &
      MESSAGE e024(e!) WITH im_rate
                            im_from_curr
                            im_to_curr
                            im_date
       RAISING not_found.
    ENDIF.

    "for from currency rate < to currency rate
    IF lwa_exch_rate-exch_rate_v IS NOT INITIAL
      AND lwa_exch_rate-from_factor IS NOT INITIAL
      AND lwa_exch_rate-to_factor IS NOT INITIAL.
      ex_amount_output = ( lv_amount * lwa_exch_rate-to_factor ) / ( lwa_exch_rate-exch_rate_v * lwa_exch_rate-from_factor ).

      "for from currency rate > to currency rate
    ELSEIF lwa_exch_rate-exch_rate IS NOT INITIAL
      AND lwa_exch_rate-to_factor IS NOT INITIAL.
      ex_amount_output = ( lv_amount * lwa_exch_rate-exch_rate * lwa_exch_rate-to_factor ).

    ELSE.
      "No exchange rate available for & & & &
      MESSAGE e024(e!) WITH im_rate
                            im_from_curr
                            im_to_curr
                            im_date
       RAISING not_found.
    ENDIF.

  ENDMETHOD.


  METHOD convert_xstring_to_str.

    DATA: lr_conv TYPE REF TO cl_abap_conv_in_ce.

    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        input       = im_xstring
        encoding    = 'UTF-8'
        replacement = '?'
        ignore_cerr = abap_true
      RECEIVING
        conv        = lr_conv.

    CALL METHOD lr_conv->read
      IMPORTING
        data = ex_string.
  ENDMETHOD.


  METHOD get_attach_pdf.

*/.. Data Declarations
    DATA: lw_otf          TYPE itcoo,
          lw_content      TYPE soli,
          li_content_txt  TYPE soli_tab,
          li_content_hex  TYPE solix_tab,
          li_objhead      TYPE soli_tab,
          lv_len          TYPE so_obj_len,
          lv_bin_filesize TYPE i,
          lv_transfer_bin TYPE sx_boolean.

    REFRESH: li_content_txt,
             li_content_hex,
             li_objhead.

    CLEAR: lv_transfer_bin,
           lv_len.

    LOOP AT im_otf  INTO  lw_otf.
      CLEAR lw_content.
      CONCATENATE lw_otf-tdprintcom lw_otf-tdprintpar
             INTO lw_content.
      APPEND lw_content TO li_content_txt.
    ENDLOOP.

*/.. FM to convert OTF to PDF
    CALL FUNCTION 'SX_OBJECT_CONVERT_OTF_PDF'
      EXPORTING
        format_src      = 'OTF'
        format_dst      = 'PDF'
      CHANGING
        transfer_bin    = lv_transfer_bin
        content_txt     = li_content_txt
        content_bin     = li_content_hex
        objhead         = li_objhead
        len             = lv_len
      EXCEPTIONS
        err_conv_failed = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ex_context_hex = li_content_hex.
    ex_len = lv_len.

  ENDMETHOD.


  METHOD get_tvarvc.
    SELECT *
   INTO TABLE ex_tvarvc[]
   FROM tvarvc
   WHERE name = im_name
     AND type = im_type.

    IF sy-subrc <> 0.
      RAISE not_found.
    ELSE.
      SORT ex_tvarvc[] BY name
                          type
                          numb.
    ENDIF.
  ENDMETHOD.


  METHOD get_tvarv_parameter.
    DATA:
      li_tvarvc  TYPE zcl_utilities=>ty_t_tvarvc,
      lwa_tvarvc TYPE tvarvc.

    "Get Table TVARVC
    CALL METHOD zcl_utilities=>get_tvarvc
      EXPORTING
        im_name   = im_name
        im_type   = co_type_parameter  "P
      IMPORTING
        ex_tvarvc = li_tvarvc[]
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE e742(db) WITH im_name
        RAISING not_found.
    ENDIF.

    READ TABLE li_tvarvc[]
      INTO lwa_tvarvc
      INDEX 1.

    IF sy-subrc = 0.
      ex_value = lwa_tvarvc-low.
    ENDIF.
  ENDMETHOD.


  METHOD get_tvarv_select_option.
    DATA:
      li_tvarvc  TYPE zcl_utilities=>ty_t_tvarvc,
      lwa_tvarvc TYPE tvarvc,
      lwa_range  TYPE zcl_utilities=>ty_range.

    "Get Table TVARVC
    CALL METHOD zcl_utilities=>get_tvarvc
      EXPORTING
        im_name   = im_name
        im_type   = co_type_select_option  "S
      IMPORTING
        ex_tvarvc = li_tvarvc[]
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE e743(db) WITH im_name
        RAISING not_found.
    ENDIF.

    "Populate Range Variable
    LOOP AT li_tvarvc[] INTO lwa_tvarvc.
      CLEAR lwa_range.
      lwa_range-sign    = lwa_tvarvc-sign.
      lwa_range-option  = lwa_tvarvc-opti.
      lwa_range-low     = lwa_tvarvc-low.
      lwa_range-high    = lwa_tvarvc-high.
      APPEND lwa_range TO ex_value[].
    ENDLOOP.
  ENDMETHOD.


  METHOD remove_special_char.

    DATA: l_number_in      TYPE sph_call-no_dialed,
          l_number_out     TYPE sph_call-no_dialed,
          allowed_char(37) VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',
          l_char_len       TYPE i,
          l_num_len        TYPE i,
          l_pos            TYPE i.
    MOVE im_string TO l_number_in.
    CLEAR l_number_out.
    CONDENSE l_number_in NO-GAPS.
    TRANSLATE l_number_in TO UPPER CASE.                 "#EC TRANSLANG
    IF l_number_in CN allowed_char.
      l_num_len = strlen( l_number_in ).
      l_pos = 0.
      WHILE l_pos < l_num_len.
        l_char_len = charlen( l_number_in+l_pos ).
        IF l_number_in+l_pos(l_char_len) CO allowed_char.
          CONCATENATE l_number_out l_number_in+l_pos(l_char_len) INTO l_number_out.
        ENDIF.
        ADD l_char_len TO l_pos.
      ENDWHILE.

      MOVE l_number_out TO ex_string.
    ELSE.
      MOVE l_number_in TO ex_string.
    ENDIF.
  ENDMETHOD.


  METHOD send_email.

    DATA: lc_bcs_ex         TYPE REF TO cx_bcs,
          lc_doc            TYPE REF TO cl_document_bcs,
          lc_doc_ex         TYPE REF TO cx_document_bcs,
          lc_recipients     TYPE REF TO if_recipient_bcs,
          lc_sender         TYPE REF TO if_sender_bcs,
          lc_address_bcs_ex TYPE REF TO cx_address_bcs.
    DATA: li_emails TYPE bcsy_smtpa.
    DATA: lv_uname  TYPE sy-uname,
          lv_email  TYPE adr6-smtp_addr,
          lv_objdes TYPE so_obj_des.

    TRY.
        DATA(lc_bcs) = cl_bcs=>create_persistent( ).
      CATCH cx_send_req_bcs INTO DATA(lc_send_req_bcs_ex).  "
        EXIT.
    ENDTRY.

****Sender************************************************************
    IF im_sender IS INITIAL.
      TRY.
          lc_sender = cl_sapuser_bcs=>create( i_user = sy-uname ).
        CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
      ENDTRY.

    ELSE.
      FIND FIRST OCCURRENCE OF '@' IN im_sender.
      IF sy-subrc IS NOT INITIAL.
        lv_uname = im_sender.
        TRY.
            lc_sender = cl_sapuser_bcs=>create( i_user = lv_uname ).
          CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
        ENDTRY.

      ELSE.
        lv_email = im_sender.
        TRY .
            lc_sender = cl_cam_address_bcs=>create_internet_address( lv_email ).
          CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
        ENDTRY.
      ENDIF.
    ENDIF.

    TRY.
        lc_bcs->set_sender( i_sender = lc_sender ).
      CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
    ENDTRY.

****Recipient*********************************************************
    SPLIT im_recipient AT ';' INTO TABLE li_emails.
    LOOP AT li_emails INTO lv_email.

      FIND FIRST OCCURRENCE OF '@' IN lv_email.
      IF sy-subrc IS NOT INITIAL.

        lv_uname = lv_email.
        TRY.
            lc_recipients = cl_sapuser_bcs=>create( i_user = lv_uname ).
          CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
        ENDTRY.

      ELSE.
        TRY .
            lc_recipients = cl_cam_address_bcs=>create_internet_address( lv_email ).
          CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
        ENDTRY.
      ENDIF.

      TRY.
          lc_bcs->add_recipient(
              i_recipient = lc_recipients
              i_express   = 'X'
              ).
        CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
      ENDTRY.
    ENDLOOP.

****CC****************************************************************
    FREE lc_recipients.
    CLEAR li_emails.
    SPLIT im_cc AT ';' INTO TABLE li_emails.
    LOOP AT li_emails INTO lv_email.
      TRY .
          lc_recipients = cl_cam_address_bcs=>create_internet_address( lv_email ).
        CATCH cx_address_bcs INTO lc_address_bcs_ex.  "
      ENDTRY.
      TRY.
          lc_bcs->add_recipient(
              i_recipient     = lc_recipients    " Recipient of Message
              i_copy          = abap_true    " Send Copy
          ).
        CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
      ENDTRY.
    ENDLOOP.

    lv_objdes = im_subject.
    TRY.
        lc_doc = cl_document_bcs=>create_document(
                 i_type          = 'HTM'
                 i_subject       = lv_objdes
                 i_text          = im_body ).
      CATCH cx_document_bcs INTO lc_doc_ex.  "
    ENDTRY.

****Attachment********************************************************
    IF im_attachment[] IS NOT INITIAL AND
       im_attachment_type IS NOT INITIAL AND
       im_attachment_subject IS NOT INITIAL AND
       im_attachment_size IS NOT INITIAL.
      TRY .
          DATA(lc_obj_head) = cl_bcs_objhead=>create( ).
          lc_obj_head->set_filename( im_attachment_subject ).
          lc_doc->add_attachment(
            EXPORTING
              i_attachment_type     = im_attachment_type    " Document Class for Attachment
              i_attachment_subject  = im_attachment_subject    " Attachment Title
              i_attachment_size     = im_attachment_size    " Size of Document Content
              i_attachment_language = sy-langu    " Language in Which Attachment Is Created
              i_att_content_hex     = im_attachment
              i_attachment_header   = lc_obj_head->mt_objhead ).   " Content (Binary)
        CATCH cx_document_bcs.    "
      ENDTRY.
    ENDIF.

    TRY.
        lc_bcs->set_document( lc_doc ).
      CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
    ENDTRY.

****Subject***********************************************************
    TRY.
        lc_bcs->set_message_subject( im_subject  ).
      CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
    ENDTRY.

****Send**************************************************************
    TRY .
        lc_bcs->set_send_immediately( abap_true  ).
      CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
    ENDTRY.
    TRY .
        re_result = lc_bcs->send( ).
      CATCH cx_send_req_bcs INTO lc_send_req_bcs_ex.    "
    ENDTRY.
    IF im_do_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
