FUNCTION zfm_export_template.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_OBJECT_NAME) TYPE  W3OBJID
*"     REFERENCE(I_FILENAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------
  DATA : ls_wwwdata_tab LIKE wwwdatatab.

  CLEAR: e_return.
*Make sure template exists in SMw0 as binary object
  SELECT * FROM wwwdata INNER JOIN tadir
                ON wwwdata~objid = tadir~obj_name
                INTO  CORRESPONDING FIELDS OF ls_wwwdata_tab UP TO 1 ROWS
                WHERE wwwdata~srtf2 = 0
                AND   wwwdata~relid = 'MI'
                AND   tadir~pgmid    = 'R3TR'
                AND   tadir~object   = 'W3MI'
                AND   tadir~obj_name = i_object_name.
  ENDSELECT.
  IF sy-subrc <> 0.
    e_return-type        = 'E'.
    e_return-id          = 'OO'.
    e_return-number      = '000'.
    e_return-message_v1  = 'Template'.
    e_return-message_v2  = i_object_name.
    e_return-message_v3  = 'does not exist in'.
    e_return-message_v4  = 'transaction SMW0'.

    EXIT.
  ENDIF.

  IF i_filename IS NOT INITIAL.
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = ls_wwwdata_tab
        destination = i_filename.
  ELSE.
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key = ls_wwwdata_tab.
  ENDIF.

ENDFUNCTION.
