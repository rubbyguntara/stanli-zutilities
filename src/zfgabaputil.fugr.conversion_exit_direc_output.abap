FUNCTION CONVERSION_EXIT_DIREC_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
DATA : lv_domname TYPE dd07v-domname,
         lv_value   TYPE dd07v-domvalue_l,
         lv_text    TYPE dd07v-ddtext.

  output = input.

  IF input <> space.
    lv_domname  = 'ZDO_IF_DIRECT'.
    lv_value    = input.

    CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
      EXPORTING
        name      = lv_domname
        spras     = sy-langu
        value     = lv_value
      IMPORTING
        text      = lv_text
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      CLEAR output.
    ELSE.
      output = lv_text.
    ENDIF.

  ENDIF.


ENDFUNCTION.
