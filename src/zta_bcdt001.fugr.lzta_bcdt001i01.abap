*----------------------------------------------------------------------*
***INCLUDE LZTA_BCDT001I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_field INPUT.
  DATA: l_index TYPE sytabix, " Index to note the lines found
        l_tabix TYPE sytabix. " Index of total table

  DATA: l_matnr TYPE matnr.
  FIELD-SYMBOLS: <record> TYPE any. " Work area for table

* Modify changed by and changed on fields for any updates
  LOOP AT total.
    l_tabix = sy-tabix.
* Assign the table header line to the work area
    ASSIGN total TO <record>.
    IF sy-subrc = 0. " If assigning is successful.
* Check if data was changed and get the corresponding line index
      CASE <action>.
        WHEN 'N'. " New entry
          READ TABLE extract WITH KEY <vim_xtotal_key>. " Key field

          IF sy-subrc = 0.
* Clears the variables after updation.
            l_index = sy-tabix.

            <record>+205(12)  = sy-uname. " Created by
            <record>+217(8)   = sy-datum. " Created on
            <record>+225(6)   = sy-uzeit. " Created at

* Modify total table
            MODIFY total FROM <record> INDEX l_tabix.

            CHECK l_index > 0.
            extract = <record>.

* Modify extract table
            MODIFY extract INDEX l_index.
          ENDIF.

        WHEN 'U'. " Updated entry
          READ TABLE extract WITH KEY <vim_xtotal_key>. " Key field

          IF sy-subrc = 0.
* Clears the variables after updation.
            l_index = sy-tabix.

            IF <record>+205(12) IS INITIAL.
              <record>+205(12)  = sy-uname. " Created by
              <record>+217(8)   = sy-datum. " Created on
              <record>+225(6)   = sy-uzeit. " Created at
            ENDIF.
            <record>+231(12)  = sy-uname. " Changed by
            <record>+243(8)   = sy-datum. " Changed on
            <record>+251(6)   = sy-uzeit. " Changed at

* Modify total table
            MODIFY total FROM <record> INDEX l_tabix.

            CHECK l_index > 0.
            extract = <record>.

* Modify extract table
            MODIFY extract INDEX l_index.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.
  ENDLOOP.
ENDMODULE.
