class ZSAPLINK_VDAT definition
  public
  inheriting from ZSAPLINK
  create public .

public section.
  type-pools ABAP .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  class-data T_E071 type ZSAPLINK_TOOLS=>TY_T_E071 .
  class-data T_E071K type ZSAPLINK_TOOLS=>TY_T_E071K .
  class-data V_MODE type C .

  methods CREATEIXMLDOCFROMREQUEST
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods CREATEIXMLDOCFROMDATABASE
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
ENDCLASS.



CLASS ZSAPLINK_VDAT IMPLEMENTATION.


method CHECKEXISTS.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  exists = abap_false.  " No implementation of existence check

endmethod.


method CREATEIXMLDOCFROMDATABASE.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  TYPES: BEGIN OF ty_vdat,
           name     TYPE dd02v-tabname,
           activity TYPE e071-activity,
         END OF ty_vdat.

  TYPES: BEGIN OF ty_tabu,
           tabname  TYPE dd02v-tabname,
         END OF ty_tabu.

  DATA: lv_str     TYPE string,
        lv_objtype TYPE string,
        lv_objname TYPE ddobjname,
        lv_is_last TYPE abap_bool,

        lr_t_table  TYPE REF TO data,

        lt_dd26v TYPE STANDARD TABLE OF dd26v,

        lo_vdat_node    TYPE REF TO if_ixml_element,
        lo_tabu_node    TYPE REF TO if_ixml_element,
        lo_datarow_node TYPE REF TO if_ixml_element,
        ls_vdat         TYPE ty_vdat,
        ls_tabu         TYPE ty_tabu.

  FIELD-SYMBOLS: <fs_t_table> TYPE STANDARD TABLE,
                 <fs_s_table> TYPE any,
                 <fs_dd26v>   LIKE LINE OF lt_dd26v.

* Get the object type and name
  lv_objtype = me->getobjecttype( ).
  lv_objname = objname.

* Get the list of tables in View
  CALL FUNCTION 'DDIF_VIEW_GET'
    EXPORTING
      name          = lv_objname
      langu         = sy-langu
    TABLES
      dd26v_tab     = lt_dd26v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR lt_dd26v[] IS INITIAL.
    lv_str = `View does not exist in the active state`.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_str.
  ENDIF.

* Save data from each table only once in the XML document
  SORT lt_dd26v BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_dd26v COMPARING tabname.

  SORT lt_dd26v BY ddlanguage viewname tabname.

* Save contents of all View tables in XML document
  LOOP AT lt_dd26v ASSIGNING <fs_dd26v>.

    AT NEW viewname.

* Creates the VDAT root node
      lo_vdat_node = xmldoc->create_element( lv_objtype ).
      ls_vdat-name  = <fs_dd26v>-viewname.
      CLEAR ls_vdat-activity.
      me->setattributesfromstructure( node      = lo_vdat_node
                                      structure = ls_vdat  ).

    ENDAT.

    AT NEW tabname.

* Creates the TABU node
      lo_tabu_node = xmldoc->create_element( `TABU` ).
      ls_tabu-tabname  = <fs_dd26v>-tabname.
      me->setattributesfromstructure( node      = lo_tabu_node
                                      structure = ls_tabu  ).

      CREATE DATA lr_t_table TYPE STANDARD TABLE OF (ls_tabu-tabname).
      ASSIGN lr_t_table->* TO <fs_t_table>.

* Get data from table
      lv_is_last = abap_false.
      AT LAST.
        lv_is_last = abap_true.
      ENDAT.
      zsaplink_tools=>get_filtered_table( EXPORTING
                                            iv_vdat_tdat    = ls_vdat-name
                                            iv_is_tdat      = abap_false
                                            iv_tabname      = ls_tabu-tabname
                                            iv_is_last      = lv_is_last
                                          IMPORTING
                                            et_results      = <fs_t_table> ).

* Save data to XML document
      LOOP AT <fs_t_table> ASSIGNING <fs_s_table>.

* Creates the DataRow node
        lo_datarow_node = xmldoc->create_element( `DataRow` ).
        me->setattributesfromstructure( node      = lo_datarow_node
                                        structure = <fs_s_table>  ).
        lo_tabu_node->append_child( lo_datarow_node ).

      ENDLOOP.

      lo_vdat_node->append_child( lo_tabu_node ).

    ENDAT.

    AT END OF viewname.

      xmldoc->append_child( lo_vdat_node ).
      EXIT.

    ENDAT.

  ENDLOOP.

  ixmldocument = xmldoc.

endmethod.


method CREATEIXMLDOCFROMOBJECT.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

* Get all tasks of the request supplied in the calling program, if that's the case
  zsaplink_tools=>init_plugin( EXPORTING
                                 iv_mastertype = 'VDAT'
                               IMPORTING
                                 et_e071  = me->t_e071
                                 et_e071k = me->t_e071k
                               CHANGING
                                 cv_mode  = me->v_mode ).

  IF me->v_mode = zsaplink_tools=>c_mode_request.
    ixmldocument = me->createixmldocfromrequest( ).
  ELSEIF me->v_mode = zsaplink_tools=>c_mode_object.
    ixmldocument = me->createixmldocfromdatabase( ).
  ENDIF.

endmethod.


method CREATEIXMLDOCFROMREQUEST.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  TYPES: BEGIN OF ty_vdat,
           name     TYPE dd02v-tabname,
           activity TYPE e071-activity,
         END OF ty_vdat.

  TYPES: BEGIN OF ty_tabu,
           tabname  TYPE dd02v-tabname,
         END OF ty_tabu.

  DATA: lv_str           TYPE string,
        lv_objtype       TYPE string,
        lv_from          TYPE sy-tabix,
        lv_len           TYPE i,
        lv_complete_keys TYPE abap_bool,
        lv_charfields    TYPE string,
        lv_keyfields     TYPE string,
        lv_where         TYPE string,

        lr_t_table  TYPE REF TO data,
        lr_s_table  TYPE REF TO data,
        lr_tabkeys  TYPE REF TO data,
        lr_tabkeys2 TYPE REF TO data,

        lt_dd03p       TYPE STANDARD TABLE OF dd03p,
        lt_fields      TYPE cl_abap_structdescr=>component_table,
        lt_keyfields   TYPE abap_keydescr_tab,
        lt_charfields  TYPE abap_keydescr_tab,
        lt_sortfields  TYPE abap_sortorder_tab,

        lo_structdescr TYPE REF TO cl_abap_structdescr,
        lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
        lo_tabkeydescr TYPE REF TO cl_abap_typedescr,

        lo_vdat_node    TYPE REF TO if_ixml_element,
        lo_tabu_node    TYPE REF TO if_ixml_element,
        lo_datarow_node TYPE REF TO if_ixml_element,
        ls_vdat         TYPE ty_vdat,
        ls_tabu         TYPE ty_tabu.

  FIELD-SYMBOLS: <fs_e071>       LIKE LINE OF t_e071,
                 <fs_e071k>      LIKE LINE OF t_e071k,
                 <fs_t_table>    TYPE STANDARD TABLE,
                 <fs_s_table>    TYPE any,
                 <fs_t_tabkeys>  TYPE STANDARD TABLE,
                 <fs_t_tabkeys2> TYPE STANDARD TABLE,
                 <fs_s_tabkey>   TYPE any,
                 <fs_field>      LIKE LINE OF lt_fields,
                 <fs_dd03p>      LIKE LINE OF lt_dd03p,
                 <fs_sortfield>  LIKE LINE OF lt_sortfields,
                 <fs_keyfield>   LIKE LINE OF lt_keyfields.

* Get the object type
  lv_objtype = me->getobjecttype( ).

* Look for the first task activity not yet processed
  READ TABLE t_e071 ASSIGNING <fs_e071>
    WITH KEY obj_name = objname
             used     = space
    BINARY SEARCH.

  IF sy-subrc <> 0.
    lv_str = `No more object(s) of this type found in the request(s) supplied`.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = lv_str.
  ENDIF.

* Position on the first key of the object being processed
  READ TABLE t_e071k TRANSPORTING NO FIELDS WITH KEY trkorr     = <fs_e071>-trkorr
                                                     activity   = <fs_e071>-activity
                                                     mastername = <fs_e071>-obj_name.

  IF sy-subrc = 0.

    lv_from = sy-tabix.

* Process all keys belonging to current object
    LOOP AT t_e071k ASSIGNING <fs_e071k> FROM lv_from.

      AT NEW mastername.

* Creates the VDAT root node
        lo_vdat_node = xmldoc->create_element( lv_objtype ).
        ls_vdat-name  = objname.
        ls_vdat-activity = <fs_e071>-activity.
        me->setattributesfromstructure( node      = lo_vdat_node
                                        structure = ls_vdat  ).

      ENDAT.

      AT NEW objname.

* Get information about all fields of the table
        REFRESH lt_dd03p.
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = <fs_e071k>-objname
            langu         = sy-langu
          TABLES
            dd03p_tab     = lt_dd03p
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        IF sy-subrc <> 0 OR lt_dd03p[] IS INITIAL.
          lv_str = `Table does not exist in the active state`.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_str.
        ENDIF.

* Create the type object for field E071K-TABKEY
        lo_tabkeydescr = cl_abap_typedescr=>describe_by_name( `E071K-TABKEY` ).

* Loop thru all key fields of the table:
*   1) Building a list of all key fields
*   2) Building a list of all CHARLIKE key fields that fit fields E071K-TABKEY
*   3) Building the WHERE clause for the SELECT to be used for data retrieval, using FOR ALL ENTRIES
*   4) Setting up table LT_FIELDS, with all key fields
        CLEAR: lv_keyfields, lv_charfields, lv_where.
        lv_complete_keys = abap_true.
        REFRESH lt_fields.
        LOOP AT lt_dd03p ASSIGNING <fs_dd03p>
          WHERE keyflag = abap_true AND
                fieldname(1) <> '.'.

* Build the list of all key fields names
          CONCATENATE lv_keyfields <fs_dd03p>-fieldname INTO lv_keyfields
            SEPARATED BY space.

* Add key field to the key type strucuture of the object keys table
          APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_field>.
          <fs_field>-name = <fs_dd03p>-fieldname.
          <fs_field>-type ?= cl_abap_structdescr=>describe_by_name( <fs_dd03p>-rollname ).

          CHECK lv_complete_keys = abap_true.

* Check if structure containing all key fields of the View table is CHARLIKE
          IF <fs_field>-type->type_kind CN 'CNDT'.
            lv_complete_keys = abap_false.
            CONTINUE.
          ENDIF.

* Build the type object for key fields found so far, to help answer the following question below
          lo_structdescr = cl_abap_structdescr=>create( lt_fields ).

* If I add current CHARLIKE key field to key structure,
*   will it make the CHARLIKE key structure larger than field E071K-TABKEY ?
          IF lo_structdescr->length > lo_tabkeydescr->length.
            lv_complete_keys = abap_false.
            CONTINUE.
          ENDIF.

* Build the list of CHARLIKE key fields names
          CONCATENATE lv_charfields <fs_dd03p>-fieldname INTO lv_charfields
            SEPARATED BY space.

* Build the 'FIELDNAME = <FS_T_TAKEYS>-FIELDNAME' WHERE condition
          CONCATENATE '<FS_T_TABKEYS>-' <fs_dd03p>-fieldname INTO lv_str.
          CONCATENATE lv_where 'AND' <fs_dd03p>-fieldname '=' lv_str INTO lv_where SEPARATED BY space.

        ENDLOOP.

* Table has no key fields (?!?)
        IF sy-subrc <> 0.
          lv_str = `Table has no key fields`.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_str.
        ENDIF.

* Get rid of the " " at the beginning of key fuields list
        SHIFT lv_keyfields LEFT BY 1 PLACES.

* List of table key fields
        SPLIT lv_keyfields AT space INTO TABLE lt_keyfields.

* Get rid of the " " at the beginning of CHAR key fields list
        SHIFT lv_charfields LEFT BY 1 PLACES.

* List of table CHARLIKE key fields
        SPLIT lv_charfields AT space INTO TABLE lt_charfields.

* Get rid of the " AND " at the beginning of the WHERE clause
        SHIFT lv_where LEFT BY 5 PLACES.

* Creates the dynamic table with key fields of the data table
        lo_structdescr = cl_abap_structdescr=>create( lt_fields ).
        lo_tabledescr = cl_abap_tabledescr=>create( p_line_type = lo_structdescr
                                                    p_key       = lt_keyfields ).
        CREATE DATA lr_tabkeys TYPE HANDLE lo_tabledescr.
        ASSIGN lr_tabkeys->* TO <fs_t_tabkeys>.

      ENDAT.

* If we are not storing complete keys, get rid of the "*" at the end of key
      CLEAR lv_str.
      lv_len = strlen( <fs_e071k>-tabkey ) - 1.
      IF lv_len >= 0.
        IF lv_complete_keys = abap_false AND
           <fs_e071k>-tabkey+lv_len(1) = '*'.
          lv_str = <fs_e071k>-tabkey(lv_len).
        ELSE.
          lv_str = <fs_e071k>-tabkey.
        ENDIF.
      ENDIF.

* Creates a new key in the keys table
      APPEND INITIAL LINE TO <fs_t_tabkeys> ASSIGNING <fs_s_tabkey>.
      <fs_s_tabkey> = lv_str.

      AT END OF objname.

* Get rid of duplicate keys in the keys table
        SORT <fs_t_tabkeys> BY table_line.
        IF lv_complete_keys = abap_true.
          DELETE ADJACENT DUPLICATES FROM <fs_t_tabkeys> COMPARING ALL FIELDS.
        ELSEIF lt_charfields[] IS NOT INITIAL.

* Create a table with same fields as LT_TABKEYS, but with CHAR key fields
          lo_tabledescr = cl_abap_tabledescr=>create( p_line_type = lo_structdescr
                                                      p_key       = lt_charfields ).
          CREATE DATA lr_tabkeys2 TYPE HANDLE lo_tabledescr.
          ASSIGN lr_tabkeys2->* TO <fs_t_tabkeys2>.

* Get rid of all CHARLIKE key duplicates
          <fs_t_tabkeys2>[] = <fs_t_tabkeys>[].
          DELETE ADJACENT DUPLICATES FROM <fs_t_tabkeys2>.
          <fs_t_tabkeys>[] = <fs_t_tabkeys2>[].

        ENDIF.

* Only reuse the keys table as data table if data table fields are all key fields and
*   if data table allows storing complete keys in the request
        IF lines( lt_dd03p ) <> lines( lt_keyfields ) OR
           lv_complete_keys = abap_false.

* If not, then create the table that will contain the records to be written
          CREATE DATA lr_t_table TYPE STANDARD TABLE OF (<fs_e071k>-objname) WITH KEY (lt_keyfields).
          ASSIGN lr_t_table->* TO <fs_t_table>.

* And select the desired records from database using CHARLIKE keys table <FS_T_TABKEYS> as the key
          IF <fs_t_tabkeys>[] IS NOT INITIAL.
            SELECT  *
              INTO  TABLE <fs_t_table>
              FROM  (<fs_e071k>-objname)
              CLIENT SPECIFIED
              FOR ALL ENTRIES IN <fs_t_tabkeys>
              WHERE (lv_where).
          ENDIF.

        ELSE.

* If yes, the table <fs_t_tabkeys> already contains all fields that should be written
          ASSIGN <fs_t_tabkeys> TO <fs_t_table>.

        ENDIF.

        IF <fs_t_table>[] IS NOT INITIAL.

* Get rid of duplicate entries in the data table
          REFRESH lt_sortfields.
          LOOP AT lt_keyfields ASSIGNING <fs_keyfield>.
            APPEND INITIAL LINE TO lt_sortfields ASSIGNING <fs_sortfield>.
            <fs_sortfield>-name = <fs_keyfield>.
          ENDLOOP.
          SORT <fs_t_table> BY (lt_sortfields).
          DELETE ADJACENT DUPLICATES FROM <fs_t_table>.

* Creates the TABU node containing the table name
          lo_tabu_node = xmldoc->create_element( 'TABU' ).
          ls_tabu-tabname = <fs_e071k>-objname.
          me->setattributesfromstructure( node      = lo_tabu_node
                                          structure = ls_tabu  ).

* Writes all records to the XML document
          LOOP AT <fs_t_table> ASSIGNING <fs_s_table>.

            lo_datarow_node = xmldoc->create_element( `DataRow` ).
            me->setattributesfromstructure( node = lo_datarow_node structure = <fs_s_table> ).
            lo_tabu_node->append_child( lo_datarow_node ).

          ENDLOOP.

* Inserts the TABU node into the XML document
          lo_vdat_node->append_child( lo_tabu_node ).

        ENDIF.

      ENDAT.

* If it's the last key for current object, exit
      AT END OF mastername.

* Inserts the VDAT node into the XML document
        xmldoc->append_child( lo_vdat_node ).
        EXIT.

      ENDAT.

    ENDLOOP.

  ENDIF.

  <fs_e071>-used = abap_true.

  SORT t_e071 BY obj_name used trkorr activity.

  ixmldocument = xmldoc.

endmethod.


method CREATEOBJECTFROMIXMLDOC.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  TYPES: BEGIN OF ty_vdat,
           name     TYPE dd02l-tabname,
           activity TYPE e071-activity,
         END OF ty_vdat,

         BEGIN OF ty_tabu,
           tabname TYPE dd02l-tabname,
         END OF ty_tabu,

         BEGIN OF ty_entry,
           tabname TYPE dd02l-tabname,
           table   TYPE REF TO data,
         END OF ty_entry.

  DATA: lo_vdat_node TYPE REF TO if_ixml_element,

        lo_tabu_node     TYPE REF TO if_ixml_element,
        lo_tabu_filter   TYPE REF TO if_ixml_node_filter,
        lo_tabu_iterator TYPE REF TO if_ixml_node_iterator,

        lo_datarow_node     TYPE REF TO if_ixml_element,
        lo_datarow_filter   TYPE REF TO if_ixml_node_filter,
        lo_datarow_iterator TYPE REF TO if_ixml_node_iterator,

        lt_dd03p TYPE STANDARD TABLE OF dd03p,

        ls_vdat TYPE ty_vdat,
        ls_tabu TYPE ty_tabu,

        lv_objtype TYPE string,

        lv_clidep TYPE abap_bool,

        lv_num_keyfields TYPE i,

        lr_table   TYPE REF TO data,
        lr_table_2 LIKE lr_table,
        lr_struct  TYPE REF TO data,

        ls_ko200      TYPE ko200,
        lt_e071k      TYPE tredt_keys,
        lt_fields     TYPE cl_abap_structdescr=>component_table,
        lt_keyfields  TYPE STANDARD TABLE OF dd03p-fieldname,
        lt_sortfields TYPE abap_sortorder_tab,
        lt_entries    TYPE STANDARD TABLE OF ty_entry,

        lo_structdescr TYPE REF TO cl_abap_structdescr,
        lo_tabkeydescr TYPE REF TO cl_abap_typedescr,
        lo_typedescr   TYPE REF TO cl_abap_typedescr,

        lv_keyfields     TYPE string,
        lv_where         TYPE string,
        lv_complete_keys TYPE abap_bool,
        lv_tabix         TYPE sy-tabix,
        lv_str           TYPE string.

  FIELD-SYMBOLS: <fs_t_table>    TYPE STANDARD TABLE,
                 <fs_t_table_2>  LIKE <fs_t_table>,
                 <fs_s_table>    TYPE any,
                 <fs_dd03p>      LIKE LINE OF lt_dd03p,
                 <fs_e071k>      LIKE LINE OF lt_e071k,
                 <fs_fields>     LIKE LINE OF lt_fields,
                 <fs_keyfields>  LIKE LINE OF lt_keyfields,
                 <fs_sortfields> LIKE LINE OF lt_sortfields,
                 <fs_entries>    LIKE LINE OF lt_entries,
                 <fs_struct>     TYPE any,
                 <fs_mandt>      TYPE mandt.

* Get object type
  lv_objtype = getobjecttype( ).

* Save the XML document object
  xmldoc = ixmldocument.

* Get root node for VDAT object
  lo_vdat_node = xmldoc->find_from_name( lv_objtype ).

* Get View name from XML
  me->getstructurefromattributes(
          EXPORTING  node      = lo_vdat_node
          CHANGING   structure = ls_vdat ).

* Returns the name of the object
  name = ls_vdat-name.

* Create the filter to loop thru all TABU entries in document
  lo_tabu_filter = lo_vdat_node->create_filter_name( `TABU` ).

* Get an iterator to be used to cycle thru all occurrencies of TABU entries
  lo_tabu_iterator = lo_vdat_node->create_iterator_filtered( lo_tabu_filter ).

* Loop thru all tables (TABU entries) that make up the View
  lo_tabu_node ?= lo_tabu_iterator->get_next( ).
  WHILE lo_tabu_node IS BOUND.

* Get table name from XML
    me->getstructurefromattributes(
            EXPORTING  node      = lo_tabu_node
            CHANGING   structure = ls_tabu ).

* Check that table exists
    REFRESH lt_dd03p.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = ls_tabu-tabname
        langu         = sy-langu
      TABLES
        dd03p_tab     = lt_dd03p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR lt_dd03p[] IS INITIAL.
      lv_str = `Table does not exist in the active state`.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lv_str.
    ENDIF.

* Count number of key fields
    LOOP AT lt_dd03p TRANSPORTING NO FIELDS WHERE keyflag = abap_true.
      ADD  1 TO lv_num_keyfields.
    ENDLOOP.

* Create the type object for field E071K-TABKEY
    lo_tabkeydescr = cl_abap_typedescr=>describe_by_name( `E071K-TABKEY` ).

* Build a structure type containing all key fields of the View table
    DELETE lt_dd03p WHERE keyflag      = abap_false OR
                          fieldname(1) = '.'.
    CLEAR: lv_keyfields, lv_where.
    lv_clidep = abap_false.
    lv_complete_keys = abap_true.
    REFRESH lt_fields.
    LOOP AT lt_dd03p ASSIGNING <fs_dd03p>.

      AT FIRST.

* Check if the View table is a client dependent table:
*   First table field must be a key field of type CLNT
        IF <fs_dd03p>-datatype = 'CLNT' AND
           lv_num_keyfields <> 1.
          lv_clidep = abap_true.
        ENDIF.

      ENDAT.

* Build the key fields list
      CONCATENATE lv_keyfields <fs_dd03p>-fieldname INTO lv_keyfields
        SEPARATED BY space.

* Build the WHERE clause used to retrieve entries from database
      CONCATENATE `<FS_T_TABLE>-` <fs_dd03p>-fieldname INTO lv_str.
      CONCATENATE lv_where `AND` <fs_dd03p>-fieldname `=` lv_str INTO lv_where
        SEPARATED BY space.

      CHECK lv_complete_keys = abap_true.

      lo_typedescr = cl_abap_typedescr=>describe_by_name( <fs_dd03p>-rollname ).

* Only CHARLIKE key fields are accepted
      IF lo_typedescr->type_kind CN 'CNDT'.
        lv_complete_keys = abap_false.
        CONTINUE.
      ENDIF.

* One more CHARLIKE key field found
      APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
      <fs_fields>-name = <fs_dd03p>-fieldname.
      <fs_fields>-type ?= lo_typedescr.

* Build the type object for key fields found so far, to help answer the following question below
      lo_structdescr = cl_abap_structdescr=>create( lt_fields ).

* If I add current CHARLIKE key field to key structure,
*   will it make the CHARLIKE key structure larger than field E071K-TABKEY ?
      IF lo_structdescr->length > lo_tabkeydescr->length.

* CHARLIKE key field just added will make CHARLIKE key structure not fit field E071K-TABKEY, so delete it
        lv_tabix = lines( lt_fields ).
        DELETE lt_fields INDEX lv_tabix.

* Rebuild the type object for key fields, excluding current CHARLIKE key field that did not fit
        lo_structdescr = cl_abap_structdescr=>create( lt_fields ).

        lv_complete_keys = abap_false.
        CONTINUE.

      ENDIF.

    ENDLOOP.

* Error if View table has no key fields (?!?)
    IF sy-subrc <> 0.
      lv_str = `Table has no key fields`.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lv_str.
    ENDIF.

* Get rid of the " " at the beginning of the list of key fields of View table
    SHIFT lv_keyfields LEFT BY 1 PLACES.

* Build a list of all View table key fields
    SPLIT lv_keyfields AT space INTO TABLE lt_keyfields.

* Get rid of the " AND " at the beginning of the WHERE clause
    SHIFT lv_where LEFT BY 5 PLACES.

* Create a work area to store all CHARLIKE key fields of the View table that fit field E070K-TABKEY
    CREATE DATA lr_struct TYPE HANDLE lo_structdescr.
    ASSIGN lr_struct->* TO <fs_struct>.

* Create the View table using the CHARLIKE key. This will allow us to delete adjacent duplicates from it
*   using a dynamically COMPARING addition
    CREATE DATA lr_table TYPE STANDARD TABLE OF (ls_tabu-tabname) WITH KEY (lt_keyfields).
    ASSIGN lr_table->* TO <fs_t_table>.

* Create a table to contain existing data from database
    CREATE DATA lr_table_2 LIKE <fs_t_table>.
    ASSIGN lr_table_2->* TO <fs_t_table_2>.

* Create the filter to loop thru all DataRow entries in document for the View table
    lo_datarow_filter = lo_tabu_node->create_filter_name( `DataRow` ).

* Get an iterator to be used to cycle thru all occurrencies of DataRow entries
    lo_datarow_iterator = lo_tabu_node->create_iterator_filtered( lo_datarow_filter ).

* Collect all DataRow entries of current View table
    lo_datarow_node ?= lo_datarow_iterator->get_next( ).
    WHILE lo_datarow_node IS BOUND.

* Append contents of each DataRow entry to the View table
      APPEND INITIAL LINE TO <fs_t_table> ASSIGNING <fs_s_table>.

      me->getstructurefromattributes(
              EXPORTING   node      = lo_datarow_node
              CHANGING    structure = <fs_s_table> ).

* If it's a client dependent table, replace CLNT key field with logged on client
      IF lv_clidep = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE <fs_s_table> TO <fs_mandt>.
        <fs_mandt> = sy-mandt.
      ENDIF.

* Build table key to be stored in the object keys table
      <fs_struct> = <fs_s_table>.

* Build the table key for each DataRow entry
      APPEND INITIAL LINE TO lt_e071k ASSIGNING <fs_e071k>.
      <fs_e071k>-pgmid      = 'R3TR'.
      <fs_e071k>-object     = 'TABU'.
      <fs_e071k>-objname    = ls_tabu-tabname.
      <fs_e071k>-mastertype = 'VDAT'.
      <fs_e071k>-mastername = ls_vdat-name.
      <fs_e071k>-viewname   = ls_vdat-name.
      <fs_e071k>-tabkey     = <fs_struct>.
      <fs_e071k>-sortflag   = '2'.
      <fs_e071k>-activity   = ls_vdat-activity.

* Get next DataRow entry
      lo_datarow_node ?= lo_datarow_iterator->get_next( ).

    ENDWHILE.

    IF <fs_t_table>[] IS NOT INITIAL.

* Check if any of the entries to be inserted already exists in database
      SELECT  (lv_keyfields)
        FROM  (ls_tabu-tabname)
        INTO  TABLE <fs_t_table_2>
        FOR ALL ENTRIES IN <fs_t_table>
        WHERE (lv_where).

* At least one table entry already exists in datrabase
      IF sy-subrc = 0.

* Overwrite existing records or write only the records that does not exist in database
*   depending on the "Overwrite Originals" flag of SAPLINK selection screen
        IF overwrite = abap_true.
          DELETE (ls_tabu-tabname) FROM TABLE <fs_t_table_2>.
        ELSE.

* If no overwrite is desired, delete all existing entries from the
*   View table and also from the CHARLIKE key fields table
          LOOP AT <fs_t_table_2> ASSIGNING <fs_s_table>.

* Remove entry from View table
            DELETE TABLE <fs_t_table> FROM <fs_s_table>.

* Also remove entry from the object keys table
            <fs_struct> = <fs_s_table>.
            READ TABLE lt_e071k TRANSPORTING NO FIELDS
              WITH KEY objname    = ls_tabu-tabname
                       viewname   = ls_vdat-name
                       mastername = ls_vdat-name
                       tabkey     = <fs_struct>.
            IF sy-subrc = 0.
              DELETE lt_e071k INDEX sy-tabix.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

* Store all entries that will be written to database later
    IF <fs_t_table>[] IS NOT INITIAL.

* Get rid of duplicate keys in the View table
      REFRESH lt_sortfields.
      LOOP AT lt_keyfields ASSIGNING <fs_keyfields>.
        APPEND INITIAL LINE TO lt_sortfields ASSIGNING <fs_sortfields>.
        <fs_sortfields>-name = <fs_keyfields>.
      ENDLOOP.
      SORT <fs_t_table> BY (lt_sortfields).
      DELETE ADJACENT DUPLICATES FROM <fs_t_table>.

      APPEND INITIAL LINE TO lt_entries ASSIGNING <fs_entries>.
      <fs_entries>-tabname  = ls_tabu-tabname.
      CREATE DATA <fs_entries>-table LIKE <fs_t_table>.
      ASSIGN <fs_entries>-table->* TO <fs_t_table_2>.
      <fs_t_table_2>[] =  <fs_t_table>[].

    ENDIF.

* If not storing complete keys, append an "*" at the end of every key entry
    IF lv_complete_keys = abap_false.
      LOOP AT lt_e071k ASSIGNING <fs_e071k>
        WHERE objname    = ls_tabu-tabname AND
              viewname   = ls_vdat-name AND
              mastername = ls_vdat-name.
        CONCATENATE <fs_e071k>-tabkey '*' INTO <fs_e071k>-tabkey.
      ENDLOOP.
    ENDIF.

* Get next TABU entry
    lo_tabu_node ?= lo_tabu_iterator->get_next( ).

  ENDWHILE.

  IF lt_entries[] IS NOT INITIAL.

* Get rid of duplicate object keys
    SORT lt_e071k BY mastername objname tabkey.
    DELETE ADJACENT DUPLICATES FROM lt_e071k COMPARING mastername objname tabkey.

* Attach inserted entries to a request
    ls_ko200-pgmid     = 'R3TR'.
    ls_ko200-object    = lv_objtype.
    ls_ko200-obj_name  = ls_vdat-name.
    ls_ko200-objfunc   = 'K'.
    ls_ko200-activity  = ls_vdat-activity.
    ls_ko200-operation = 'I'.

    IF zsaplink_tools=>add_to_request( is_ko200  = ls_ko200
                                       it_e071k  = lt_e071k ) <> 0.
      lv_str = `Error while attaching table entries to the request`.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lv_str.
    ENDIF.

* Insert all selected records into database
    LOOP AT lt_entries ASSIGNING <fs_entries>.
      ASSIGN <fs_entries>-table->* TO <fs_t_table_2>.
      INSERT (<fs_entries>-tabname) FROM TABLE <fs_t_table_2>.
    ENDLOOP.

  ENDIF.

endmethod.


method DELETEOBJECT.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  DATA: v_msg TYPE string.

  v_msg = `Operation not implemented`.
  RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING
      textid = zcx_saplink=>error_message
      msg = v_msg.

endmethod.


method GETOBJECTTYPE.
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


* Plugin developed by Lino Lopes (lino2112@gmail.com)

  objecttype = 'VDAT'.  "Selected view contents

endmethod.
ENDCLASS.
