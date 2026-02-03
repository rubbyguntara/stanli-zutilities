class ZSAPLINK_TRANSACTIONS definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

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

  types:
    BEGIN OF ty_header,
           tcode       TYPE tstc-tcode,
           pgmna       TYPE tstc-pgmna,
           dypno       TYPE swydynp-dynpro,
           tcode_call  TYPE tstc-tcode,
           type        TYPE c LENGTH 1,
           skip_screen TYPE c LENGTH 1,
           s_webgui    TYPE tstcc-s_webgui,
           s_win32     TYPE tstcc-s_win32,
           s_platin    TYPE tstcc-s_platin,
           masterlang  TYPE tadir-masterlang,
           ttext       TYPE tstct-ttext,
       END OF ty_header .

  constants C_HEX_TRA type X value '00' ##NO_TEXT.
  constants C_HEX_PAR type X value '02' ##NO_TEXT.
  constants C_HEX_REP type X value '80' ##NO_TEXT.
  constants C_HEX_OBJ type X value '08' ##NO_TEXT.

  methods CONVERT_TSTCP_PARAM
    importing
      !IV_PARAM type TSTCP-PARAM
    returning
      value(RE_PARAM) type SWYPARAM .
  methods GET_TCODE_FROM_PARAM
    importing
      !IV_PARAM type TSTCP-PARAM
    returning
      value(RE_TCODE) type TSTC-TCODE .
ENDCLASS.



CLASS ZSAPLINK_TRANSACTIONS IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                         |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Juan Sebastián Soto
*      sebastiansoto@freeit.com.ar

  DATA: lv_objectname TYPE tstc-tcode.

  lv_objectname = me->objname.

  CALL FUNCTION 'RS_TRANSACTION_CHECK'
    EXPORTING
      objectname       = lv_objectname
      suppress_dialog  = abap_true
    EXCEPTIONS
      object_not_found = 1
      error_occured    = 2
      OTHERS           = 3.

  CHECK sy-subrc EQ 0.

  exists = abap_true.

ENDMETHOD.


METHOD convert_tstcp_param.

  DATA: lt_param_list   TYPE stringtab.

  DATA: lw_param        TYPE LINE OF swyparam.

  DATA: lv_garbage      TYPE c LENGTH 1,
        lv_params       TYPE c LENGTH 240,
        lv_param_list   TYPE string.

  SPLIT iv_param AT ' ' INTO lv_garbage lv_params IN CHARACTER MODE.

  SPLIT lv_params AT ';' INTO TABLE lt_param_list IN CHARACTER MODE.

  LOOP AT lt_param_list INTO lv_param_list.

    CLEAR lw_param.

    SPLIT lv_param_list AT '=' INTO lw_param-field lw_param-value.
    APPEND lw_param TO re_param.

  ENDLOOP.

ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                         |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Juan Sebastián Soto
*      sebastiansoto@freeit.com.ar

  DATA: lt_tcodes         TYPE STANDARD TABLE OF tstc,
        lt_gui_attributes TYPE STANDARD TABLE OF tstcc,
        lt_params         TYPE swyparam.

  DATA: ls_header         TYPE ty_header,
        ls_tadir          TYPE tadir,
        ls_tcodes         TYPE tstc,
        ls_gui_attributes TYPE tstcc,
        ls_tstct          TYPE tstct,
        ls_tstcp          TYPE tstcp,
        ls_params         TYPE LINE OF swyparam.

  DATA: lv_transaction TYPE tstc-tcode,
        lv_object      TYPE tadir-object,
        lv_obj_name    TYPE tadir-obj_name,
        lv_message     TYPE string.

*xml nodes
  DATA: _objtype   TYPE string,
        rootnode    TYPE REF TO if_ixml_element,
        header_node TYPE REF TO if_ixml_element,
        line_node   TYPE REF TO if_ixml_element,
        rc          TYPE sysubrc.

  lv_transaction = me->objname.
  lv_obj_name    = me->objname.

  _objtype  = me->getobjecttype( ).
  lv_object = me->getobjecttype( ).

* Read Master System Data
  CALL FUNCTION 'RM_TADIR_READ'
    EXPORTING
      p_object        = lv_object
      p_obj_name      = lv_obj_name
    IMPORTING
      e_tadir         = ls_tadir
    EXCEPTIONS
      interface_error = 1
      OTHERS          = 2.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING
      textid = zcx_saplink=>system_error.
  ENDIF.

* Read Transaction Header Data
  CALL FUNCTION 'RPY_TRANSACTION_READ'
    EXPORTING
      transaction      = lv_transaction
    TABLES
      tcodes           = lt_tcodes
      gui_attributes   = lt_gui_attributes
    EXCEPTIONS
      permission_error = 1
      cancelled        = 2
      not_found        = 3
      object_not_found = 4
      OTHERS           = 5.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING
      textid = zcx_saplink=>system_error.
  ENDIF.

* Get Default Description
  SELECT SINGLE *
  FROM tstct
  INTO ls_tstct
  WHERE sprsl EQ ls_tadir-masterlang
    AND tcode EQ lv_transaction.

* Compile Header
  READ TABLE lt_tcodes         INTO ls_tcodes INDEX 1.
  READ TABLE lt_gui_attributes INTO ls_gui_attributes INDEX 1.

  MOVE-CORRESPONDING ls_tcodes TO ls_header.
  MOVE-CORRESPONDING ls_gui_attributes TO ls_header.
  ls_header-masterlang = ls_tadir-masterlang.
  ls_header-ttext      = ls_tstct-ttext.

  CASE ls_tcodes-cinfo.
    WHEN c_hex_tra.
      ls_header-type = 'D'.
    WHEN c_hex_par.
      ls_header-type = 'P'.
    WHEN c_hex_rep.
      ls_header-type = 'R'.
    WHEN OTHERS.

      lv_message = text-001.

      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lv_message.

  ENDCASE.

* Read Transaction parameters if needed.
  IF ls_tcodes-cinfo EQ c_hex_par.
    SELECT SINGLE * FROM tstcp INTO ls_tstcp WHERE tcode EQ lv_transaction.

    IF ls_tstcp-param+1(1) EQ '*'. " Do skip initial screen.
      ls_header-skip_screen = abap_true.
    ENDIF.

    lt_params = me->convert_tstcp_param( ls_tstcp-param ).

    ls_header-tcode_call = me->get_tcode_from_param( ls_tstcp-param ).

  ENDIF.

* Create parent node
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  setattributesfromstructure( node = rootnode structure = ls_header ).

* Create param subnode
  LOOP AT lt_params INTO ls_params.
    line_node = xmldoc->create_element( 'params' ).
    setattributesfromstructure( node = line_node structure = ls_params ).
    rc = rootnode->append_child( line_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.

ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                         |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Juan Sebastián Soto
*      sebastiansoto@freeit.com.ar

  DATA: lt_params         TYPE swyparam.

  DATA: ls_header    TYPE ty_header,
        ls_params    TYPE LINE OF swyparam.

*xml nodes
  DATA: rootnode     TYPE REF TO if_ixml_element,
        line_node    TYPE REF TO if_ixml_element,
        node         TYPE REF TO if_ixml_element,
        filter       TYPE REF TO if_ixml_node_filter,
        iterator     TYPE REF TO if_ixml_node_iterator,
        _objtype     TYPE string.

  DATA: trobjtype  TYPE trobjtype,
        trobj_name TYPE trobj_name.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ls_header.

  IF checkexists( ) EQ abap_true.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

* retrieve standard text lines
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'params' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR line_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_params.
    APPEND ls_params TO lt_params.
    node ?= iterator->get_next( ).
  ENDWHILE.

* Create object
  CASE ls_header-type.
    WHEN 'D'. " Dialog

      CALL FUNCTION 'SWY_CREATE_DIALOG_TRANSACTION'
        EXPORTING
          new_tcode                     = ls_header-tcode
          text                          = ls_header-ttext
          program                       = ls_header-pgmna
          dynpro                        = ls_header-dypno
          enabled_html                  = ls_header-s_webgui
          enabled_java                  = ls_header-s_platin
          enabled_wingui                = ls_header-s_win32
          devclass                      = devclass
        EXCEPTIONS
          transaction_could_not_created = 1
          OTHERS                        = 2.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
      ENDIF.

    WHEN 'R'. " Report

      CALL FUNCTION 'RPY_TRANSACTION_INSERT'
        EXPORTING
          transaction                   = ls_header-tcode
          program                       = ls_header-pgmna
          dynpro                        = ls_header-dypno
          language                      = ls_header-masterlang
          development_class             = devclass
          transaction_type              = 'R'
          shorttext                     = ls_header-ttext
          professionel_user_transaction = abap_true
          html_enabled                  = ls_header-s_webgui
          java_enabled                  = ls_header-s_platin
          wingui_enabled                = ls_header-s_win32
        EXCEPTIONS
          cancelled                     = 1
          already_exist                 = 2
          permission_error              = 3
          name_not_allowed              = 4
          name_conflict                 = 5
          illegal_type                  = 6
          object_inconsistent           = 7
          db_access_error               = 8
          OTHERS                        = 9.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
      ENDIF.

    WHEN 'P'. " Params

      CALL FUNCTION 'SWY_CREATE_PARAM_TRANSACTION'
        EXPORTING
          new_tcode                     = ls_header-tcode
          text                          = ls_header-ttext
          called_transaction            = ls_header-tcode_call
          transaction_easy_web          = abap_false
          transaction_complex           = abap_true
          enabled_html                  = ls_header-s_webgui
          enabled_java                  = ls_header-s_platin
          enabled_wingui                = ls_header-s_win32
          parameter                     = lt_params
          devclass                      = devclass
        EXCEPTIONS
          transaction_could_not_created = 1
          OTHERS                        = 2.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
      ENDIF.

  ENDCASE.

ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                         |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/


*      Plugin created by:
*      Juan Sebastián Soto
*      elsebasoto@gmail.com

  DATA: lv_trobjtype   TYPE trobjtype,
        lv_trobj_name  TYPE trobj_name,
        lv_transaction TYPE tstc-tcode.

  lv_transaction = me->objname.

  CALL FUNCTION 'RPY_TRANSACTION_DELETE'
    EXPORTING
      transaction      = lv_transaction
    EXCEPTIONS
      not_excecuted    = 1
      object_not_found = 2
      OTHERS           = 3.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING
      textid = zcx_saplink=>system_error.
  ENDIF.

ENDMETHOD.


METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                         |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Juan Sebastián Soto
*      elsebasoto@gmail.com

  objecttype = 'TRAN'.  "Table Type

ENDMETHOD.


METHOD GET_TCODE_FROM_PARAM.

  DATA: lv_garbage      TYPE c LENGTH 1,
        lv_params       TYPE c LENGTH 240.

  SPLIT iv_param AT ' ' INTO lv_params lv_garbage IN CHARACTER MODE.

  re_tcode = lv_params+2.

ENDMETHOD.
ENDCLASS.
