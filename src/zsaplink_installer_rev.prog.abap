*&---------------------------------------------------------------------*
*& Report ZSAPLINK_INSTALLER_REV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
REPORT  zsaplink_installer_rev.


CLASS zcx_saplink DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC
  .
  PUBLIC SECTION.

    CONSTANTS error_message TYPE sotr_conc VALUE '005056C000081ED696E10D9EF345B892'. "#EC NOTEXT
    CONSTANTS existing TYPE sotr_conc VALUE '005056C000081ED696E10D9EF345D892'. "#EC NOTEXT
    CONSTANTS incorrect_file_format TYPE sotr_conc VALUE '005056C000081ED696E10D9EF345F892'. "#EC NOTEXT
    CONSTANTS locked TYPE sotr_conc VALUE '005056C000081ED696E10D9EF3461892'. "#EC NOTEXT
    DATA msg TYPE string VALUE '44F7518323DB08BC02000000A7E42BB6'. "#EC NOTEXT
    CONSTANTS not_authorized TYPE sotr_conc VALUE '005056C000081ED696E10D9EF3463892'. "#EC NOTEXT
    CONSTANTS not_found TYPE sotr_conc VALUE '005056C000081ED696E10D9EF3465892'. "#EC NOTEXT
    CONSTANTS no_plugin TYPE sotr_conc VALUE '005056C000081ED696E10D9EF3467892'. "#EC NOTEXT
    CONSTANTS system_error TYPE sotr_conc VALUE '005056C000081ED696E10D9EF3469892'. "#EC NOTEXT
    CONSTANTS zcx_saplink TYPE sotr_conc VALUE '005056C000081ED696E10D9EF346B892'. "#EC NOTEXT
    DATA object TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !msg      TYPE string DEFAULT '44F7518323DB08BC02000000A7E42BB6'
        !object   TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zsaplink DEFINITION
  CREATE PUBLIC
  ABSTRACT
  .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF  gts_version_info,
*     Change this if import or export is incompatible to older major versions or if enhancements are so important to force the new version of the plugin
*     Please comment changes in major version in class documentation
        zsaplink_plugin_major_version TYPE i,
*     Change this if bugfixes are being done but the basic structure and im- and exportbehaviour don't change.  Reset to 0 when incrementing major version
*     Please comment changes in minor version in class documentation
        zsaplink_plugin_minor_version TYPE i,
*     Change this if cosmetic changes are being done or if internal handling changed but no change to import- or exportbehaviour
*    ( i.e. speeding up the plugin will fall into this ).  Reset to 0 when incrementeing major or minor version
        zsaplink_plugin_build_version TYPE i,
*
        zsaplink_plugin_info1         TYPE string,  " Plugin info - part 1    -  See demoimplementation how this may be used
        zsaplink_plugin_info2         TYPE string,  " Plugin info - part 2
        zsaplink_plugin_info3         TYPE string,  " Plugin info - part 3
        zsaplink_plugin_info4         TYPE string,  " Plugin info - part 4
        zsaplink_plugin_info5         TYPE string,  " Plugin info - part 5
      END OF gts_version_info .

    DATA nugget_level TYPE int4 READ-ONLY VALUE 0.          "#EC NOTEXT

    CLASS-METHODS getobjectinfofromixmldoc
      IMPORTING
        !ixmldocument TYPE REF TO if_ixml_document
      EXPORTING
        !objtypename  TYPE string
        !objname      TYPE string
      RAISING
        zcx_saplink .
    CLASS-METHODS convertstringtoixmldoc
      IMPORTING
        VALUE(xmlstring)    TYPE string
      RETURNING
        VALUE(ixmldocument) TYPE REF TO if_ixml_document .
    CLASS-METHODS convertixmldoctostring
      IMPORTING
        !ixmldocument    TYPE REF TO if_ixml_document
      RETURNING
        VALUE(xmlstring) TYPE string .
    CLASS-METHODS get_version_info_static
      IMPORTING
        !iv_classname          TYPE clike
      RETURNING
        VALUE(rs_version_info) TYPE gts_version_info .
    METHODS createobjectfromixmldoc
      ABSTRACT
      IMPORTING
        !ixmldocument TYPE REF TO if_ixml_document
        !devclass     TYPE devclass DEFAULT '$TMP'
        !overwrite    TYPE flag OPTIONAL
      RETURNING
        VALUE(name)   TYPE string
      RAISING
        zcx_saplink .
    METHODS createixmldocfromobject
      ABSTRACT
      RETURNING
        VALUE(ixmldocument) TYPE REF TO if_ixml_document
      RAISING
        zcx_saplink .
    METHODS createstringfromobject
      RETURNING
        VALUE(string) TYPE string
      RAISING
        zcx_saplink .
    METHODS constructor
      IMPORTING
        !name TYPE string .
    METHODS uploadxml
      FINAL
      IMPORTING
        !xmldata TYPE string .
    CLASS-METHODS getplugins
      CHANGING
        VALUE(objecttable) TYPE table .
    METHODS checkexists
      ABSTRACT
      RETURNING
        VALUE(exists) TYPE flag .
    METHODS valuehelp
      IMPORTING
        !i_objtype       TYPE string
      RETURNING
        VALUE(e_objname) TYPE string .
    CLASS-METHODS checkobject
      IMPORTING
        !i_ixmldocument TYPE REF TO if_ixml_document
      EXPORTING
        !e_objtype      TYPE string
        !e_objname      TYPE string
        !e_pluginexists TYPE flag
        !e_objectexists TYPE flag
        !e_targetobject TYPE REF TO zsaplink .
    METHODS get_version_info
      RETURNING
        VALUE(rs_version_info) TYPE gts_version_info .
  PROTECTED SECTION.

    DATA objname TYPE string .
    DATA ixml TYPE REF TO if_ixml .
    DATA xmldoc TYPE REF TO if_ixml_document .

    METHODS deleteobject
      ABSTRACT
      RAISING
        zcx_saplink .
    CLASS-METHODS setattributesfromstructure
      IMPORTING
        !node      TYPE REF TO if_ixml_element
        !structure TYPE data .
    CLASS-METHODS getstructurefromattributes
      IMPORTING
        !node            TYPE REF TO if_ixml_element
        !preserveversion TYPE flag OPTIONAL
      CHANGING
        !structure       TYPE data .
    METHODS createxmlstring
      FINAL
      RETURNING
        VALUE(xml) TYPE string .
    CLASS-METHODS buildtablefromstring
      IMPORTING
        !source            TYPE string
      RETURNING
        VALUE(sourcetable) TYPE table_of_strings .
    CLASS-METHODS buildsourcestring
      IMPORTING
        !sourcetable        TYPE rswsourcet OPTIONAL
        !pagetable          TYPE o2pageline_table OPTIONAL
      RETURNING
        VALUE(sourcestring) TYPE string .
    METHODS getobjecttype
      ABSTRACT
      RETURNING
        VALUE(objecttype) TYPE string .
    METHODS createotrfromnode
      IMPORTING
        VALUE(node) TYPE REF TO if_ixml_element
        !devclass   TYPE devclass DEFAULT '$TMP'
      EXPORTING
        !concept    TYPE sotr_text-concept
      RAISING
        zcx_saplink .
    METHODS createnodefromotr
      IMPORTING
        !otrguid    TYPE sotr_conc
      RETURNING
        VALUE(node) TYPE REF TO if_ixml_element .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_objecttable,
        classname TYPE string,
        object    TYPE ko100-object,
        text      TYPE ko100-text,
      END OF t_objecttable .

    DATA streamfactory TYPE REF TO if_ixml_stream_factory .
    DATA xmldata TYPE string .
    DATA:
      objecttable TYPE TABLE OF t_objecttable .
ENDCLASS.

CLASS zsaplink_data_elements DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS createobjectfromixmldoc_nolang
      IMPORTING
        !ixmldocument TYPE REF TO if_ixml_document
        !devclass     TYPE devclass DEFAULT '$TMP'
        !overwrite    TYPE flag OPTIONAL
      RETURNING
        VALUE(name)   TYPE string
      RAISING
        zcx_saplink .

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS c_multilanguagesupport TYPE string VALUE 'MultiLanguageSupport' ##NO_TEXT.
ENDCLASS.

CLASS zsaplink_view_tech_settings DEFINITION
 INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_table_tech_settings DEFINITION
 INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_view_cluster DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    METHODS check_authority
      RAISING
        zcx_saplink .
    METHODS create_transport
      RETURNING
        VALUE(rv_trkorr) TYPE trkorr
      RAISING
        zcx_saplink .
    METHODS enqueue
      IMPORTING
        !action TYPE c
      RAISING
        zcx_saplink .
ENDCLASS.

CLASS zsaplink_views DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_table_types DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_vdat DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS seop .
    TYPE-POOLS seor .
    TYPE-POOLS seos .
    TYPE-POOLS seot .
    TYPE-POOLS seox .

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    CLASS-DATA t_e071 TYPE zsaplink_tools=>ty_t_e071 .
    CLASS-DATA t_e071k TYPE zsaplink_tools=>ty_t_e071k .
    CLASS-DATA v_mode TYPE c .

    METHODS createixmldocfromrequest
      RETURNING
        VALUE(ixmldocument) TYPE REF TO if_ixml_document
      RAISING
        zcx_saplink .
    METHODS createixmldocfromdatabase
      RETURNING
        VALUE(ixmldocument) TYPE REF TO if_ixml_document
      RAISING
        zcx_saplink .
ENDCLASS.

CLASS zsaplink_table_contents DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.


CLASS zsaplink_tables DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_transactions DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    TYPES:
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

    CONSTANTS c_hex_tra TYPE x VALUE '00' ##NO_TEXT.
    CONSTANTS c_hex_par TYPE x VALUE '02' ##NO_TEXT.
    CONSTANTS c_hex_rep TYPE x VALUE '80' ##NO_TEXT.
    CONSTANTS c_hex_obj TYPE x VALUE '08' ##NO_TEXT.

    METHODS convert_tstcp_param
      IMPORTING
        !iv_param       TYPE tstcp-param
      RETURNING
        VALUE(re_param) TYPE swyparam .
    METHODS get_tcode_from_param
      IMPORTING
        !iv_param       TYPE tstcp-param
      RETURNING
        VALUE(re_tcode) TYPE tstc-tcode .
ENDCLASS.



CLASS zsaplink_message_class DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.

CLASS zsaplink_functiongroup DEFINITION
 INHERITING FROM zsaplink
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    METHODS actualize_object_tree .
    METHODS create_textpool
      IMPORTING
        !textpoolnode TYPE REF TO if_ixml_element .
    METHODS create_function_modules
      IMPORTING
        !fm_node   TYPE REF TO if_ixml_element
        !fct_group TYPE tlibg-area .
    METHODS create_documentation
      IMPORTING
        !docnode TYPE REF TO if_ixml_element .
    METHODS create_fm_documentation
      IMPORTING
        !docnode TYPE REF TO if_ixml_element .
    METHODS dequeue_abap
      RAISING
        zcx_saplink .
    METHODS create_includes
      IMPORTING
        !incl_node TYPE REF TO if_ixml_element
        !devclass  TYPE devclass DEFAULT '$TMP' .
    METHODS get_textpool
      RETURNING
        VALUE(textnode) TYPE REF TO if_ixml_element .
    METHODS get_documentation
      RETURNING
        VALUE(docnode) TYPE REF TO if_ixml_element .
    METHODS get_fm_documentation
      IMPORTING
        !fm_name       TYPE any
      RETURNING
        VALUE(docnode) TYPE REF TO if_ixml_element .
    METHODS get_includes
      IMPORTING
        !main_prog       TYPE sy-repid
        !fct_group       TYPE tlibt-area
      RETURNING
        VALUE(incl_node) TYPE REF TO if_ixml_element .
    METHODS create_source
      IMPORTING
        !source  TYPE table_of_strings
        !attribs TYPE trdir .
    METHODS enqueue_abap
      RAISING
        zcx_saplink .
    METHODS get_function_modules
      IMPORTING
        !fct_group     TYPE tlibg-area
      RETURNING
        VALUE(fm_node) TYPE REF TO if_ixml_element .
    METHODS transport_copy
      IMPORTING
        !author   TYPE syuname
        !devclass TYPE devclass
      RAISING
        zcx_saplink .
    METHODS get_dynpro
      RETURNING
        VALUE(dynp_node) TYPE REF TO if_ixml_element .
    METHODS create_dynpro
      IMPORTING
        !dynp_node TYPE REF TO if_ixml_element .
    METHODS get_pfstatus
      RETURNING
        VALUE(pfstat_node) TYPE REF TO if_ixml_element .
    METHODS create_pfstatus
      IMPORTING
        !pfstat_node TYPE REF TO if_ixml_element .
ENDCLASS.


CLASS zsaplink_domains DEFINITION
    INHERITING FROM zsaplink
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS createobjectfromixmldoc_nolang
      IMPORTING
        !ixmldocument TYPE REF TO if_ixml_document
        !devclass     TYPE devclass DEFAULT '$TMP'
        !overwrite    TYPE flag OPTIONAL
      RETURNING
        VALUE(name)   TYPE string
      RAISING
        zcx_saplink .

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS c_multilanguagesupport TYPE string VALUE 'MultiLanguageSupport' ##NO_TEXT.
ENDCLASS.

CLASS zsaplink_enh_implementation DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS seop .
    TYPE-POOLS seor .
    TYPE-POOLS seos .
    TYPE-POOLS seot .
    TYPE-POOLS seox .

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    DATA shorttext TYPE string .
    DATA spot_name TYPE enhspotname .
    CONSTANTS enhancement_id TYPE string VALUE 'enhancement_id' ##NO_TEXT.
    CONSTANTS shorttext_id TYPE string VALUE 'shorttext' ##NO_TEXT.
    CONSTANTS spot_name_id TYPE string VALUE 'spot_name' ##NO_TEXT.
    DATA badiimpl_tool TYPE REF TO cl_enh_tool_badi_impl .
    DATA hookimpl_tool TYPE REF TO cl_enh_tool_hook_impl .
    DATA enhtooltype TYPE enhtooltype VALUE 'Tool Type' ##NO_TEXT.
    CONSTANTS enhtooltype_id TYPE string VALUE 'tooltype_id' ##NO_TEXT.
    DATA pgmid TYPE pgmid .
    DATA obj_name TYPE trobj_name .
    DATA obj_type TYPE trobjtype .
    DATA program TYPE progname .
    CONSTANTS orig_obj TYPE string VALUE 'orig_object' ##NO_TEXT.

    METHODS get_impl_tool
      IMPORTING
        !i_lock TYPE boolean OPTIONAL
      RAISING
        zcx_saplink .
    METHODS get_implementations
      EXPORTING
        VALUE(r_implementations) TYPE any
      RAISING
        zcx_saplink .
    METHODS get_shorttext .
    METHODS save_badi_impl
      IMPORTING
        VALUE(enhref)     TYPE REF TO if_enh_tool
        VALUE(badi_impls) TYPE enh_badi_impl_data_it
      RAISING
        zcx_saplink .
    METHODS save_hook_impl
      IMPORTING
        VALUE(enhref)     TYPE REF TO if_enh_tool
        VALUE(hook_impls) TYPE enh_hook_impl_it
      RAISING
        zcx_saplink .
ENDCLASS.

CLASS zsaplink_oo DEFINITION
  INHERITING FROM zsaplink
  CREATE PUBLIC
  ABSTRACT
  .
  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS seop .
    TYPE-POOLS seor .
    TYPE-POOLS seos .
    TYPE-POOLS seot .
    TYPE-POOLS seox .

    CONSTANTS c_xml_key_friends TYPE string VALUE 'friends'. "#EC NOTEXT
    CONSTANTS c_xml_key_inheritance TYPE string VALUE 'inheritance'. "#EC NOTEXT
    CONSTANTS c_xml_key_sotr TYPE string VALUE 'sotr'.      "#EC NOTEXT
    CONSTANTS c_xml_key_sotrtext TYPE string VALUE 'sotrText'. "#EC NOTEXT
  PROTECTED SECTION.

    CONSTANTS c_xml_key_alias_method TYPE string VALUE 'aliasMethod'. "#EC NOTEXT
    CONSTANTS c_xml_key_clsdeferrd TYPE string VALUE 'typeClasDef'. "#EC NOTEXT
    CONSTANTS c_xml_key_forwarddeclaration TYPE string VALUE 'forwardDeclaration'. "#EC NOTEXT
    CONSTANTS c_xml_key_intdeferrd TYPE string VALUE 'typeIntfDef'. "#EC NOTEXT
    CONSTANTS c_xml_key_typepusage TYPE string VALUE 'typeUsage'. "#EC NOTEXT

    METHODS create_alias_method
      CHANGING
        !xt_aliases_method TYPE seoo_aliases_r .
    METHODS create_clsdeferrd
      CHANGING
        !xt_clsdeferrds TYPE seot_clsdeferrds_r .
    METHODS create_intdeferrd
      CHANGING
        !xt_intdeferrds TYPE seot_intdeferrds_r .
    METHODS create_otr
      IMPORTING
        VALUE(node) TYPE REF TO if_ixml_element
        !devclass   TYPE devclass DEFAULT '$TMP'
      EXPORTING
        !concept    TYPE sotr_text-concept
      RAISING
        zcx_saplink .
    METHODS create_typepusage
      CHANGING
        !xt_typepusages TYPE seot_typepusages_r .
    METHODS get_alias_method
      IMPORTING
        !it_methods  TYPE abap_methdescr_tab
      CHANGING
        !xo_rootnode TYPE REF TO if_ixml_element .
    METHODS get_clsdeferrd
      CHANGING
        !xo_rootnode TYPE REF TO if_ixml_element .
    METHODS get_intdeferrd
      CHANGING
        !xo_rootnode TYPE REF TO if_ixml_element .
    METHODS get_otr
      IMPORTING
        !otrguid    TYPE sotr_conc
      RETURNING
        VALUE(node) TYPE REF TO if_ixml_element .
    METHODS get_typepusage
      CHANGING
        !xo_rootnode TYPE REF TO if_ixml_element .
  PRIVATE SECTION.
ENDCLASS.
CLASS zsaplink_class DEFINITION
  INHERITING FROM zsaplink_oo
  CREATE PUBLIC
  .
  PUBLIC SECTION.

    TYPE-POOLS abap .
    DATA mv_steamroller TYPE abap_bool VALUE abap_false.    "#EC NOTEXT

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
    METHODS get_version_info
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS c_xml_key_method_documentation TYPE string VALUE 'methodDocumentation'. "#EC NOTEXT
    CONSTANTS c_xml_key_textelement TYPE string VALUE 'textElement'. "#EC NOTEXT
    CONSTANTS c_xml_key_textpool TYPE string VALUE 'textPool'. "#EC NOTEXT
    CONSTANTS c_xml_key_class_documentation TYPE string VALUE 'classDocumentation'. "#EC NOTEXT
    CONSTANTS c_xml_key_language TYPE string VALUE 'language'. "#EC NOTEXT
    CONSTANTS c_xml_key_object TYPE string VALUE 'OBJECT'.  "#EC NOTEXT
    CONSTANTS c_xml_key_spras TYPE string VALUE 'SPRAS'.    "#EC NOTEXT
    CONSTANTS c_xml_key_textline TYPE string VALUE 'textLine'. "#EC NOTEXT

    METHODS create_documentation .
    METHODS create_method_documentation
      IMPORTING
        !node TYPE REF TO if_ixml_element .
    METHODS create_sections .
    METHODS create_textpool .
    METHODS findimplementingclass
      IMPORTING
        !methodname      TYPE string
        !startclass      TYPE string OPTIONAL
      RETURNING
        VALUE(classname) TYPE string .
    METHODS get_documentation
      CHANGING
        !rootnode TYPE REF TO if_ixml_element .
    METHODS get_method_documentation
      IMPORTING
        !method_key TYPE seocpdkey
      CHANGING
        !rootnode   TYPE REF TO if_ixml_element .
    METHODS get_sections
      CHANGING
        !rootnode TYPE REF TO if_ixml_element .
    METHODS get_textpool
      CHANGING
        !rootnode TYPE REF TO if_ixml_element .

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.
CLASS zsaplink_program DEFINITION
  INHERITING FROM zsaplink
  FINAL
  CREATE PUBLIC
  .
  PUBLIC SECTION.

    METHODS checkexists
        REDEFINITION .
    METHODS createixmldocfromobject
        REDEFINITION .
    METHODS createobjectfromixmldoc
        REDEFINITION .
    METHODS createstringfromobject
        REDEFINITION .
  PROTECTED SECTION.

    METHODS deleteobject
        REDEFINITION .
    METHODS getobjecttype
        REDEFINITION .
  PRIVATE SECTION.

    METHODS get_source
      RETURNING
        VALUE(progsource) TYPE rswsourcet .
    METHODS update_wb_tree .
    METHODS create_textpool
      IMPORTING
        !textpoolnode TYPE REF TO if_ixml_element .
    METHODS dequeue_abap
      RAISING
        zcx_saplink .
    METHODS get_textpool
      RETURNING
        VALUE(textnode) TYPE REF TO if_ixml_element .
    METHODS create_documentation
      IMPORTING
        !docnode TYPE REF TO if_ixml_element .
    METHODS create_source
      IMPORTING
        !source  TYPE table_of_strings
        !attribs TYPE trdir .
    METHODS enqueue_abap
      RAISING
        zcx_saplink .
    METHODS get_documentation
      RETURNING
        VALUE(docnode) TYPE REF TO if_ixml_element .
    METHODS transport_copy
      IMPORTING
        !author   TYPE syuname
        !devclass TYPE devclass
      RAISING
        zcx_saplink .
    METHODS get_dynpro
      RETURNING
        VALUE(dynp_node) TYPE REF TO if_ixml_element .
    METHODS create_dynpro
      IMPORTING
        !dynp_node TYPE REF TO if_ixml_element .
    METHODS get_pfstatus
      RETURNING
        VALUE(pfstat_node) TYPE REF TO if_ixml_element .
    METHODS create_pfstatus
      IMPORTING
        !pfstat_node TYPE REF TO if_ixml_element .
ENDCLASS.
CLASS zcx_saplink IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_saplink .
    ENDIF.
    me->msg = msg .
    me->object = object .
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_domains IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DOMAINS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd01v_wa TYPE dd01v.
    l_name = objname.
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd01v_wa      = dd01v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd01v_wa-domname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DOMAINS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*
**      Plugin created by:
**      Thomas Jung
**      thomas.jung1@gmail.com

**      Stefan Schmöcker - July 2014
**      Changed to support multilinguitiy

    DATA: lt_ddlanguage TYPE STANDARD TABLE OF ddlanguage WITH NON-UNIQUE DEFAULT KEY,
          lo_rootnode   TYPE REF TO if_ixml_element,
          lo_langunode  TYPE REF TO if_ixml_element,
          lo_dd01vnode  TYPE REF TO if_ixml_element,
          lo_dd07vnode  TYPE REF TO if_ixml_element,
          lv_objecttype TYPE string,
          lv_value      TYPE string,
          lv_ddobjname  TYPE ddobjname,
          ls_dd01v      TYPE dd01v,
          lt_dd07v      TYPE STANDARD TABLE OF dd07v WITH NON-UNIQUE DEFAULT KEY.
    FIELD-SYMBOLS: <lv_ddlanguage> LIKE LINE OF lt_ddlanguage,
                   <ls_dd07v>      LIKE LINE OF lt_dd07v.

*--------------------------------------------------------------------*
* First determine all languages that we have to take into account
* This translation could have taken place
*       for the domain description  ( DD01T )
*       or for the fix-values       ( DD07T )
* Get a list of all languages
*--------------------------------------------------------------------*
    SELECT DISTINCT ddlanguage
      INTO TABLE lt_ddlanguage
      FROM dd01t
      WHERE domname = me->objname.
    SELECT DISTINCT ddlanguage
      APPENDING TABLE lt_ddlanguage
      FROM dd07t
      WHERE domname = me->objname.
    SORT lt_ddlanguage.
    DELETE ADJACENT DUPLICATES FROM lt_ddlanguage.

*--------------------------------------------------------------------*
* Build rootnode
*--------------------------------------------------------------------*
    lv_objecttype = getobjecttype( ).
    lo_rootnode   = xmldoc->create_element( lv_objecttype ).
    lo_rootnode->set_attribute(  name  = 'DOMNAME'
                                 value = me->objname ).
    lo_rootnode->set_attribute( name  = c_multilanguagesupport
                                value = 'X' ).
*--------------------------------------------------------------------*
* For each language add a language-node,
* and then add the relevant data from DD01L and DD07L
*--------------------------------------------------------------------*
    lv_ddobjname = me->objname.
    LOOP AT lt_ddlanguage ASSIGNING <lv_ddlanguage>.

*--------------------------------------------------------------------*
      lo_langunode = xmldoc->create_element( 'DDLANGUAGE' ).
      lv_value     = <lv_ddlanguage>.
      lo_langunode->set_attribute(  name  = 'LANGU'
                                    value = lv_value ).

      CLEAR: ls_dd01v,
             lt_dd07v.
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_ddobjname
          langu         = <lv_ddlanguage>
        IMPORTING
*         gotstate      = gotstate
          dd01v_wa      = ls_dd01v
        TABLES
          dd07v_tab     = lt_dd07v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd01v-domname IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_found.
      ENDIF.

      lo_dd01vnode = xmldoc->create_element( 'DD01V' ).
      setattributesfromstructure( node      = lo_dd01vnode
                                  structure = ls_dd01v ).
      lo_langunode->append_child( lo_dd01vnode ).

      LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
        lo_dd07vnode = xmldoc->create_element( 'DD07V' ).
        setattributesfromstructure( node      = lo_dd07vnode
                                    structure = <ls_dd07v> ).
        lo_langunode->append_child( lo_dd07vnode ).
      ENDLOOP.

      lo_rootnode->append_child( lo_langunode ).

    ENDLOOP.

    xmldoc->append_child( lo_rootnode ).
    ixmldocument = xmldoc.
*
*  DATA: gotstate TYPE ddgotstate,
*        dd01v_wa TYPE dd01v,
*        dd07v_tab TYPE STANDARD TABLE OF dd07v.
*
**xml nodes
*  DATA rootnode   TYPE REF TO if_ixml_element.
*  DATA dd07v_node TYPE REF TO if_ixml_element.
*  DATA rc         TYPE sysubrc.
*  DATA _domaname  TYPE ddobjname.
*  _domaname = objname.
*
*  CALL FUNCTION 'DDIF_DOMA_GET'
*    EXPORTING
*      name          = _domaname
*      langu         = sy-langu
*    IMPORTING
*      gotstate      = gotstate
*      dd01v_wa      = dd01v_wa
*    TABLES
*      dd07v_tab     = dd07v_tab
*    EXCEPTIONS
*      illegal_input = 1
*      OTHERS        = 2.
*  IF sy-subrc <> 0 OR dd01v_wa-domname IS INITIAL.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>not_found.
*  ENDIF.
*
** Create parent node
*  DATA _objtype TYPE string.
*  _objtype = getobjecttype( ).
*  rootnode = xmldoc->create_element( _objtype ).
*  setattributesfromstructure( node = rootnode structure = dd01v_wa ).
*
*  DATA: wa_dd07v LIKE LINE OF dd07v_tab.
*  LOOP AT dd07v_tab INTO wa_dd07v.
*    dd07v_node = xmldoc->create_element( 'dd07v' ).
*    setattributesfromstructure( node = dd07v_node structure = wa_dd07v ).
*    rc = rootnode->append_child( dd07v_node ).
*  ENDLOOP.

*\--------------------------------------------------------------------/
*  rc = xmldoc->append_child( lo_rootnode ).
*  ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DOMAINS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

*      Stefan Schmöcker - July 2014
*      Changed to support multilinguitiy


    DATA: gotstate  TYPE ddgotstate,
          dd01v_wa  TYPE dd01v,
          dd07v_tab TYPE STANDARD TABLE OF dd07v.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA dd07v_node  TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _domaname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

* begin of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality
    DATA: lv_multilanguagesupport TYPE flag.
    lv_multilanguagesupport = rootnode->get_attribute( name = c_multilanguagesupport ).
    IF lv_multilanguagesupport IS INITIAL.  " Not found or not set --> use old version of this class
      name = createobjectfromixmldoc_nolang(  ixmldocument =  ixmldocument
                                              devclass     =  devclass
                                              overwrite    =  overwrite    ).
      RETURN.
    ENDIF.
* end of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd01v_wa.

    objname = dd01v_wa-domname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* begin of deletion Multilinguality - moved down and into loop
** retrieve Domain details
*  FREE: filter, iterator, node.
*  filter = xmldoc->create_filter_name( 'dd07v' ).
*  iterator = xmldoc->create_iterator_filtered( filter ).
*  node ?= iterator->get_next( ).
*
*  DATA: wa_dd07v LIKE LINE OF dd07v_tab.
*  WHILE node IS NOT INITIAL.
*    CLEAR dd07v_node.
*    CALL METHOD getstructurefromattributes
*      EXPORTING
*        node      = node
*      CHANGING
*        structure = wa_dd07v.
*    APPEND wa_dd07v TO dd07v_tab.
*    node ?= iterator->get_next( ).
*  ENDWHILE.
* end of deletion Multilinguality - moved down and into loop

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

* begin of deletion Multilinguality - moved into loop
*  CALL FUNCTION 'DDIF_DOMA_PUT'
*    EXPORTING
*      name              = l_dd_objname
*      dd01v_wa          = dd01v_wa
*    TABLES
*      dd07v_tab         = dd07v_tab
*    EXCEPTIONS
*      doma_not_found    = 1
*      name_inconsistent = 2
*      doma_inconsistent = 3
*      put_failure       = 4
*      put_refused       = 5
*      OTHERS            = 6.
*  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>system_error.
*  ENDIF.
* begin of deletion Multilinguality -  moved into loop
* begin of insertion Multilinguality - loop at languages and insert them all
    DATA: lo_langunode TYPE REF TO if_ixml_element,
          lo_dd01vnode TYPE REF TO if_ixml_element,
          lo_dd07vnode TYPE REF TO if_ixml_element,
          ls_dd01v     TYPE dd01v,
          lt_dd07v     TYPE STANDARD TABLE OF dd07v WITH NON-UNIQUE DEFAULT KEY.
    FIELD-SYMBOLS: <ls_dd07v>      LIKE LINE OF lt_dd07v.

    lo_langunode ?= rootnode->find_from_name( 'DDLANGUAGE' ).
    WHILE lo_langunode IS BOUND.  " No need to extract anything from languagenode since language is duplicated in DD01V and DD07V

      CLEAR: ls_dd01v,
             lt_dd07v.
      FREE:  lo_dd01vnode,
             lo_dd07vnode.

* DD01V
      lo_dd01vnode ?= lo_langunode->find_from_name( 'DD01V' ).
      IF lo_dd01vnode IS BOUND.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = lo_dd01vnode
          CHANGING
            structure = ls_dd01v.

* DD07V
        lo_dd07vnode ?= lo_langunode->find_from_name( 'DD07V' ).
        WHILE lo_dd07vnode IS BOUND.
          APPEND INITIAL LINE TO lt_dd07v ASSIGNING <ls_dd07v>.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = lo_dd07vnode
            CHANGING
              structure = <ls_dd07v>.
          lo_dd07vnode ?= lo_dd07vnode->get_next( ).
        ENDWHILE.


        CALL FUNCTION 'DDIF_DOMA_PUT'
          EXPORTING
            name              = l_dd_objname
            dd01v_wa          = ls_dd01v
          TABLES
            dd07v_tab         = lt_dd07v
          EXCEPTIONS
            doma_not_found    = 1
            name_inconsistent = 2
            doma_inconsistent = 3
            put_failure       = 4
            put_refused       = 5
            OTHERS            = 6.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
        ENDIF.
      ENDIF.

      lo_langunode ?= lo_langunode->get_next( ).


    ENDWHILE.
* end of insertion Multilinguality - loop at languages and insert them all



    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DOMAINS->CREATEOBJECTFROMIXMLDOC_NOLANG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc_nolang.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

*      Stefan Schmöcker - July 2014
*      Changed to support multilinguitiy


    DATA: gotstate  TYPE ddgotstate,
          dd01v_wa  TYPE dd01v,
          dd07v_tab TYPE STANDARD TABLE OF dd07v.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA dd07v_node  TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _domaname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd01v_wa.

    objname = dd01v_wa-domname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* retrieve Domain details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd07v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    DATA: wa_dd07v LIKE LINE OF dd07v_tab.
    WHILE node IS NOT INITIAL.
      CLEAR dd07v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_dd07v.
      APPEND wa_dd07v TO dd07v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = l_dd_objname
        dd01v_wa          = dd01v_wa
      TABLES
        dd07v_tab         = dd07v_tab
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.



    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_DOMAINS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_DOMAINS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'DOMA'.  "Domain
  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_enh_implementation IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_ENH_IMPLEMENTATION->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA cx  TYPE REF TO cx_root.

    TRY.
        me->get_impl_tool( ).
        exists = 'X'.
      CATCH cx_root INTO cx.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_ENH_IMPLEMENTATION->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
    " General DATA types
    DATA _objtype TYPE string.
    DATA rc TYPE sysubrc.
    DATA cx TYPE REF TO cx_enh_root.
    " XML related DATA types
    DATA rootnode TYPE REF TO if_ixml_element.
    " Object specific DATA types
    DATA badi_impls      TYPE enh_badi_impl_data_it.
    DATA hook_impls      TYPE enh_hook_impl_it.
    DATA impls_xml  TYPE string.
    DATA impls_ixml TYPE REF TO if_ixml_document.
    DATA impls_root TYPE REF TO if_ixml_element.
    DATA value TYPE string.
    DATA enh_include TYPE progname.
    DATA extension TYPE enhincludeextension.
    DATA enhobj    TYPE enhobj.
    FIELD-SYMBOLS <fs_impls> TYPE any.

    " Set root object
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    rootnode->set_attribute(
      EXPORTING
        name      = me->enhancement_id " NAME
        value     = objname          " VALUE
    ).

    TRY.
        me->get_impl_tool( ).
      CATCH cx_enh_root INTO cx.
    ENDTRY.
    " Read Shorttext of implementation
    me->get_shorttext( ).
    rootnode->set_attribute(
      EXPORTING
        name      = me->shorttext_id " NAME
        value     = me->shorttext       " VALUE
    ).

    " set enhancement tool type
    value = enhtooltype.
    rootnode->set_attribute(
        EXPORTING
          name      = me->enhtooltype_id
          value     = value
      ).

    IF enhtooltype EQ cl_enh_tool_badi_impl=>tooltype.  " BADI implementation
      " Enhancement Spot
      value = me->badiimpl_tool->get_spot_name( ).
      rootnode->set_attribute(
        EXPORTING
          name      = me->spot_name_id
          value     = value
      ).
      ASSIGN badi_impls TO <fs_impls>.

    ELSE. "hook implementation

      ASSIGN hook_impls TO <fs_impls>.
      CALL METHOD me->hookimpl_tool->get_original_object
        EXPORTING
          version  = 'I'
        IMPORTING
          pgmid    = me->pgmid
          obj_name = me->obj_name
          obj_type = me->obj_type
          program  = me->program.

      CONCATENATE me->pgmid    me->obj_name
                  me->obj_type me->program
      INTO value  SEPARATED BY '-'.

      rootnode->set_attribute(
        EXPORTING
          name      = me->orig_obj
          value     = value
      ).

    ENDIF.

    " Read implementations and transform to XML
    CALL METHOD me->get_implementations
      IMPORTING
        r_implementations = <fs_impls>.

    CALL TRANSFORMATION (`ID`)
    SOURCE impls = <fs_impls>
    RESULT XML impls_xml.

    impls_ixml = zsaplink=>convertstringtoixmldoc( xmlstring = impls_xml ).
    impls_root = impls_ixml->get_root_element( ).

    rootnode->append_child( new_child = impls_root ).

* append root node to xmldoc
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_ENH_IMPLEMENTATION->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA cx  TYPE REF TO cx_root.
    DATA: msg      TYPE string.

    DATA _objtype TYPE string.
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA ixml TYPE REF TO if_ixml.
    DATA impls_root TYPE REF TO if_ixml_node.
    DATA impls_ixml TYPE REF TO if_ixml_document.
    DATA impls_xml  TYPE string.
    DATA badi_impls      TYPE enh_badi_impl_data_it.
    DATA hook_impls      TYPE enh_hook_impl_it.
    DATA tool_type       TYPE enhtooltype.
    DATA class_not_active TYPE boolean.
    DATA enhobj TYPE string.
    DATA enhname   TYPE enhname.
    DATA enhcomp   TYPE enhcompositename.
    DATA enhref    TYPE REF TO if_enh_tool.

    FIELD-SYMBOLS: <badiimpl> LIKE LINE OF badi_impls.
    FIELD-SYMBOLS: <hookimpl> LIKE LINE OF hook_impls.
    FIELD-SYMBOLS <fs_impls> TYPE any.

    DATA: error_list TYPE REF TO cl_wb_checklist,
          error_tab  TYPE swbme_error_tab.
    FIELD-SYMBOLS: <error> LIKE LINE OF error_tab.
    FIELD-SYMBOLS: <mtext> LIKE LINE OF <error>-mtext.


    DATA:
      e_devclass  TYPE devclass,
      e_overwrite TYPE seox_boolean.

    e_devclass = devclass.
    _objtype = getobjecttype( ).
    e_overwrite = overwrite.
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    me->objname = rootnode->get_attribute( me->enhancement_id ).
    me->shorttext = rootnode->get_attribute( me->shorttext_id ).
    me->spot_name = rootnode->get_attribute( me->spot_name_id ).
    me->enhtooltype = tool_type = rootnode->get_attribute( me->enhtooltype_id ).
    enhobj = rootnode->get_attribute( me->orig_obj ).

    SPLIT enhobj AT '-'
     INTO me->pgmid    me->obj_name
          me->obj_type me->program.

    " check if object exists
    TRY.
        me->get_impl_tool( 'X' ).
      CATCH cx_root INTO cx.
    ENDTRY.

    IF me->badiimpl_tool IS BOUND   OR
       me->hookimpl_tool IS BOUND.
      IF overwrite         IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
        " Delete existing object
        me->deleteobject( ).
      ENDIF.
    ENDIF.
    " Read BAdI/HOOK Implementations from Slinkee
    impls_root = rootnode->get_first_child( ).
    ixml = cl_ixml=>create( ).
    impls_ixml = ixml->create_document( ).
    impls_ixml->append_child( new_child = impls_root ).

    impls_xml = zsaplink=>convertixmldoctostring( ixmldocument = impls_ixml ).

    IF tool_type EQ cl_enh_tool_badi_impl=>tooltype.
      ASSIGN badi_impls TO <fs_impls>.
    ELSE.
      ASSIGN hook_impls TO <fs_impls>.
    ENDIF.

    CALL TRANSFORMATION (`ID`)
      SOURCE XML impls_xml
      RESULT impls = <fs_impls>.

    " Create Implementation
    " try create
    enhname = me->objname.
    " Now create the new implementation
    TRY.
        CALL METHOD cl_enh_factory=>create_enhancement
          EXPORTING
            enhname       = enhname
            enhtype       = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype   = tool_type
            compositename = enhcomp
          IMPORTING
            enhancement   = enhref
          CHANGING
*           TRKORR        =
            devclass      = e_devclass.
      CATCH cx_enh_root INTO cx.
        msg = cx->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.

    IF tool_type EQ cl_enh_tool_badi_impl=>tooltype.
      " enhancement is of type BAdI
      CALL METHOD me->save_badi_impl
        EXPORTING
          enhref     = enhref
          badi_impls = <fs_impls>.
    ELSE.
      " enhancement is of type hook
      CALL METHOD me->save_hook_impl
        EXPORTING
          enhref     = enhref
          hook_impls = <fs_impls>.
    ENDIF.
    " successful install
    name = me->objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_ENH_IMPLEMENTATION->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA: badi_implementations TYPE enh_badi_impl_data_it.
    DATA: hook_implementations TYPE enh_hook_impl_it.

    IF enhtooltype EQ cl_enh_tool_badi_impl=>tooltype.
      IF me->badiimpl_tool->if_enh_object~is_locked( ) NE abap_true.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = 'Enhancement Object is not locked'.
      ENDIF.
      me->badiimpl_tool->if_enh_object~delete(
        EXPORTING
          nevertheless_delete = 'X'   " Enhancement Boolean
          run_dark            = 'X'    " Enhancement Boolean
      ).
      me->badiimpl_tool->if_enh_object~unlock( ).

    ELSE.
      IF me->hookimpl_tool->if_enh_object~is_locked( ) NE abap_true.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = 'Enhancement Object is not locked'.
      ENDIF.
      me->hookimpl_tool->if_enh_object~delete(
        EXPORTING
          nevertheless_delete = 'X'   " Enhancement Boolean
          run_dark            = 'X'    " Enhancement Boolean
      ).
      me->hookimpl_tool->if_enh_object~unlock( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_ENH_IMPLEMENTATION->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    objecttype = 'ENHO'.  " Enhancement Implementation

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_ENH_IMPLEMENTATION->GET_IMPLEMENTATIONS
* +-------------------------------------------------------------------------------------------------+
* | [<---] R_IMPLEMENTATIONS              TYPE        ANY
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_implementations.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/


    IF enhtooltype EQ cl_enh_tool_badi_impl=>tooltype.
      CALL METHOD me->badiimpl_tool->get_implementations
        EXPORTING
          version            = 'I'
        RECEIVING
          re_implementations = r_implementations.
    ELSE.
      CALL METHOD me->hookimpl_tool->get_hook_impls
        EXPORTING
          version      = 'I'
        RECEIVING
          enhancements = r_implementations.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_ENH_IMPLEMENTATION->GET_IMPL_TOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LOCK                         TYPE        BOOLEAN(optional)
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_impl_tool.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA cx  TYPE REF TO cx_enh_root.
    DATA msg TYPE string.

    DATA enhname   TYPE enhname.
    DATA enhspot   TYPE enhspotname.
    DATA enhref    TYPE REF TO if_enh_tool.
    DATA classname TYPE enhtoolclassname.

    enhname = objname.
    enhspot = objname.

    " FREE: enhref, me->badiimpl_tool.

    TRY.
        enhref = cl_enh_factory=>get_enhancement(
                   enhancement_id = enhname
                   lock           = i_lock
                 ).

        " Check that is is realy a BAdI or Hook
        enhtooltype = enhref->get_tool( ).
        IF enhtooltype NE cl_enh_tool_badi_impl=>tooltype AND
           enhtooltype NE cl_enh_tool_hook_impl=>tooltype.
          CONCATENATE 'The enhancement' enhname 'is not a BAdI/Hook Implementation' INTO msg SEPARATED BY space.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = msg.
        ENDIF.

      CATCH cx_enh_root INTO cx.
        msg = cx->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_found
            object = objname.

    ENDTRY.

    " convert based on type of enhancement
    IF enhtooltype EQ cl_enh_tool_badi_impl=>tooltype.
      me->badiimpl_tool ?= enhref.
    ELSE.
      me->hookimpl_tool ?= enhref.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_ENH_IMPLEMENTATION->GET_SHORTTEXT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_shorttext.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    IF enhtooltype EQ cl_enh_tool_badi_impl=>tooltype.
      me->shorttext = me->badiimpl_tool->if_enh_object_docu~get_shorttext( ).
    ELSE.
      me->shorttext = me->hookimpl_tool->if_enh_object_docu~get_shorttext( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_ENH_IMPLEMENTATION->SAVE_BADI_IMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] ENHREF                         TYPE REF TO IF_ENH_TOOL
* | [--->] BADI_IMPLS                     TYPE        ENH_BADI_IMPL_DATA_IT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_badi_impl.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA  class_not_active TYPE boolean.
    DATA: error_list TYPE REF TO cl_wb_checklist.
    DATA error_tab  TYPE swbme_error_tab.

    DATA  cx  TYPE REF TO cx_root.
    DATA: msg      TYPE string,
          msg_long TYPE string.

    FIELD-SYMBOLS: <badiimpl> LIKE LINE OF badi_impls.
    FIELD-SYMBOLS: <error> LIKE LINE OF error_tab.
    FIELD-SYMBOLS: <mtext> LIKE LINE OF <error>-mtext.

    me->badiimpl_tool ?= enhref.
    " Set Enhancement Spot Name
    me->badiimpl_tool->set_spot_name( spot_name = me->spot_name ).

    me->badiimpl_tool->if_enh_object_docu~set_shorttext( me->shorttext ).

    LOOP AT badi_impls ASSIGNING <badiimpl>.
      " The implementation can only be created when the
      " implementing class is active
      CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
        EXPORTING
          clskey        = <badiimpl>-impl_class
        IMPORTING
          not_active    = class_not_active
        EXCEPTIONS
          not_specified = 1
          not_existing  = 2
          is_interface  = 3
          no_text       = 4
          inconsistent  = 5
          OTHERS        = 6.
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF class_not_active = abap_true.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = 'Classes for Enhancement Implementations must be active'.
      ENDIF.
      TRY.
          CALL METHOD badiimpl_tool->add_implementation
            EXPORTING
              im_implementation = <badiimpl>.
        CATCH cx_enh_root INTO cx.
          msg = cx->get_text( ).
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = msg.
      ENDTRY.
    ENDLOOP.

    TRY.
        " Check if the object has still errors which would avoid saving
        me->badiimpl_tool->if_enh_object~check(
          EXPORTING
            version                = cl_enh_tool_badi_impl=>inactive   " ABAP: Program Status (Active, Saved, Transported...)
          CHANGING
            error_list             = error_list    " List of All Error Messages from a Syntax Check
        ).

        error_list->get_error_messages(
          IMPORTING
            p_error_tab = error_tab    " Error Message Table
        ).

        IF error_tab IS NOT INITIAL.
          LOOP AT error_tab ASSIGNING <error> WHERE mtype = 'E'.
            LOOP AT <error>-mtext ASSIGNING <mtext>.
              CONCATENATE msg <mtext> ';' INTO msg.
            ENDLOOP.
          ENDLOOP.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = msg.
        ENDIF.

        " Save
        me->badiimpl_tool->if_enh_object~save( run_dark = 'X' ).
        " Unlock
        me->badiimpl_tool->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO cx.
        " Unlock
        me->badiimpl_tool->if_enh_object~unlock( ).
        msg = cx->get_text( ).
        msg_long = cx->get_longtext( ).
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_ENH_IMPLEMENTATION->SAVE_HOOK_IMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] ENHREF                         TYPE REF TO IF_ENH_TOOL
* | [--->] HOOK_IMPLS                     TYPE        ENH_HOOK_IMPL_IT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_hook_impl.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    DATA  class_not_active TYPE boolean.

    DATA  cx  TYPE REF TO cx_root.
    DATA: msg      TYPE string,
          msg_long TYPE string.

    DATA  include TYPE progname.
    DATA  extension TYPE enhincludeextension.

    FIELD-SYMBOLS: <hookimpl> LIKE LINE OF hook_impls.

    me->hookimpl_tool ?= enhref.
    " saving the enhancement include?
    CALL METHOD me->hookimpl_tool->get_hook_impls_include
      IMPORTING
        include   = include
        extension = extension.

    IF include IS INITIAL OR
       extension IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Enhancement include not saved'.
    ENDIF.

    TRY.
        " Set orig object name
        CALL METHOD me->hookimpl_tool->set_original_object
          EXPORTING
            pgmid    = me->pgmid
            obj_name = me->obj_name
            obj_type = me->obj_type
            program  = me->program.

      CATCH cx_enh_root INTO cx.
        msg = cx->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.

    me->hookimpl_tool->if_enh_object_docu~set_shorttext( me->shorttext ).

    LOOP AT hook_impls ASSIGNING <hookimpl>.
      TRY.
          me->hookimpl_tool->add_hook_impl(
            overwrite = <hookimpl>-overwrite
            method =    <hookimpl>-method
            enhmode =   <hookimpl>-enhmode
            full_name = <hookimpl>-full_name
            source =    <hookimpl>-source
           ).

        CATCH cx_enh_root INTO cx.
          msg = cx->get_text( ).
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = msg.
      ENDTRY.
    ENDLOOP.

    TRY.
        " Save
        me->hookimpl_tool->if_enh_object~save( run_dark = 'X' ).
        " Unlock
        me->hookimpl_tool->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO cx.
        " Unlock
        me->hookimpl_tool->if_enh_object~unlock( ).
        msg = cx->get_text( ).
        msg_long = cx->get_longtext( ).
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS zsaplink IMPLEMENTATION.
  METHOD buildsourcestring.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA stemp TYPE string.
    DATA pageline TYPE o2pageline.

    IF sourcetable IS NOT INITIAL.
      LOOP AT sourcetable INTO stemp.
        CONCATENATE sourcestring stemp cl_abap_char_utilities=>newline
          INTO sourcestring.
      ENDLOOP.
    ELSEIF pagetable IS NOT INITIAL.
      LOOP AT pagetable INTO pageline.
        CONCATENATE sourcestring pageline-line
          cl_abap_char_utilities=>newline
          INTO sourcestring.
      ENDLOOP.
    ENDIF.

* remove extra newline characters for conversion comparison consistency
    SHIFT sourcestring LEFT DELETING LEADING
      cl_abap_char_utilities=>newline.
    SHIFT sourcestring RIGHT DELETING TRAILING
      cl_abap_char_utilities=>newline.
    SHIFT sourcestring LEFT DELETING LEADING space.
  ENDMETHOD.
  METHOD buildtablefromstring.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    SPLIT source AT cl_abap_char_utilities=>newline
      INTO TABLE sourcetable.
  ENDMETHOD.
  METHOD checkobject.
    DATA l_objtable LIKE objecttable.
    DATA l_objline  LIKE LINE OF objecttable.

    CLEAR: e_objtype, e_objname, e_pluginexists, e_objectexists.
    TRY.
        CALL METHOD zsaplink=>getobjectinfofromixmldoc
          EXPORTING
            ixmldocument = i_ixmldocument
          IMPORTING
            objtypename  = e_objtype
            objname      = e_objname.
      CATCH zcx_saplink.
    ENDTRY.

    CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).

    READ TABLE l_objtable INTO l_objline WITH KEY object = e_objtype.

    IF sy-subrc = 0.
      e_pluginexists = 'X'.
      CREATE OBJECT e_targetobject
        TYPE
          (l_objline-classname)
        EXPORTING
          name                  = e_objname.

      e_objectexists = e_targetobject->checkexists( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

*  data meTypeDescr type ref to CL_ABAP_TYPEDESCR.
*  clear className.
*
*  objName = name.
*  meTypeDescr = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_OBJECT_REF( me ).
*  className = meTypeDescr->get_relative_name( ).

    objname = name.
    TRANSLATE objname TO UPPER CASE.

    ixml = cl_ixml=>create( ).
    xmldoc = ixml->create_document( ).
    streamfactory = ixml->create_stream_factory( ).
  ENDMETHOD.
  METHOD convertixmldoctostring.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA _ixml TYPE REF TO if_ixml.
    DATA _encoding   TYPE REF TO if_ixml_encoding.
    DATA _streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA _outputstream TYPE REF TO if_ixml_ostream.
    DATA _renderer TYPE REF TO if_ixml_renderer.
    DATA _tempstring  TYPE string.
    DATA _tempstringx TYPE xstring.
    DATA _printxmldoc TYPE REF TO cl_xml_document.
    DATA _rc TYPE sysubrc.

    _ixml = cl_ixml=>create( ).
    _encoding = _ixml->create_encoding(
        byte_order    = if_ixml_encoding=>co_none
        character_set = 'utf-8' ).
    _streamfactory = _ixml->create_stream_factory( ).
    _outputstream = _streamfactory->create_ostream_xstring( _tempstringx ).
    _outputstream->set_encoding( encoding = _encoding ).
    _renderer = _ixml->create_renderer(
                  document = ixmldocument
                  ostream  = _outputstream
                ).
    _renderer->set_normalizing( ).
    _rc = _renderer->render( ).
    CREATE OBJECT _printxmldoc.
    _rc = _printxmldoc->parse_string( _tempstring ).

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = _tempstringx
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = _tempstring.

    xmlstring = _tempstring.
  ENDMETHOD.
  METHOD convertstringtoixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA ixml TYPE REF TO if_ixml.
    DATA streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA istream TYPE REF TO if_ixml_istream.
    DATA ixmlparser TYPE REF TO if_ixml_parser.
    DATA xmldoc TYPE REF TO if_ixml_document.

    " Make sure to convert Windows Line Break to Unix as
    " this linebreak is used to get a correct import
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN xmlstring WITH cl_abap_char_utilities=>newline.

    ixml = cl_ixml=>create( ).
    xmldoc = ixml->create_document( ).
    streamfactory = ixml->create_stream_factory( ).
    istream = streamfactory->create_istream_string( xmlstring ).
    ixmlparser = ixml->create_parser(  stream_factory = streamfactory
                                       istream        = istream
                                       document       = xmldoc ).
    ixmlparser->parse( ).
    ixmldocument = xmldoc.
  ENDMETHOD.
  METHOD createnodefromotr.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.

    DATA _ixml TYPE REF TO if_ixml.
    DATA _xmldoc TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = otrguid
      IMPORTING
        header         = sotrheader
      TABLES
        entries        = sotrtexttable
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
    rootnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotr ). "OTR object type
    CLEAR sotrheader-concept.                                 "ewH:33
    setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
    LOOP AT sotrtexttable INTO sotrtextline.
      txtnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotrtext ).
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      setattributesfromstructure(
        node = txtnode structure = sotrtextline ).
      rc = rootnode->append_child( txtnode ).
    ENDLOOP.

    node = rootnode.

  ENDMETHOD.
  METHOD createotrfromnode.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.
    DATA sotrpaket TYPE sotr_pack.

* get OTR header info
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = sotrheader.

* get OTR text info
    filter = node->create_filter_name( zsaplink_oo=>c_xml_key_sotrtext ).
    iterator = node->create_iterator_filtered( filter ).
    txtnode ?= iterator->get_next( ).

    WHILE txtnode IS NOT INITIAL.
      CLEAR sotrtextline.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = txtnode
        CHANGING
          structure = sotrtextline.
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      APPEND sotrtextline TO sotrtexttable.
      txtnode ?= iterator->get_next( ).
    ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


    DATA objecttable TYPE sotr_objects.
    DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
    CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
      EXPORTING
        object_vector    = sotrheader-objid_vec
      IMPORTING
        objects          = objecttable
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
    sotrpaket-paket = devclass.
    CALL FUNCTION 'SOTR_CREATE_CONCEPT'
      EXPORTING
        paket                         = sotrpaket
        crea_lan                      = sotrheader-crea_lan
        alias_name                    = sotrheader-alias_name
*       CATEGORY                      =
        object                        = objecttype
        entries                       = sotrtexttable
*       FLAG_CORRECTION_ENTRY         =
*       IN_UPDATE_TASK                =
*       CONCEPT_DEFAULT               = sotrHeader-concept "ewH:33
      IMPORTING
        concept                       = concept         "ewH:33
      EXCEPTIONS
        package_missing               = 1
        crea_lan_missing              = 2
        object_missing                = 3
        paket_does_not_exist          = 4
        alias_already_exist           = 5
        object_type_not_found         = 6
        langu_missing                 = 7
        identical_context_not_allowed = 8
        text_too_long                 = 9
        error_in_update               = 10
        no_master_langu               = 11
        error_in_concept_id           = 12
        alias_not_allowed             = 13
        tadir_entry_creation_failed   = 14
        internal_error                = 15
        error_in_correction           = 16
        user_cancelled                = 17
        no_entry_found                = 18
        OTHERS                        = 19.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.
  METHOD createstringfromobject.
  ENDMETHOD.
  METHOD createxmlstring.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA outputstream TYPE REF TO if_ixml_ostream.
    DATA renderer TYPE REF TO if_ixml_renderer.
    DATA tempstring TYPE string.
    DATA printxmldoc TYPE REF TO cl_xml_document.
    DATA rc TYPE sysubrc.

    streamfactory = ixml->create_stream_factory( ).
    outputstream = streamfactory->create_ostream_cstring( tempstring ).
    renderer = ixml->create_renderer(
      document = xmldoc ostream = outputstream ).
    rc = renderer->render( ).
    CREATE OBJECT printxmldoc.
    rc = printxmldoc->parse_string( tempstring ).
    xml = tempstring.
  ENDMETHOD.
  METHOD getobjectinfofromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_node.
    DATA rootattr TYPE REF TO if_ixml_named_node_map.
    DATA attrnode TYPE REF TO if_ixml_node.
    DATA nodename TYPE string.

    rootnode ?= ixmldocument->get_root_element( ).
* Check whether got a valid ixmldocument
    IF rootnode IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>incorrect_file_format.
    ENDIF.

* get object type
    objtypename = rootnode->get_name( ).
    TRANSLATE objtypename TO UPPER CASE.

* get object name
    rootattr = rootnode->get_attributes( ).
    attrnode = rootattr->get_item( 0 ).
    objname = attrnode->get_value( ).
  ENDMETHOD.
  METHOD getplugins.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA classlist TYPE seo_inheritances.
    DATA classline TYPE vseoextend.
    DATA classobject TYPE REF TO zsaplink.
    DATA objectline TYPE t_objecttable.
    DATA tabletypeline TYPE ko105.
    DATA tabletypesin TYPE TABLE OF ko105.
    DATA tabletypesout TYPE tr_object_texts.
    DATA tabletypeoutline TYPE ko100.
    DATA clsname TYPE string.
    DATA objtype TYPE trobjtype.

    REFRESH objecttable.

    SELECT * FROM vseoextend INTO TABLE classlist
      WHERE refclsname LIKE 'ZSAPLINK%'
      AND version = '1'.

    LOOP AT classlist INTO classline.
      clsname = classline-clsname.
      TRY.
          CREATE OBJECT classobject
            TYPE
              (clsname)
            EXPORTING
              name      = 'foo'.
          objtype = classobject->getobjecttype( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
      CLEAR tabletypeline.
      REFRESH tabletypesin.

      tabletypeline-object = objtype.
      APPEND tabletypeline TO tabletypesin.

      CALL FUNCTION 'TRINT_OBJECT_TABLE'
        TABLES
          tt_types_in  = tabletypesin
          tt_types_out = tabletypesout.

      LOOP AT tabletypesout INTO tabletypeoutline.
        objectline-classname = clsname.
        MOVE-CORRESPONDING tabletypeoutline TO objectline.
        APPEND objectline TO objecttable.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD getstructurefromattributes.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA attributelist TYPE REF TO if_ixml_named_node_map.
    DATA nodeiterator TYPE REF TO if_ixml_node_iterator.
    DATA attributenode TYPE REF TO if_ixml_node.
    DATA value TYPE string.
    DATA name TYPE string.
    FIELD-SYMBOLS <value> TYPE any.

    CLEAR structure.
    if node is not INITIAL.
    attributelist = node->get_attributes( ).
    nodeiterator = attributelist->create_iterator( ).
    attributenode = nodeiterator->get_next( ).
    WHILE attributenode IS NOT INITIAL.
      name = attributenode->get_name( ).
      IF name = 'VERSION' AND preserveversion IS INITIAL. "ewh:issue 45
*    if name = 'VERSION'.
        value = '0'.
      ELSE.
        value = attributenode->get_value( ).
      ENDIF.
      ASSIGN COMPONENT name OF STRUCTURE structure TO <value>.
      IF sy-subrc = 0.
        <value> = value.
      ENDIF.
      attributenode = nodeiterator->get_next( ).

    ENDWHILE.
    endif.















*    .-"-.
*  .'=^=^='.
* /=^=^=^=^=\
*:^=SAPLINK=^;
*|^ EASTER  ^|
*:^=^EGG^=^=^:
* \=^=^=^=^=/
*  `.=^=^=.'
*    `~~~`
* Don't like the way we did something?
* Help us fix it!  Tell us what you think!
* http://saplink.org
  ENDMETHOD.
  METHOD get_version_info.

*--------------------------------------------------------------------*
* Please use the following 6 lines of code when versioning a
* SAPLINK-Plugin.  See documentation of Type GTS_VERSION_INFO
* what should be put here
*--------------------------------------------------------------------*
    rs_version_info-zsaplink_plugin_major_version = 0.  " Default for all child classes, that have not been updated to return a version info.
    rs_version_info-zsaplink_plugin_minor_version = 0.  " Default for all child classes, that have not been updated to return a version info.
    rs_version_info-zsaplink_plugin_build_version = 0.  " Default for all child classes, that have not been updated to return a version info.

    rs_version_info-zsaplink_plugin_info1         = ''. " Sufficient to set this the first time a child class is being updated
    rs_version_info-zsaplink_plugin_info2         = ''. " Sufficient to set this the first time a child class is being updated
    rs_version_info-zsaplink_plugin_info3         = ''. " Sufficient to set this the first time a child class is being updated
    rs_version_info-zsaplink_plugin_info4         = ''. " Sufficient to set this the first time a child class is being updated
    rs_version_info-zsaplink_plugin_info5         = ''. " Sufficient to set this the first time a child class is being updated

* Hint - see redefinition of this class in ZSAPLINK_CLASS how information may be set
  ENDMETHOD.
  METHOD get_version_info_static.

    DATA: lo_zsaplink TYPE REF TO zsaplink.

    TRY.
        CREATE OBJECT lo_zsaplink TYPE (iv_classname)
           EXPORTING
             name = 'Not needed for versio info'.
        rs_version_info = lo_zsaplink->get_version_info( ).
      CATCH cx_root.  " Don't pass version info for unknown or abstract classes
    ENDTRY.

  ENDMETHOD.
  METHOD setattributesfromstructure.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA int TYPE i.
    int = int.
    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA acomponent TYPE abap_compdescr.
    FIELD-SYMBOLS <fieldvalue> TYPE any.
    DATA rc TYPE sysubrc.
    DATA sname TYPE string.
    DATA svalue TYPE string.

    structdescr ?= cl_abap_structdescr=>describe_by_data( structure ).
    LOOP AT structdescr->components INTO acomponent.
      ASSIGN COMPONENT acomponent-name OF STRUCTURE
        structure TO <fieldvalue>.
      IF sy-subrc = 0.
        sname = acomponent-name.
*      sValue = <fieldValue>.
*     for certain attributes, set to a standard for exporting
        CASE sname.
*        when 'VERSION'. "version should always export as inactive
*          sValue = '0'. "commented by ewH: issue 45
          WHEN 'DEVCLASS'. "development class should always be $TMP
            svalue = '$TMP'.
            " Developer, Date and Time Metadata has to be removed to
            " not clutter diffs
            "
            " Meta Attributes for DDIC Types
          WHEN 'AS4USER'.
            CLEAR svalue.
          WHEN 'AS4DATE'.
            CLEAR svalue.
          WHEN 'AS4TIME'.
            CLEAR svalue.
            " Meta Attributes for PROG
          WHEN 'CNAM'.
            CLEAR svalue.
          WHEN 'CDAT'.
            CLEAR svalue.
          WHEN 'UNAM'.
            CLEAR svalue.
          WHEN 'UDAT'.
            CLEAR svalue.
          WHEN 'VERN'.
            CLEAR svalue.
          WHEN 'SDATE'.
            CLEAR svalue.
          WHEN 'STIME'.
            CLEAR svalue.
          WHEN 'IDATE'.
            CLEAR svalue.
          WHEN 'ITIME'.
            CLEAR svalue.
            " Meta Attributes for CLAS
          WHEN 'AUTHOR'.
            CLEAR svalue.
          WHEN 'CREATEDON'.
            CLEAR svalue.
          WHEN 'CHANGEDBY'.
            CLEAR svalue.
          WHEN 'CHANGEDON'.
            CLEAR svalue.
          WHEN 'CHANGETIME'.
            CLEAR svalue.
          WHEN 'CHGDANYON'.
            CLEAR svalue.
          WHEN 'R3RELEASE'.
            CLEAR svalue.
          WHEN 'UUID'.
            CLEAR svalue.
            " SOTR
          WHEN 'CREA_NAME'.
            CLEAR svalue.
          WHEN 'CHAN_NAME'.
            CLEAR svalue.
          WHEN 'CREA_TSTUT'.
            CLEAR svalue.
          WHEN 'CHAN_TSTUT'.
            CLEAR svalue.
            " MSAG
          WHEN 'LASTUSER'.
            CLEAR svalue.
          WHEN 'LDATE'.
            CLEAR svalue.
          WHEN 'LTIME'.
            CLEAR svalue.
          WHEN 'DGEN'.
            CLEAR svalue.
          WHEN 'TGEN'.
            CLEAR svalue.
          WHEN 'GENDATE'.
            CLEAR svalue.
          WHEN 'GENTIME'.
            CLEAR svalue.
            " BSP
          WHEN 'IMPLCLASS'.
            CLEAR svalue.
          WHEN OTHERS.
            svalue = <fieldvalue>.
        ENDCASE.
        IF svalue IS NOT INITIAL.
          rc = node->set_attribute( name = sname value = svalue ).
        ENDIF.
      ELSE.
* WHAT?>!??
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD uploadxml.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA istream TYPE REF TO if_ixml_istream.
    DATA ixmlparser TYPE REF TO if_ixml_parser.

    istream = streamfactory->create_istream_string( xmldata ).
    ixmlparser = ixml->create_parser(  stream_factory = streamfactory
                                       istream        = istream
                                       document       = xmldoc ).
    ixmlparser->parse( ).

  ENDMETHOD.
  METHOD valuehelp.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA l_object_type TYPE  euobj-id.
    DATA objname(40) TYPE c.

    l_object_type = i_objtype.


    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type           = l_object_type
        object_name           = objname
        suppress_selection    = 'X'
        use_alv_grid          = ''
        without_personal_list = ''
      IMPORTING
        object_name_selected  = objname
      EXCEPTIONS
        cancel                = 1.

    e_objname = objname.
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_data_elements IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DATA_ELEMENTS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd04v_wa TYPE dd04v.
    l_name = objname.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd04v_wa      = dd04v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd04v_wa-rollname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DATA_ELEMENTS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

**      Stefan Schmöcker - July 2014
**      Changed to support multilinguitiy


    DATA: lt_ddlanguage TYPE STANDARD TABLE OF ddlanguage WITH NON-UNIQUE DEFAULT KEY,
          lo_rootnode   TYPE REF TO if_ixml_element,
          lo_langunode  TYPE REF TO if_ixml_element,
          lo_tparanode  TYPE REF TO if_ixml_element,
          lo_dd04vnode  TYPE REF TO if_ixml_element,
          lv_objecttype TYPE string,
          lv_value      TYPE string,
          lv_ddobjname  TYPE ddobjname,
          ls_dd04v      TYPE dd04v.

    FIELD-SYMBOLS: <lv_ddlanguage> LIKE LINE OF lt_ddlanguage.

*--------------------------------------------------------------------*
* First determine all languages that we have to take into account
* This translation could have taken place
*       for the domain description  ( DD01T )
*       or for the fix-values       ( DD07T )
* Get a list of all languages
*--------------------------------------------------------------------*
    SELECT DISTINCT ddlanguage
      INTO TABLE lt_ddlanguage
      FROM dd04t
      WHERE rollname = me->objname.

*--------------------------------------------------------------------*
* Build rootnode
*--------------------------------------------------------------------*
    lv_objecttype = getobjecttype( ).
    lo_rootnode   = xmldoc->create_element( lv_objecttype ).

*--------------------------------------------------------------------*
* Simulate old structure to allow old versions of this class to import as well
*--------------------------------------------------------------------*
    DATA: gotstate TYPE ddgotstate,
          dd04v_wa TYPE dd04v,
          tpara_wa TYPE tpara.
    DATA _dtelname  TYPE ddobjname.
    DATA tpara_node TYPE REF TO if_ixml_element.
    DATA rc         TYPE sysubrc.
    _dtelname = objname.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = _dtelname
        langu         = sy-langu
      IMPORTING
        gotstate      = gotstate
        dd04v_wa      = dd04v_wa
        tpara_wa      = tpara_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR dd04v_wa-rollname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    setattributesfromstructure( node = lo_rootnode structure = dd04v_wa ).

    tpara_node = xmldoc->create_element( 'tpara' ).
    setattributesfromstructure( node = tpara_node structure = tpara_wa ).
    rc = lo_rootnode->append_child( tpara_node ).

    lo_rootnode->set_attribute( name  = c_multilanguagesupport
                                value = 'X' ).

*--------------------------------------------------------------------*
* For each language add a language-node,
* and then add the relevant data from DD04
*--------------------------------------------------------------------*
    lv_ddobjname = me->objname.
    LOOP AT lt_ddlanguage ASSIGNING <lv_ddlanguage>.

      lo_langunode = xmldoc->create_element( 'DDLANGUAGE' ).
      lv_value     = <lv_ddlanguage>.
      lo_langunode->set_attribute(  name  = 'LANGU'
                                    value = lv_value ).

      CLEAR: ls_dd04v.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_ddobjname
          langu         = <lv_ddlanguage>
        IMPORTING
          dd04v_wa      = ls_dd04v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd04v-rollname IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_found.
      ENDIF.

      lo_dd04vnode = xmldoc->create_element( 'DD04V' ).
      setattributesfromstructure( node      = lo_dd04vnode
                                  structure = ls_dd04v ).
      lo_langunode->append_child( lo_dd04vnode ).


      lo_rootnode->append_child( lo_langunode ).

    ENDLOOP.

    xmldoc->append_child( lo_rootnode ).
    ixmldocument = xmldoc.


*
*  DATA: gotstate TYPE ddgotstate,
*        dd04v_wa TYPE dd04v,
*        tpara_wa TYPE tpara.
*
**xml nodes
*  DATA rootnode   TYPE REF TO if_ixml_element.
*  DATA tpara_node TYPE REF TO if_ixml_element.
*  DATA rc         TYPE sysubrc.
*  DATA _dtelname  TYPE ddobjname.
*  _dtelname = objname.
*
*  CALL FUNCTION 'DDIF_DTEL_GET'
*    EXPORTING
*      name          = _dtelname
*      langu         = sy-langu
*    IMPORTING
*      gotstate      = gotstate
*      dd04v_wa      = dd04v_wa
*      tpara_wa      = tpara_wa
*    EXCEPTIONS
*      illegal_input = 1
*      OTHERS        = 2.
*
*  IF sy-subrc <> 0 OR dd04v_wa-rollname IS INITIAL.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>not_found.
*  ENDIF.
*
** Create parent node
*  DATA _objtype TYPE string.
*  _objtype = getobjecttype( ).
*  rootnode = xmldoc->create_element( _objtype ).
*  setattributesfromstructure( node = rootnode structure = dd04v_wa ).
*
*  tpara_node = xmldoc->create_element( 'tpara' ).
*  setattributesfromstructure( node = tpara_node structure = tpara_wa ).
*  rc = rootnode->append_child( tpara_node ).
*
**\--------------------------------------------------------------------/
*  rc = xmldoc->append_child( rootnode ).
*  ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DATA_ELEMENTS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

*      Stefan Schmöcker - July 2014
*      Changed to support multilinguitiy

    DATA: gotstate TYPE ddgotstate,
          dd04v_wa TYPE dd04v,
          tpara_wa TYPE tpara.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA tpara_node  TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _dtelname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).


* begin of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality
    DATA: lv_multilanguagesupport TYPE flag.
    lv_multilanguagesupport = rootnode->get_attribute( name = c_multilanguagesupport ).
    IF lv_multilanguagesupport IS INITIAL.  " Not found or not set --> use old version of this class
      name = createobjectfromixmldoc_nolang(  ixmldocument =  ixmldocument
                                              devclass     =  devclass
                                              overwrite    =  overwrite    ).
      RETURN.
    ENDIF.
* end of insertion Multilinguality - check if nugget/slinkee was exported w/o support of multilinguality

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd04v_wa.

    objname = dd04v_wa-rollname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* retrieve data element details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'tpara' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    IF node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = tpara_wa.
    ENDIF.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

* begin of deletion Multilinguality - moved into loop
*  CALL FUNCTION 'DDIF_DTEL_PUT'
*    EXPORTING
*      name              = l_dd_objname
*      dd04v_wa          = dd04v_wa
*    EXCEPTIONS
*      dtel_not_found    = 1
*      name_inconsistent = 2
*      dtel_inconsistent = 3
*      put_failure       = 4
*      put_refused       = 5
*      OTHERS            = 6.
*
*  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING textid = zcx_saplink=>system_error.
*  ENDIF.
* begin of deletion Multilinguality -  moved into loop
* begin of insertion Multilinguality - loop at languages and insert them all
    DATA: lo_langunode TYPE REF TO if_ixml_element,
          lo_dd04vnode TYPE REF TO if_ixml_element,
          ls_dd04v     TYPE dd04v.

    lo_langunode ?= rootnode->find_from_name( 'DDLANGUAGE' ).
    WHILE lo_langunode IS BOUND.  " No need to extract anything from languagenode since language is duplicated in DD01V and DD07V

      CLEAR: ls_dd04v.
      FREE:  lo_dd04vnode.

* DD04V
      lo_dd04vnode ?= lo_langunode->find_from_name( 'DD04V' ).
      IF lo_dd04vnode IS BOUND.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = lo_dd04vnode
          CHANGING
            structure = ls_dd04v.

        CALL FUNCTION 'DDIF_DTEL_PUT'
          EXPORTING
            name              = l_dd_objname
            dd04v_wa          = ls_dd04v
          EXCEPTIONS
            dtel_not_found    = 1
            name_inconsistent = 2
            dtel_inconsistent = 3
            put_failure       = 4
            put_refused       = 5
            OTHERS            = 6.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
        ENDIF.
      ENDIF.

      lo_langunode ?= lo_langunode->get_next( ).

    ENDWHILE.

* end of insertion Multilinguality - loop at languages and insert them all

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_DATA_ELEMENTS->CREATEOBJECTFROMIXMLDOC_NOLANG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc_nolang.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate TYPE ddgotstate,
          dd04v_wa TYPE dd04v,
          tpara_wa TYPE tpara.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA tpara_node  TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _dtelname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd04v_wa.

    objname = dd04v_wa-rollname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* retrieve Domain details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'tpara' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    IF node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = tpara_wa.
    ENDIF.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = l_dd_objname
        dd04v_wa          = dd04v_wa
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_DATA_ELEMENTS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_DATA_ELEMENTS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'DTEL'.  "Data Elements
  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_transactions IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TRANSACTIONS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_TRANSACTIONS->CONVERT_TSTCP_PARAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PARAM                       TYPE        TSTCP-PARAM
* | [<-()] RE_PARAM                       TYPE        SWYPARAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_tstcp_param.

    DATA: lt_param_list   TYPE stringtab.

    DATA: lw_param        TYPE LINE OF swyparam.

    DATA: lv_garbage    TYPE c LENGTH 1,
          lv_params     TYPE c LENGTH 240,
          lv_param_list TYPE string.

    SPLIT iv_param AT ' ' INTO lv_garbage lv_params IN CHARACTER MODE.

    SPLIT lv_params AT ';' INTO TABLE lt_param_list IN CHARACTER MODE.

    LOOP AT lt_param_list INTO lv_param_list.

      CLEAR lw_param.

      SPLIT lv_param_list AT '=' INTO lw_param-field lw_param-value.
      APPEND lw_param TO re_param.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TRANSACTIONS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
    DATA: _objtype    TYPE string,
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

        lv_message = TEXT-001.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TRANSACTIONS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
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

    DATA: ls_header TYPE ty_header,
          ls_params TYPE LINE OF swyparam.

*xml nodes
    DATA: rootnode  TYPE REF TO if_ixml_element,
          line_node TYPE REF TO if_ixml_element,
          node      TYPE REF TO if_ixml_element,
          filter    TYPE REF TO if_ixml_node_filter,
          iterator  TYPE REF TO if_ixml_node_iterator,
          _objtype  TYPE string.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TRANSACTIONS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TRANSACTIONS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_TRANSACTIONS->GET_TCODE_FROM_PARAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PARAM                       TYPE        TSTCP-PARAM
* | [<-()] RE_TCODE                       TYPE        TSTC-TCODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tcode_from_param.

    DATA: lv_garbage TYPE c LENGTH 1,
          lv_params  TYPE c LENGTH 240.

    SPLIT iv_param AT ' ' INTO lv_params lv_garbage IN CHARACTER MODE.

    re_tcode = lv_params+2.

  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_vdat IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VDAT->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_VDAT->CREATEIXMLDOCFROMDATABASE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromdatabase.
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
             tabname TYPE dd02v-tabname,
           END OF ty_tabu.

    DATA: lv_str          TYPE string,
          lv_objtype      TYPE string,
          lv_objname      TYPE ddobjname,
          lv_is_last      TYPE abap_bool,

          lr_t_table      TYPE REF TO data,

          lt_dd26v        TYPE STANDARD TABLE OF dd26v,

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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VDAT->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_VDAT->CREATEIXMLDOCFROMREQUEST
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromrequest.
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
             tabname TYPE dd02v-tabname,
           END OF ty_tabu.

    DATA: lv_str           TYPE string,
          lv_objtype       TYPE string,
          lv_from          TYPE sy-tabix,
          lv_len           TYPE i,
          lv_complete_keys TYPE abap_bool,
          lv_charfields    TYPE string,
          lv_keyfields     TYPE string,
          lv_where         TYPE string,

          lr_t_table       TYPE REF TO data,
          lr_s_table       TYPE REF TO data,
          lr_tabkeys       TYPE REF TO data,
          lr_tabkeys2      TYPE REF TO data,

          lt_dd03p         TYPE STANDARD TABLE OF dd03p,
          lt_fields        TYPE cl_abap_structdescr=>component_table,
          lt_keyfields     TYPE abap_keydescr_tab,
          lt_charfields    TYPE abap_keydescr_tab,
          lt_sortfields    TYPE abap_sortorder_tab,

          lo_structdescr   TYPE REF TO cl_abap_structdescr,
          lo_tabledescr    TYPE REF TO cl_abap_tabledescr,
          lo_tabkeydescr   TYPE REF TO cl_abap_typedescr,

          lo_vdat_node     TYPE REF TO if_ixml_element,
          lo_tabu_node     TYPE REF TO if_ixml_element,
          lo_datarow_node  TYPE REF TO if_ixml_element,
          ls_vdat          TYPE ty_vdat,
          ls_tabu          TYPE ty_tabu.

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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VDAT->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

    DATA: lo_vdat_node        TYPE REF TO if_ixml_element,

          lo_tabu_node        TYPE REF TO if_ixml_element,
          lo_tabu_filter      TYPE REF TO if_ixml_node_filter,
          lo_tabu_iterator    TYPE REF TO if_ixml_node_iterator,

          lo_datarow_node     TYPE REF TO if_ixml_element,
          lo_datarow_filter   TYPE REF TO if_ixml_node_filter,
          lo_datarow_iterator TYPE REF TO if_ixml_node_iterator,

          lt_dd03p            TYPE STANDARD TABLE OF dd03p,

          ls_vdat             TYPE ty_vdat,
          ls_tabu             TYPE ty_tabu,

          lv_objtype          TYPE string,

          lv_clidep           TYPE abap_bool,

          lv_num_keyfields    TYPE i,

          lr_table            TYPE REF TO data,
          lr_table_2          LIKE lr_table,
          lr_struct           TYPE REF TO data,

          ls_ko200            TYPE ko200,
          lt_e071k            TYPE tredt_keys,
          lt_fields           TYPE cl_abap_structdescr=>component_table,
          lt_keyfields        TYPE STANDARD TABLE OF dd03p-fieldname,
          lt_sortfields       TYPE abap_sortorder_tab,
          lt_entries          TYPE STANDARD TABLE OF ty_entry,

          lo_structdescr      TYPE REF TO cl_abap_structdescr,
          lo_tabkeydescr      TYPE REF TO cl_abap_typedescr,
          lo_typedescr        TYPE REF TO cl_abap_typedescr,

          lv_keyfields        TYPE string,
          lv_where            TYPE string,
          lv_complete_keys    TYPE abap_bool,
          lv_tabix            TYPE sy-tabix,
          lv_str              TYPE string.

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

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VDAT->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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
        msg    = v_msg.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VDAT->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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

  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_views IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEWS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd25v_wa TYPE dd25v.
    l_name = objname.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd25v_wa      = dd25v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd25v_wa-viewname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEWS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate  TYPE ddgotstate,
          dd25v_wa  TYPE dd25v,
          dd09l_wa  TYPE dd09v,
          dd26v_tab TYPE STANDARD TABLE OF dd26v,
          dd26v_wa  LIKE LINE OF dd26v_tab,
          dd27p_tab TYPE STANDARD TABLE OF dd27p,
          dd27p_wa  LIKE LINE OF dd27p_tab,
          dd28j_tab TYPE STANDARD TABLE OF dd28j,
          dd28j_wa  LIKE LINE OF dd28j_tab,
          dd28v_tab TYPE STANDARD TABLE OF dd28v,
          dd28v_wa  LIKE LINE OF dd28v_tab.


*xml nodes
    DATA rootnode   TYPE REF TO if_ixml_element.
    DATA dd09l_node TYPE REF TO if_ixml_element.
    DATA dd26v_node TYPE REF TO if_ixml_element.
    DATA dd27p_node TYPE REF TO if_ixml_element.
    DATA dd28j_node TYPE REF TO if_ixml_element.
    DATA dd28v_node TYPE REF TO if_ixml_element.
    DATA rc         TYPE sysubrc.
    DATA _viewname  TYPE ddobjname.
    _viewname = objname.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = _viewname
        langu         = sy-langu
      IMPORTING
        gotstate      = gotstate
        dd25v_wa      = dd25v_wa
        dd09l_wa      = dd09l_wa
      TABLES
        dd26v_tab     = dd26v_tab
        dd27p_tab     = dd27p_tab
        dd28j_tab     = dd28j_tab
        dd28v_tab     = dd28v_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR dd25v_wa-viewname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    DATA _objtype TYPE string.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = dd25v_wa ).

    dd09l_node = xmldoc->create_element( 'dd09l' ).
    setattributesfromstructure( node = dd09l_node structure = dd09l_wa ).
    rc = rootnode->append_child( dd09l_node ).

    LOOP AT dd26v_tab INTO dd26v_wa.
      dd26v_node = xmldoc->create_element( 'dd26v' ).
      setattributesfromstructure( node = dd26v_node structure = dd26v_wa ).
      rc = rootnode->append_child( dd26v_node ).
    ENDLOOP.

    LOOP AT dd27p_tab INTO dd27p_wa.
      dd27p_node = xmldoc->create_element( 'dd27p' ).
      setattributesfromstructure( node = dd27p_node structure = dd27p_wa ).
      rc = rootnode->append_child( dd27p_node ).
    ENDLOOP.

    LOOP AT dd28j_tab INTO dd28j_wa.
      dd28j_node = xmldoc->create_element( 'dd28j' ).
      setattributesfromstructure( node = dd28j_node structure = dd28j_wa ).
      rc = rootnode->append_child( dd28j_node ).
    ENDLOOP.

    LOOP AT dd28v_tab INTO dd28v_wa.
      dd28v_node = xmldoc->create_element( 'dd28v' ).
      setattributesfromstructure( node = dd28v_node structure = dd28v_wa ).
      rc = rootnode->append_child( dd28v_node ).
    ENDLOOP.

*\--------------------------------------------------------------------/
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEWS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate  TYPE ddgotstate,
          dd25v_wa  TYPE dd25v,
          dd09l_wa  TYPE dd09v,
          dd26v_tab TYPE STANDARD TABLE OF dd26v,
          dd26v_wa  LIKE LINE OF dd26v_tab,
          dd27p_tab TYPE STANDARD TABLE OF dd27p,
          dd27p_wa  LIKE LINE OF dd27p_tab,
          dd28j_tab TYPE STANDARD TABLE OF dd28j,
          dd28j_wa  LIKE LINE OF dd28j_tab,
          dd28v_tab TYPE STANDARD TABLE OF dd28v,
          dd28v_wa  LIKE LINE OF dd28v_tab.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA dd09l_node TYPE REF TO if_ixml_element.
    DATA dd26v_node TYPE REF TO if_ixml_element.
    DATA dd27p_node TYPE REF TO if_ixml_element.
    DATA dd28j_node TYPE REF TO if_ixml_element.
    DATA dd28v_node TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _tablname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd25v_wa.

    objname = dd25v_wa-viewname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

    dd09l_node = xmldoc->find_from_name( 'dd09l' ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = dd09l_node
      CHANGING
        structure = dd09l_wa.

* retrieve Tabl details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd26v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd26v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd26v_wa.
      APPEND dd26v_wa TO dd26v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd27p' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd27p_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd27p_wa.
      APPEND dd27p_wa TO dd27p_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd28j' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd28j_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd28j_wa.
      APPEND dd28j_wa TO dd28j_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd28v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd28v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd28v_wa.
      APPEND dd28v_wa TO dd28v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = l_dd_objname
        dd25v_wa          = dd25v_wa
        dd09l_wa          = dd09l_wa
      TABLES
        dd26v_tab         = dd26v_tab
        dd27p_tab         = dd27p_tab
        dd28j_tab         = dd28j_tab
        dd28v_tab         = dd28v_tab
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEWS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEWS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'VIEW'.  "Views
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_table_types IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TYPES->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd40v_wa TYPE dd40v.
    l_name = objname.
    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd40v_wa      = dd40v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd40v_wa-typename IS NOT INITIAL.
      exists = 'X'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TYPES->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate   TYPE ddgotstate,
          dd40v_wa   TYPE dd40v,
          lt_dd42v   TYPE STANDARD TABLE OF dd42v WITH NON-UNIQUE DEFAULT KEY,
          ls_dd42v   LIKE LINE OF lt_dd42v,
          lt_dd43v   TYPE STANDARD TABLE OF dd43v WITH NON-UNIQUE DEFAULT KEY,
          ls_dd43v   LIKE LINE OF lt_dd43v,

          _objtype   TYPE string,

*xml nodes
          rootnode   TYPE REF TO if_ixml_element,
          dd42v_node TYPE REF TO if_ixml_element,
          dd43v_node TYPE REF TO if_ixml_element,
          rc         TYPE sysubrc,
          _ttypname  TYPE ddobjname.


    _ttypname  = objname.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = _ttypname
        langu         = sy-langu
      IMPORTING
        gotstate      = gotstate
        dd40v_wa      = dd40v_wa
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR dd40v_wa-typename IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = dd40v_wa ).

    LOOP AT lt_dd42v INTO ls_dd42v.
      dd42v_node = xmldoc->create_element( 'dd42v' ).
      setattributesfromstructure( node = dd42v_node structure = ls_dd42v ).
      rc = rootnode->append_child( dd42v_node ).
    ENDLOOP.

    LOOP AT lt_dd43v INTO ls_dd43v.
      dd43v_node = xmldoc->create_element( 'dd43v' ).
      setattributesfromstructure( node = dd43v_node structure = ls_dd43v ).
      rc = rootnode->append_child( dd43v_node ).
    ENDLOOP.

*\--------------------------------------------------------------------/
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TYPES->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate     TYPE ddgotstate,
          dd40v_wa     TYPE dd40v,
          lt_dd42v     TYPE STANDARD TABLE OF dd42v WITH NON-UNIQUE DEFAULT KEY,
          ls_dd42v     LIKE LINE OF lt_dd42v,
          lt_dd43v     TYPE STANDARD TABLE OF dd43v WITH NON-UNIQUE DEFAULT KEY,
          ls_dd43v     LIKE LINE OF lt_dd43v,

*xml nodes
          rootnode     TYPE REF TO if_ixml_element,
          dd42v_node   TYPE REF TO if_ixml_element,
          dd43v_node   TYPE REF TO if_ixml_element,
          node         TYPE REF TO if_ixml_element,
          filter       TYPE REF TO if_ixml_node_filter,
          iterator     TYPE REF TO if_ixml_node_iterator,
          rc           TYPE sysubrc,
          _ttypname    TYPE ddobjname,
          _devclass    TYPE devclass,
          checkexists  TYPE flag,
          _objtype     TYPE string,
* putting object into ddic
          l_pgmid      TYPE tadir-pgmid,
          l_object     TYPE tadir-object,
          l_obj_name   TYPE tadir-obj_name,
          l_dd_objname TYPE ddobjname,
          l_srcsystem  TYPE tadir-srcsystem,
          l_author     TYPE tadir-author,
          l_devclass   TYPE tadir-devclass,
          l_masterlang TYPE tadir-masterlang.


    _devclass   = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd40v_wa.

    objname = dd40v_wa-typename.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* retrieve table type details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd42v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CLEAR dd42v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_dd42v.
      APPEND ls_dd42v TO lt_dd42v.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd43v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CLEAR dd43v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_dd43v.
      APPEND ls_dd43v TO lt_dd43v.
      node ?= iterator->get_next( ).
    ENDWHILE.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = l_dd_objname
        dd40v_wa          = dd40v_wa
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.



    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_TYPES->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_TYPES->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014-2015 SAPlink project members                              |
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
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'TTYP'.  "Table Type
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_table_contents IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_CONTENTS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

* No implementation

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_CONTENTS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

    TYPES: BEGIN OF ttabname,
             tabname TYPE dd02v-tabname,
           END OF ttabname.

    DATA xtabname     TYPE ttabname.
    DATA xdd02v       TYPE dd02v.

    DATA root_node    TYPE REF TO if_ixml_element.
    DATA datarow_node TYPE REF TO if_ixml_element.
    DATA rc           TYPE sysubrc.
    DATA _tablname    TYPE ddobjname.
    DATA _objtype     TYPE string.

    DATA dref_tab TYPE REF TO data.
    DATA dref_wa  TYPE REF TO data.

    FIELD-SYMBOLS: <dyn_tab> TYPE table.
    FIELD-SYMBOLS: <dyn_wa>  TYPE any.

* Check that table exits.
    _tablname = objname.

* Does the table exist?
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = _tablname
      IMPORTING
        dd02v_wa      = xdd02v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR xdd02v-tabname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `Table not found`.
    ENDIF.

* Create parent node
    _objtype  = getobjecttype( ).
    root_node = xmldoc->create_element( _objtype ).
    xtabname-tabname = xdd02v-tabname.
    me->setattributesfromstructure( node = root_node structure = xtabname  ).

* Create dynamic internal table and work area
    CREATE DATA dref_tab TYPE TABLE OF (xdd02v-tabname).
    ASSIGN dref_tab->* TO <dyn_tab>.
    CREATE DATA dref_wa LIKE LINE OF <dyn_tab>.
    ASSIGN dref_wa->* TO <dyn_wa>.

* Select all data
    SELECT * INTO TABLE <dyn_tab> FROM (xdd02v-tabname).

* Write records to XML node
    LOOP AT <dyn_tab> ASSIGNING <dyn_wa>.
      datarow_node = xmldoc->create_element( `DataRow` ).
      me->setattributesfromstructure( node = datarow_node structure = <dyn_wa> ).
      rc = root_node->append_child( datarow_node ).
    ENDLOOP.

* Add node
    rc = xmldoc->append_child( root_node ).
    ixmldocument = xmldoc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_CONTENTS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

    TYPES: BEGIN OF ttabname,
             tabname TYPE dd02v-tabname,
           END OF ttabname.

    DATA xtabname    TYPE ttabname.
    DATA xdd02v      TYPE dd02v.
    DATA xtadir      TYPE tadir.

    DATA idd03p TYPE TABLE OF dd03p.
    DATA xdd03p LIKE LINE OF idd03p.

    DATA root_node        TYPE REF TO if_ixml_element.
    DATA datarow_node     TYPE REF TO if_ixml_element.
    DATA datarow_filter   TYPE REF TO if_ixml_node_filter.
    DATA datarow_iterator TYPE REF TO if_ixml_node_iterator.

    DATA _objtype           TYPE string.
    DATA l_answer           TYPE string.
    DATA l_nameclass        TYPE c.
    DATA l_client_dependent TYPE abap_bool.

    DATA dref_tab TYPE REF TO data.
    DATA dref_wa  TYPE REF TO data.

    FIELD-SYMBOLS: <dyn_tab>  TYPE table.
    FIELD-SYMBOLS: <dyn_wa>   TYPE any.
    FIELD-SYMBOLS: <fs_mandt> TYPE any.

    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    root_node = xmldoc->find_from_name( _objtype ).

* Get table name from XML.
    me->getstructurefromattributes(
            EXPORTING  node      = root_node
            CHANGING   structure = xtabname ).

    objname = xtabname-tabname.

* Check that table exists
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = xtabname-tabname
      IMPORTING
        dd02v_wa      = xdd02v
      TABLES
        dd03p_tab     = idd03p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR xdd02v-tabname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `Table not found`.
    ENDIF.

* Check for MANDT field, if found, then set client dependent
    READ TABLE idd03p INTO xdd03p WITH KEY fieldname = 'MANDT'.
    IF sy-subrc = 0.
      l_client_dependent = abap_true.
    ENDIF.

* Only allow tables in customer namespace
    CLEAR xtadir.
    SELECT SINGLE * FROM tadir INTO xtadir
                WHERE pgmid    = 'R3TR'
                  AND object   = 'TABL'
                  AND obj_name = xdd02v-tabname.
    CALL FUNCTION 'TRINT_OBJECT_NAMESPACE_INFO'
      EXPORTING
        iv_pgmid               = xtadir-pgmid
        iv_object              = xtadir-object
        iv_objname             = xtadir-obj_name
      IMPORTING
        ev_nameclass           = l_nameclass
      EXCEPTIONS
        namespace_not_existing = 1
        namespace_use_rejected = 2
        invalid_object         = 3
        OTHERS                 = 4.
    IF l_nameclass <> `C`.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Table is not within customer namespace'.
    ENDIF.

* Create dynamic internal table and work area
    CREATE DATA dref_tab TYPE TABLE OF (xdd02v-tabname).
    ASSIGN dref_tab->* TO <dyn_tab>.
    CREATE DATA dref_wa LIKE LINE OF <dyn_tab>.
    ASSIGN dref_wa->* TO <dyn_wa>.

* Build dynamic internal table from XML
    FREE: datarow_filter, datarow_iterator, datarow_node.
    datarow_filter = xmldoc->create_filter_name( `DataRow` ).
    datarow_iterator = xmldoc->create_iterator_filtered( datarow_filter ).
    datarow_node ?= datarow_iterator->get_next( ).
    WHILE datarow_node IS NOT INITIAL.
      APPEND INITIAL LINE TO <dyn_tab> ASSIGNING <dyn_wa>.
      me->getstructurefromattributes(
              EXPORTING   node      = datarow_node
              CHANGING    structure = <dyn_wa> ).
      datarow_node ?= datarow_iterator->get_next( ).
    ENDWHILE.

* Any records imported from XML, if not, give error.
    IF lines( <dyn_tab> ) = 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `No data records present in XML document`.
    ENDIF.

* Change MANDT field to current client number
* Always add imported records to current client number
    IF l_client_dependent = abap_true.
      LOOP AT <dyn_tab> ASSIGNING <dyn_wa>.
        ASSIGN COMPONENT `MANDT` OF STRUCTURE <dyn_wa> TO <fs_mandt>.
        <fs_mandt> = sy-mandt.
      ENDLOOP.
    ENDIF.

* Check that db table is initial, if so, then insert data and exit
    DATA: l_count TYPE i.
    CASE l_client_dependent .
      WHEN abap_true.
        SELECT COUNT( * )  INTO l_count
               FROM (xdd02v-tabname) CLIENT SPECIFIED
                       WHERE mandt = sy-mandt.
      WHEN abap_false.
        SELECT COUNT( * )  INTO l_count
               FROM (xdd02v-tabname).
    ENDCASE.
    IF l_count = 0.
      INSERT (xdd02v-tabname) FROM TABLE <dyn_tab>.
      name = objname.
      RETURN.
    ENDIF.

* Still here, then ask user how he wants to handle the existing
* data, either modify it, of delete/insert
    DATA: text_question TYPE string.

    text_question = `Table contains data which may be modified, ` &
                    `would you like to modify existing records, ` &
                    `or delete existing data first and insert`.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = text_question
        text_button_1  = 'Modify Data'      " UPdate table via MODIFY
        icon_button_1  = 'ICON_CHANGE'
        text_button_2  = 'Del/Ins Data'     " Delete data first, then INSERT
        icon_button_2  = 'ICON_DELETE'
      IMPORTING
        answer         = l_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
* Check answer
    CASE l_answer .
      WHEN  '1'.   "Modify existing rows, insert new rows based on key
        MODIFY (xdd02v-tabname) FROM TABLE <dyn_tab>.
      WHEN  '2'.   "Delete existing data first, then insert new data
        CASE l_client_dependent .
          WHEN abap_true.
            DELETE FROM (xdd02v-tabname) WHERE mandt = sy-mandt.
          WHEN abap_false.
            DELETE FROM (xdd02v-tabname).
        ENDCASE.
        INSERT (xdd02v-tabname) FROM TABLE <dyn_tab>.
      WHEN  'A'.   "Action has been cancelled
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Action Cancelled`.
    ENDCASE.

    name = objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_CONTENTS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

* No implementation

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_CONTENTS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

    objecttype = 'TABU'.  "Table Contents

  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_view_cluster IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_CLUSTER->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    SELECT SINGLE vclname INTO objname FROM vcldir WHERE vclname = objname.
    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_VIEW_CLUSTER->CHECK_AUTHORITY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_authority.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
      EXPORTING
        view_action                    = 'U'
        view_name                      = 'V_VCLDIR'
        no_warning_for_clientindep     = 'X'
      EXCEPTIONS
        no_authority                   = 1
        no_clientindependent_authority = 2
        OTHERS                         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_authorized.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_CLUSTER->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    DATA lv_vclname             TYPE vcldir-vclname.
    DATA ls_vcldir              TYPE v_vcldir.
    DATA lt_vclstruc            TYPE TABLE OF v_vclstruc.
    DATA lt_vclstrudep          TYPE TABLE OF vclstrudep.
    DATA lt_vcltab              TYPE TABLE OF vclmf.
    DATA ls_vclstruc            TYPE v_vclstruc.
    DATA ls_vclstrudep          TYPE vclstrudep.
    DATA ls_vcltab              TYPE vclmf.
    DATA rc                     TYPE sysubrc.               "#EC NEEDED

    DATA _objtype               TYPE string.
    DATA rootnode               TYPE REF TO if_ixml_element.
    DATA node                   TYPE REF TO if_ixml_element.



    " Read view cluster info
    lv_vclname = objname.
    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vcltab
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.


    " Create XML
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = ls_vcldir ).

    LOOP AT lt_vclstruc INTO ls_vclstruc.
      node = xmldoc->create_element( 'vclstruc' ).
      setattributesfromstructure( node = node structure = ls_vclstruc ).
      rc = rootnode->append_child( node ).
    ENDLOOP.

    LOOP AT lt_vclstrudep INTO ls_vclstrudep.
      node = xmldoc->create_element( 'vclstrudep' ).
      setattributesfromstructure( node = node structure = ls_vclstrudep ).
      rc = rootnode->append_child( node ).
    ENDLOOP.

    LOOP AT lt_vcltab INTO ls_vcltab.
      node = xmldoc->create_element( 'vcltab' ).
      setattributesfromstructure( node = node structure = ls_vcltab ).
      rc = rootnode->append_child( node ).
    ENDLOOP.

    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_CLUSTER->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    DATA lv_vclname             TYPE vcldir-vclname.
    DATA ls_vcldir              TYPE v_vcldir.
    DATA lt_vclstruc            TYPE TABLE OF v_vclstruc.
    DATA lt_vclstrudep          TYPE TABLE OF vclstrudep.
    DATA lt_vcltab              TYPE TABLE OF vclmf.
    DATA ls_vclstruc            TYPE v_vclstruc.
    DATA ls_vclstrudep          TYPE vclstrudep.
    DATA ls_vcltab              TYPE vclmf.
    DATA checkexists            TYPE flag.
    DATA lv_trkorr              TYPE trkorr.

    DATA _objtype               TYPE string.
    DATA rootnode               TYPE REF TO if_ixml_element.
    DATA node                   TYPE REF TO if_ixml_element.
    DATA filter                 TYPE REF TO if_ixml_node_filter.
    DATA iterator               TYPE REF TO if_ixml_node_iterator.



    _objtype = getobjecttype( ).
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = ls_vcldir.

    objname = ls_vcldir-vclname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.                                                 "#EC NEEDED
        " Object will be overwritten automatically
        " No need for deletion
      ENDIF.
    ENDIF.


    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'vclstruc' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR ls_vclstruc.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_vclstruc.
      APPEND ls_vclstruc TO lt_vclstruc.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'vclstrudep' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR ls_vclstrudep.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_vclstrudep.
      APPEND ls_vclstrudep TO lt_vclstrudep.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'vcltab' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR ls_vcltab.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_vcltab.
      APPEND ls_vcltab TO lt_vcltab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    check_authority( ).
    enqueue( action = 'E' ).
    lv_trkorr = create_transport( ).

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vcltab.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_korrnum       = lv_trkorr
        iv_objectname    = ls_vcldir-vclname
        iv_objecttype    = 'C'
        iv_maint_mode    = 'I'
      EXCEPTIONS
        illegal_call     = 1
        object_not_found = 2
        generate_error   = 3
        transport_error  = 4
        OTHERS           = 5.

    IF sy-subrc = 0.
      " successful install
      enqueue( action = 'D' ).
      name = ls_vcldir-vclname.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_VIEW_CLUSTER->CREATE_TRANSPORT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TRKORR                      TYPE        TRKORR
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_transport.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    DATA lt_ko200 TYPE TABLE OF ko200.
    DATA ls_ko200 TYPE ko200.
    DATA lv_trkorr  TYPE trkorr.


    ls_ko200-pgmid = 'R3TR'.
    ls_ko200-object = 'VCLS'.
    ls_ko200-obj_name = objname.
    APPEND ls_ko200 TO lt_ko200.

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      IMPORTING
        we_order                = lv_trkorr
      TABLES
        wt_ko200                = lt_ko200
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      EXPORTING
        wi_order                = lv_trkorr
      IMPORTING
        we_order                = rv_trkorr
      TABLES
        wt_ko200                = lt_ko200
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEW_CLUSTER->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.                                      "#EC NEEDED
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

* Do need to delete -> Object will be regenerated

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_VIEW_CLUSTER->ENQUEUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] ACTION                         TYPE        C
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enqueue.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    DATA: vcl_sellist TYPE TABLE OF vimsellist.
    DATA: vcl_sel     TYPE vimsellist.
    DATA: lv_error    TYPE string.
    DATA: lv_subrc    TYPE sysubrc.

    REFRESH vcl_sellist. CLEAR vcl_sellist.
    vcl_sel-viewfield = 'VCLNAME'.
    vcl_sel-operator  = 'EQ'.
    vcl_sel-value     = objname.
    vcl_sel-ddic      = 'S'.
    APPEND vcl_sel TO vcl_sellist.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action         = action
        enqueue_mode   = 'E'
        view_name      = 'V_VCLDIR'
        enqueue_range  = 'X'
      TABLES
        sellist        = vcl_sellist
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
    IF sy-subrc <> 0 AND action = 'E'.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
        WHEN 2.
          MESSAGE e050(sv) WITH 'V_VCLDIR' INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action         = action
        enqueue_mode   = 'E'
        view_name      = 'V_VCLSTRUC'
        enqueue_range  = 'X'
      TABLES
        sellist        = vcl_sellist
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
    IF sy-subrc <> 0 AND action = 'E'.
      lv_subrc = sy-subrc.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLDIR'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.

      CASE lv_subrc.
        WHEN 1.
          MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
        WHEN 2.
          MESSAGE e050(sv) WITH 'V_VCLSTRUC' INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action         = action
        enqueue_mode   = 'E'
        view_name      = 'V_VCLSTDEP'
        enqueue_range  = 'X'
      TABLES
        sellist        = vcl_sellist
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
    IF sy-subrc <> 0 AND action = 'E'.
      lv_subrc = sy-subrc.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLDIR'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLSTRUC'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.

      CASE lv_subrc.
        WHEN 1.
          MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
        WHEN 2.
          MESSAGE e050(sv) WITH 'V_VCLSTDEP' INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action         = action
        enqueue_mode   = 'E'
        view_name      = 'V_VCLMF'
        enqueue_range  = 'X'
      TABLES
        sellist        = vcl_sellist
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
    IF sy-subrc <> 0 AND action = 'E'.
      lv_subrc = sy-subrc.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLDIR'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLSTRUC'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.
      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          action        = 'D'
          enqueue_mode  = 'E'
          view_name     = 'V_VCLSTDEP'
          enqueue_range = 'X'
        TABLES
          sellist       = vcl_sellist.
      CASE lv_subrc.
        WHEN 1.
          MESSAGE e049(sv) WITH sy-msgv1 INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
        WHEN 2.
          MESSAGE e050(sv) WITH 'V_VCLMF' INTO lv_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lv_error.
      ENDCASE.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEW_CLUSTER->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

    " Plugin created by:
    " Nicolas Busson
    " bussonnicolas@gmail.com

    objecttype = 'VCLS'. " View cluster

  ENDMETHOD.
ENDCLASS.


CLASS zsaplink_view_tech_settings IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_TECH_SETTINGS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd09l_wa TYPE dd09l.
    l_name = objname.

    CALL FUNCTION 'DDIF_VIET_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd09l_wa      = dd09l_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd09l_wa-tabname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_TECH_SETTINGS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate TYPE ddgotstate,
          dd09l_wa TYPE dd09l.

*xml nodes
    DATA rootnode   TYPE REF TO if_ixml_element.
    DATA rc         TYPE sysubrc.
    DATA _tabtname  TYPE ddobjname.
    _tabtname = objname.

    CALL FUNCTION 'DDIF_VIET_GET'
      EXPORTING
        name          = _tabtname
        langu         = sy-langu
      IMPORTING
        gotstate      = gotstate
        dd09l_wa      = dd09l_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR dd09l_wa-tabname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    DATA _objtype TYPE string.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = dd09l_wa ).

*\--------------------------------------------------------------------/
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_VIEW_TECH_SETTINGS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate TYPE ddgotstate,
          dd09l_wa TYPE dd09l.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA rc          TYPE sysubrc.
    DATA _tabtname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd09l_wa.

    objname = dd09l_wa-tabname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_VIET_PUT'
      EXPORTING
        name              = l_dd_objname
        dd09l_wa          = dd09l_wa
      EXCEPTIONS
        viet_not_found    = 1
        name_inconsistent = 2
        viet_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEW_TECH_SETTINGS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_VIEW_TECH_SETTINGS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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

*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'VIET'.  "View Technical Settings
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_table_tech_settings IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TECH_SETTINGS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

    DATA: l_name   TYPE ddobjname,
          dd09l_wa TYPE dd09l.
    l_name = objname.

    CALL FUNCTION 'DDIF_TABT_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd09l_wa      = dd09l_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd09l_wa-tabname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TECH_SETTINGS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

    DATA: gotstate TYPE ddgotstate,
          dd09l_wa TYPE dd09l.

*xml nodes
    DATA rootnode   TYPE REF TO if_ixml_element.
    DATA rc         TYPE sysubrc.
    DATA _tabtname  TYPE ddobjname.
    _tabtname = objname.

    CALL FUNCTION 'DDIF_TABT_GET'
      EXPORTING
        name          = _tabtname
      IMPORTING
        gotstate      = gotstate
        dd09l_wa      = dd09l_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR dd09l_wa-tabname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    DATA _objtype TYPE string.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = dd09l_wa ).

*\--------------------------------------------------------------------/
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLE_TECH_SETTINGS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

    DATA: gotstate TYPE ddgotstate,
          dd09l_wa TYPE dd09l.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA rc          TYPE sysubrc.
    DATA _tabtname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd09l_wa.

    objname = dd09l_wa-tabname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_TABT_PUT'
      EXPORTING
        name              = l_dd_objname
        dd09l_wa          = dd09l_wa
      EXCEPTIONS
        tabt_not_found    = 1
        name_inconsistent = 2
        tabt_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_TECH_SETTINGS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLE_TECH_SETTINGS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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
    objecttype = 'TABT'.  "Table Technical Settings
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_tables IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLES->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: l_name   TYPE ddobjname,
          dd02v_wa TYPE dd02v.
    l_name = objname.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = l_name
      IMPORTING
        dd02v_wa      = dd02v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0 AND dd02v_wa-tabname IS NOT INITIAL.
      exists = 'X'.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLES->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate  TYPE ddgotstate,
          dd02v_wa  TYPE dd02v,
          dd09l_wa  TYPE dd09v,
          dd03p_tab TYPE STANDARD TABLE OF dd03p,
          dd03p_wa  LIKE LINE OF dd03p_tab,
          dd05m_tab TYPE STANDARD TABLE OF dd05m,
          dd05m_wa  LIKE LINE OF dd05m_tab,
          dd08v_tab TYPE STANDARD TABLE OF dd08v,
          dd08v_wa  LIKE LINE OF dd08v_tab,
          dd12v_tab TYPE STANDARD TABLE OF dd12v,
          dd12v_wa  LIKE LINE OF dd12v_tab,
          dd17v_tab TYPE STANDARD TABLE OF dd17v,
          dd17v_wa  LIKE LINE OF dd17v_tab,
          dd35v_tab TYPE STANDARD TABLE OF dd35v,
          dd35v_wa  LIKE LINE OF dd35v_tab,
          dd36m_tab TYPE STANDARD TABLE OF dd36m,
          dd36m_wa  LIKE LINE OF dd36m_tab.

*xml nodes
    DATA rootnode   TYPE REF TO if_ixml_element.
    DATA dd09l_node TYPE REF TO if_ixml_element.
    DATA dd03p_node TYPE REF TO if_ixml_element.
    DATA dd05m_node TYPE REF TO if_ixml_element.
    DATA dd08v_node TYPE REF TO if_ixml_element.
    DATA dd12v_node TYPE REF TO if_ixml_element.
    DATA dd17v_node TYPE REF TO if_ixml_element.
    DATA dd35v_node TYPE REF TO if_ixml_element.
    DATA dd36m_node TYPE REF TO if_ixml_element.
    DATA rc         TYPE sysubrc.
    DATA _tablname  TYPE ddobjname.
    _tablname = objname.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = _tablname
        langu         = sy-langu
      IMPORTING
        gotstate      = gotstate
        dd02v_wa      = dd02v_wa
        dd09l_wa      = dd09l_wa
      TABLES
        dd03p_tab     = dd03p_tab
        dd05m_tab     = dd05m_tab
        dd08v_tab     = dd08v_tab
        dd12v_tab     = dd12v_tab
        dd17v_tab     = dd17v_tab
        dd35v_tab     = dd35v_tab
        dd36m_tab     = dd36m_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0 OR dd02v_wa-tabname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Create parent node
    DATA _objtype TYPE string.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    setattributesfromstructure( node = rootnode structure = dd02v_wa ).

    dd09l_node = xmldoc->create_element( 'dd09l' ).
    setattributesfromstructure( node = dd09l_node structure = dd09l_wa ).
    rc = rootnode->append_child( dd09l_node ).

    LOOP AT dd03p_tab INTO dd03p_wa.
      dd03p_node = xmldoc->create_element( 'dd03p' ).
      setattributesfromstructure( node = dd03p_node structure = dd03p_wa ).
      rc = rootnode->append_child( dd03p_node ).
    ENDLOOP.

    LOOP AT dd05m_tab INTO dd05m_wa.
      dd05m_node = xmldoc->create_element( 'dd05m' ).
      setattributesfromstructure( node = dd05m_node structure = dd05m_wa ).
      rc = rootnode->append_child( dd05m_node ).
    ENDLOOP.

    LOOP AT dd08v_tab INTO dd08v_wa.
      dd08v_node = xmldoc->create_element( 'dd08v' ).
      setattributesfromstructure( node = dd08v_node structure = dd08v_wa ).
      rc = rootnode->append_child( dd08v_node ).
    ENDLOOP.

    LOOP AT dd12v_tab INTO dd12v_wa.
      dd12v_node = xmldoc->create_element( 'dd12v' ).
      setattributesfromstructure( node = dd12v_node structure = dd12v_wa ).
      rc = rootnode->append_child( dd12v_node ).
    ENDLOOP.

    LOOP AT dd17v_tab INTO dd17v_wa.
      dd17v_node = xmldoc->create_element( 'dd17v' ).
      setattributesfromstructure( node = dd17v_node structure = dd17v_wa ).
      rc = rootnode->append_child( dd17v_node ).
    ENDLOOP.

    LOOP AT dd35v_tab INTO dd35v_wa.
      dd35v_node = xmldoc->create_element( 'dd35v' ).
      setattributesfromstructure( node = dd35v_node structure = dd35v_wa ).
      rc = rootnode->append_child( dd35v_node ).
    ENDLOOP.

    LOOP AT dd36m_tab INTO dd36m_wa.
      dd36m_node = xmldoc->create_element( 'dd36m' ).
      setattributesfromstructure( node = dd36m_node structure = dd36m_wa ).
      rc = rootnode->append_child( dd36m_node ).
    ENDLOOP.

*\--------------------------------------------------------------------/
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_TABLES->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    DATA: gotstate  TYPE ddgotstate,
          dd02v_wa  TYPE dd02v,
          dd09l_wa  TYPE dd09v,
          dd03p_tab TYPE STANDARD TABLE OF dd03p,
          dd03p_wa  LIKE LINE OF dd03p_tab,
          dd05m_tab TYPE STANDARD TABLE OF dd05m,
          dd05m_wa  LIKE LINE OF dd05m_tab,
          dd08v_tab TYPE STANDARD TABLE OF dd08v,
          dd08v_wa  LIKE LINE OF dd08v_tab,
          dd12v_tab TYPE STANDARD TABLE OF dd12v,
          dd12v_wa  LIKE LINE OF dd12v_tab,
          dd17v_tab TYPE STANDARD TABLE OF dd17v,
          dd17v_wa  LIKE LINE OF dd17v_tab,
          dd35v_tab TYPE STANDARD TABLE OF dd35v,
          dd35v_wa  LIKE LINE OF dd35v_tab,
          dd36m_tab TYPE STANDARD TABLE OF dd36m,
          dd36m_wa  LIKE LINE OF dd36m_tab.

*xml nodes
    DATA rootnode    TYPE REF TO if_ixml_element.
    DATA dd09l_node  TYPE REF TO if_ixml_element.
    DATA dd03p_node  TYPE REF TO if_ixml_element.
    DATA dd05m_node  TYPE REF TO if_ixml_element.
    DATA dd08v_node  TYPE REF TO if_ixml_element.
    DATA dd12v_node  TYPE REF TO if_ixml_element.
    DATA dd17v_node  TYPE REF TO if_ixml_element.
    DATA dd35v_node  TYPE REF TO if_ixml_element.
    DATA dd36m_node  TYPE REF TO if_ixml_element.
    DATA node        TYPE REF TO if_ixml_element.
    DATA filter      TYPE REF TO if_ixml_node_filter.
    DATA iterator    TYPE REF TO if_ixml_node_iterator.
    DATA rc          TYPE sysubrc.
    DATA _tablname   TYPE ddobjname.
    DATA _devclass   TYPE devclass.
    DATA checkexists TYPE flag.
    DATA _objtype    TYPE string.
    DATA _objname    TYPE string.

    _devclass = devclass.
    _objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = dd02v_wa.

    objname = dd02v_wa-tabname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

    dd09l_node = xmldoc->find_from_name( 'dd09l' ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = dd09l_node
      CHANGING
        structure = dd09l_wa.

* retrieve Tabl details
    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd03p' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd03p_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd03p_wa.
      APPEND dd03p_wa TO dd03p_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd05m' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd05m_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd05m_wa.
      APPEND dd05m_wa TO dd05m_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd08v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd08v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd08v_wa.
      APPEND dd08v_wa TO dd08v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd12v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd12v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd12v_wa.
      APPEND dd12v_wa TO dd12v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd17v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd17v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd17v_wa.
      APPEND dd17v_wa TO dd17v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd35v' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd35v_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd35v_wa.
      APPEND dd35v_wa TO dd35v_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    FREE: filter, iterator, node.
    filter = xmldoc->create_filter_name( 'dd36m' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR dd36m_node.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = dd36m_wa.
      APPEND dd36m_wa TO dd36m_tab.
      node ?= iterator->get_next( ).
    ENDWHILE.

    DATA : l_pgmid      TYPE tadir-pgmid,
           l_object     TYPE tadir-object,
           l_obj_name   TYPE tadir-obj_name,
           l_dd_objname TYPE ddobjname,
           l_srcsystem  TYPE tadir-srcsystem,
           l_author     TYPE tadir-author,
           l_devclass   TYPE tadir-devclass,
           l_masterlang TYPE tadir-masterlang.


    l_pgmid      = 'R3TR'.
    l_object     = _objtype.
    l_obj_name   = objname.
    l_dd_objname = objname.
    l_srcsystem  = sy-sysid.
    l_author     = sy-uname.
    l_devclass   = _devclass.
    l_masterlang = sy-langu.

    DATA: itadir TYPE tadir.
    itadir-pgmid      = l_pgmid.
    itadir-object     = l_object.
    itadir-obj_name   = l_obj_name.
    itadir-srcsystem  = l_srcsystem.
    itadir-author     = l_author.
    itadir-devclass   = l_devclass.
    itadir-masterlang = l_masterlang.
    MODIFY tadir FROM itadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = ' '
        wi_delete_tadir_entry          = 'X'
        wi_tadir_pgmid                 = l_pgmid
        wi_tadir_object                = l_object
        wi_tadir_obj_name              = l_obj_name
        wi_tadir_srcsystem             = l_srcsystem
        wi_tadir_author                = l_author
        wi_tadir_devclass              = l_devclass
        wi_tadir_masterlang            = l_masterlang
        iv_set_edtflag                 = ''
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1 OR 9 OR 7 OR 8. "OK! - Doesn't exist yet
        WHEN 11 OR 23 OR 24.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 22.
          _objname = l_obj_name.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>locked
              object = _objname.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = l_dd_objname
        dd02v_wa          = dd02v_wa
        dd09l_wa          = dd09l_wa
      TABLES
        dd03p_tab         = dd03p_tab
        dd05m_tab         = dd05m_tab
        dd08v_tab         = dd08v_tab
        dd35v_tab         = dd35v_tab
        dd36m_tab         = dd36m_tab
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

    DATA: trobjtype  TYPE trobjtype,
          trobj_name TYPE trobj_name.
    trobjtype  = l_object.
    trobj_name = l_obj_name.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = trobjtype
        obj_name          = trobj_name
      EXCEPTIONS
        wrong_object_name = 1.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLES->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_TABLES->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
*      Plugin created by:
*      Thomas Jung
*      thomas.jung1@gmail.com

    objecttype = 'TABL'.  "Tables and Structures
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_message_class IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_MESSAGE_CLASS->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists .
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

*      Plugin created by:
*      John Patterson
*      patterjo@gmail.com

    DATA: ls_t100a TYPE t100a,
          lv_msgid TYPE msgid.

    lv_msgid =  objname.

    SELECT SINGLE *
      INTO ls_t100a
      FROM t100a
      WHERE arbgb = lv_msgid.

    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_MESSAGE_CLASS->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject .
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

*      Plugin created by:
*      John Patterson
*      patterjo@gmail.com

    DATA: lt_t100  TYPE STANDARD TABLE OF t100,
          lt_t100t TYPE STANDARD TABLE OF t100t,
          ls_t100  LIKE LINE OF lt_t100,
          ls_t100t LIKE LINE OF lt_t100t,
          ls_t100a TYPE t100a.

    DATA: lv_msgid   TYPE msgid,
          lv_rc      TYPE sysubrc,
          lv_objtype TYPE string.

*xml nodes
    DATA: lr_rootnode   TYPE REF TO if_ixml_element,
          lr_t100_node  TYPE REF TO if_ixml_element,
          lr_t100t_node TYPE REF TO if_ixml_element.

    lv_msgid = objname.

    SELECT SINGLE *
     INTO ls_t100a
     FROM t100a WHERE arbgb = lv_msgid.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check          = 'X'
        global_lock              = space
        mode                     = 'SHOW'
        object                   = lv_msgid
        object_class             = 'T100'
      EXCEPTIONS
        canceled_in_corr         = 01
        enqueued_by_user         = 02
        enqueue_system_failure   = 03
        illegal_parameter_values = 04
        locked_by_author         = 05
        no_modify_permission     = 06
        no_show_permission       = 07
        permission_failure       = 08.
    CASE sy-subrc.
      WHEN 0.
      WHEN 2 OR 5.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>locked.
      WHEN 6 OR 7 OR 8 OR 9.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.

*--- get messages for all maintained languages
    SELECT *
     INTO TABLE lt_t100
     FROM t100
     WHERE arbgb = lv_msgid.

*--- get text for t100a
    SELECT  *
     INTO TABLE lt_t100t
     FROM t100t
     WHERE arbgb = lv_msgid.

*-- Create parent node
    lv_objtype = getobjecttype( ).
    lr_rootnode = xmldoc->create_element( lv_objtype ).
    setattributesfromstructure( node = lr_rootnode
                                structure = ls_t100a ).

*--- Create Elements for Messages
    LOOP AT lt_t100 INTO ls_t100.
      lr_t100_node = xmldoc->create_element( 't100' ).
      setattributesfromstructure( node = lr_t100_node
                                  structure = ls_t100 ).
      lv_rc = lr_rootnode->append_child( lr_t100_node ).
    ENDLOOP.

*--- Create Elements for Texts
    LOOP AT lt_t100t INTO ls_t100t.
      lr_t100t_node = xmldoc->create_element( 't100t' ).
      setattributesfromstructure( node = lr_t100t_node
                                  structure = ls_t100t ).
      lv_rc = lr_rootnode->append_child( lr_t100t_node ).
    ENDLOOP.

    lv_rc = xmldoc->append_child( lr_rootnode ).
    ixmldocument = xmldoc.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_MESSAGE_CLASS->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc .
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

*      Plugin created by:
*      John Patterson
*      patterjo@gmail.com

    DATA: lt_t100  TYPE STANDARD TABLE OF t100,
          lt_t100t TYPE STANDARD TABLE OF t100t,
          ls_t100  LIKE LINE OF lt_t100,
          ls_t100t LIKE LINE OF lt_t100t,
          ls_t100a TYPE t100a.

    DATA: lv_msgid       TYPE msgid,
          lv_rc          TYPE sysubrc,
          lv_objtype     TYPE string,
          lv_checkexists TYPE char1,
          lv_len         TYPE i.

*--- xml data objects
    DATA: lr_rootnode   TYPE REF TO if_ixml_element,
          lr_t100_node  TYPE REF TO if_ixml_element,
          lr_t100t_node TYPE REF TO if_ixml_element,
          lr_filter     TYPE REF TO if_ixml_node_filter,
          lr_iterator   TYPE REF TO if_ixml_node_iterator.

    lv_msgid = objname.
    lv_objtype = getobjecttype( ).

    xmldoc = ixmldocument.
    lr_rootnode = xmldoc->find_from_name( lv_objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = lr_rootnode
      CHANGING
        structure = ls_t100a.

    objname = ls_t100a-arbgb.

    lv_checkexists = checkexists( ).
    IF lv_checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
        deleteobject( ).
      ENDIF.
    ENDIF.

*--- Read Elements for Messages
    FREE: lr_filter, lr_iterator, lr_t100_node, lr_t100t_node.
    lr_filter = xmldoc->create_filter_name( 't100' ).
    lr_iterator = xmldoc->create_iterator_filtered( lr_filter ).
    lr_t100_node ?= lr_iterator->get_next( ).

    WHILE lr_t100_node IS NOT INITIAL.
      CLEAR ls_t100.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = lr_t100_node
        CHANGING
          structure = ls_t100.
      APPEND ls_t100 TO lt_t100.
      lr_t100_node ?= lr_iterator->get_next( ).
    ENDWHILE.

*--- Create Elements for Texts
    FREE: lr_filter, lr_iterator, lr_t100_node, lr_t100t_node.
    lr_filter = xmldoc->create_filter_name( 't100t' ).
    lr_iterator = xmldoc->create_iterator_filtered( lr_filter ).
    lr_t100t_node ?= lr_iterator->get_next( ).

    WHILE lr_t100t_node IS NOT INITIAL.
      CLEAR ls_t100t.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = lr_t100t_node
        CHANGING
          structure = ls_t100t.
      APPEND ls_t100t TO lt_t100t.
      lr_t100t_node ?= lr_iterator->get_next( ).
    ENDWHILE.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = lv_msgid
        object_class = 'T100'.

*--- Check permission
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check          = 'X'
        global_lock              = 'X'
        mode                     = 'INSERT'
        language_upd_exit        = 'UPDATE_MASTER'
        master_language          = ls_t100a-masterlang
        object                   = lv_msgid
        object_class             = 'T100'
        suppress_language_check  = ' '
      EXCEPTIONS
        canceled_in_corr         = 01
        enqueued_by_user         = 02
        enqueue_system_failure   = 03
        illegal_parameter_values = 04
        locked_by_author         = 05
        no_modify_permission     = 06
        no_show_permission       = 07
        permission_failure       = 08.
    CASE sy-subrc.
      WHEN 0.
      WHEN 2 OR 5.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>locked.
      WHEN 6 OR 7 OR 8 OR 9.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author              = sy-uname
        global_lock         = 'X'
        object              = objname
        object_class        = 'T100'
        devclass            = devclass
        master_language     = sy-langu
        mode                = 'INSERT'
*       IMPORTING
*       AUTHOR              = UNAME
*       KORRNUM             = CORRNUMBER_LOCAL
*       DEVCLASS            = DEVCLASS_LOCAL
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

*--- update message tables
    CLEAR: ls_t100a-applclass.

    ls_t100a-lastuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.

    MODIFY t100a FROM ls_t100a.
    MODIFY t100 FROM TABLE lt_t100.
    MODIFY t100t FROM TABLE lt_t100t.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = lv_msgid
        operation = 'INSERT'
        type      = 'CN'.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = lv_msgid
        object_class = 'T100'.

    name = objname.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_MESSAGE_CLASS->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject .
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
*      Plugin created by:
*      John Patterson
*      patterjo@gmail.com
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_MESSAGE_CLASS->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype .
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

*      Plugin created by:
*      John Patterson
*      patterjo@gmail.com

    objecttype = 'MSAG'. "Message Class
  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_functiongroup IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->ACTUALIZE_OBJECT_TREE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD actualize_object_tree.
    DATA: l_offset TYPE i.
    DATA: l_tree_string TYPE string.

    CONCATENATE 'PG_' 'SAPL' objname INTO l_tree_string.

* If we supported namespaces, the following code would be required
*  FIND ALL OCCURRENCES OF '/' IN objname MATCH OFFSET l_offset.
*  IF sy-subrc = 0.
*    l_tree_string  = objname.
*    REPLACE SECTION OFFSET l_offset LENGTH 1 OF  l_tree_string  WITH '/SAPL'.
*    CONCATENATE 'PG_' l_tree_string  INTO l_tree_string.
*  ELSE.
*    CONCATENATE 'PG_' 'SAPL' objname INTO l_tree_string.
*  ENDIF.

    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = l_tree_string.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_FUNCTIONGROUP->CHECKEXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EXISTS                         TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD checkexists.
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

    SELECT SINGLE area FROM tlibg INTO objname WHERE area = objname.
    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_FUNCTIONGROUP->CREATEIXMLDOCFROMOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createixmldocfromobject.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

    TYPES: BEGIN OF t_tlibt,
             area  TYPE tlibt-area,
             spras TYPE tlibt-spras,
             areat TYPE tlibt-areat,
           END OF t_tlibt.

    DATA rootnode            TYPE REF TO if_ixml_element.
    DATA mainprognode        TYPE REF TO if_ixml_element.
    DATA includesnode        TYPE REF TO if_ixml_element.
    DATA functgroupnode      TYPE REF TO if_ixml_element.
    DATA functionmodulesnode TYPE REF TO if_ixml_element.
    DATA docnode             TYPE REF TO if_ixml_element.
    DATA textpoolnode        TYPE REF TO if_ixml_element.
    DATA dynpronode          TYPE REF TO if_ixml_element.
    DATA statusnode          TYPE REF TO if_ixml_element.
    DATA sourcenode          TYPE REF TO if_ixml_element.
    DATA fmdocumenation      TYPE REF TO if_ixml_element.

    DATA rc                TYPE sysubrc.
    DATA progattribs       TYPE trdir.
    DATA progsource        TYPE rswsourcet.
    DATA _objname(30)      TYPE c.
    DATA sourcestring      TYPE string.
    DATA _objtype          TYPE string.
    DATA functiongroupname TYPE  tlibg-area.
    DATA mainfgprogname    TYPE sy-repid.
    DATA l_offset          TYPE i.
    DATA xtlibt            TYPE t_tlibt.

    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).

* function groups in reserved namespace, not supported.
    IF objname(1) = '/'.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Function Groups in / namespace are not supported'.
    ENDIF.

*  create main program name.  Other namespaces are not supported
    CONCATENATE 'SAPL' objname INTO mainfgprogname.

* If we did support namespaces, this is how we would
* build the main program name
*  FIND ALL OCCURRENCES OF '/' IN objname MATCH OFFSET l_offset.
*  IF sy-subrc = 0.
*    mainfgprogname = objname.
*    REPLACE SECTION OFFSET l_offset LENGTH 1 OF mainfgprogname WITH '/SAPL'.
*  ELSE.
*    CONCATENATE 'SAPL' objname INTO mainfgprogname.
*  ENDIF.

* Set function group name
    functiongroupname = objname.

* Get main program attributes
    SELECT SINGLE * FROM trdir
             INTO progattribs
                  WHERE name = mainfgprogname.
    IF sy-subrc <> 0.
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

* Get Function group attributes
    CLEAR xtlibt.
    SELECT SINGLE * FROM tlibt
               INTO CORRESPONDING FIELDS OF xtlibt
                       WHERE spras = sy-langu
                         AND area  = functiongroupname.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found.
    ENDIF.

    setattributesfromstructure( node = rootnode
                                structure =  xtlibt  ).

    _objname = objname.
    objname  = mainfgprogname.    " Main program is object

* Write main program for function group.
    mainprognode = xmldoc->create_element( 'mainprogram' ).
    setattributesfromstructure( node = mainprognode
                                structure =   progattribs  ).

    sourcenode = xmldoc->create_element( 'source' ).
    READ REPORT mainfgprogname INTO progsource.
    sourcestring = buildsourcestring( sourcetable = progsource ).
    rc = sourcenode->if_ixml_node~set_value( sourcestring ).

    textpoolnode =  get_textpool( ).
    rc = mainprognode->append_child( textpoolnode ).

    docnode = get_documentation( ).
    rc = rootnode->append_child( docnode ).

    dynpronode = get_dynpro( ).
    rc = mainprognode->append_child( dynpronode ).

    statusnode =  get_pfstatus( ).
    rc = mainprognode->append_child( statusnode ).

    rc = mainprognode->append_child( sourcenode ).
    rc = rootnode->append_child( mainprognode ).

* Get the includes
    includesnode = get_includes( main_prog = mainfgprogname
                                 fct_group = functiongroupname ).
    rc = rootnode->append_child( includesnode ).

* Get function modules data.
    functionmodulesnode = get_function_modules( functiongroupname ).
    rc = rootnode->append_child( functionmodulesnode ).

    rc = xmldoc->append_child( rootnode ).

    ixmldocument = xmldoc.
    objname      =  _objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZSAPLINK_FUNCTIONGROUP->CREATEOBJECTFROMIXMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXMLDOCUMENT                   TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* | [--->] OVERWRITE                      TYPE        FLAG(optional)
* | [<-()] NAME                           TYPE        STRING
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD createobjectfromixmldoc.
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

*      Plugin created by:
*      Rich Heilman
*      rich.heilman.jr@gmail.com

    TYPES: BEGIN OF t_tlibt,
             area  TYPE tlibt-area,
             spras TYPE tlibt-spras,
             areat TYPE tlibt-areat,
           END OF t_tlibt.

    DATA rootnode      TYPE REF TO if_ixml_element.
    DATA sourcenode    TYPE REF TO if_ixml_element.
    DATA textnode      TYPE REF TO if_ixml_element.
    DATA docnode       TYPE REF TO if_ixml_element.
    DATA dynpnode      TYPE REF TO if_ixml_element.
    DATA statnode      TYPE REF TO if_ixml_element.

    DATA mainprog_node        TYPE REF TO if_ixml_element.
    DATA functionmodule_node  TYPE REF TO if_ixml_element.
    DATA functionmodules_node TYPE REF TO if_ixml_element.
    DATA includes_node        TYPE REF TO if_ixml_element.
    DATA fmdoc_node      TYPE REF TO if_ixml_element.

    DATA progattribs   TYPE trdir.
    DATA source        TYPE string.
    DATA sourcetable   TYPE table_of_strings.
    DATA _objname(30)  TYPE c.
    DATA _objtype      TYPE string.
    DATA checkexists   TYPE flag.

    DATA xtlibt TYPE t_tlibt.
    DATA xstext TYPE tftit-stext.

    DATA functiongroupname TYPE  tlibg-area.

    _objtype = getobjecttype( ).
    xmldoc   = ixmldocument.

    rootnode = xmldoc->find_from_name( _objtype ).

    _objname = objname.

    getstructurefromattributes(
             EXPORTING
                  node      = rootnode
             CHANGING
                  structure = xtlibt ).

    functiongroupname = xtlibt-area.

* function groups in reserved namespace, not supported.
    IF functiongroupname(1) = '/'.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = 'Function Groups in / namespace are not supported'.
    ENDIF.

    objname = functiongroupname.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.

* Insert the function group
    xstext = xtlibt-areat.
    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = xtlibt-area
        short_text              = xstext
        devclass                = devclass
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.

* Create the function modules
    functionmodules_node  = rootnode->find_from_name( 'functionmodules' ).
    create_function_modules( fm_node = functionmodules_node
                             fct_group =  functiongroupname ).

* Create Includes
    includes_node  = rootnode->find_from_name( 'includeprograms' ).
    create_includes( devclass = devclass
                     incl_node = includes_node ).

* Update main program..... with include statements, dynpros, gui status
    mainprog_node  = rootnode->find_from_name( 'mainprogram' ).

    getstructurefromattributes(
           EXPORTING
              node      = mainprog_node
           CHANGING
              structure = progattribs ).

    objname = progattribs-name.     " Main Program Name is now the object

* Update the main program
    enqueue_abap( ).
    transport_copy( author = progattribs-cnam
                    devclass = devclass ).

* Source
    sourcenode  = mainprog_node->find_from_name( 'source' ).
    source      = sourcenode->get_value( ).
    sourcetable = buildtablefromstring( source ).
    create_source( source = sourcetable
                   attribs = progattribs ).

* Documentation
    docnode = rootnode->find_from_name( 'functionGroupDocumentation' ).
    create_documentation( docnode ).

* text pool
    textnode = mainprog_node->find_from_name( 'textPool' ).
    create_textpool( textnode ).

* Dynpros
    dynpnode = mainprog_node->find_from_name( 'dynpros' ).
    create_dynpro( dynpnode ).

* Gui status, titles
    statnode = mainprog_node->find_from_name( 'pfstatus' ).
    create_pfstatus( statnode ).

    dequeue_abap( ).

* Rebuild tree structure for SE80
    actualize_object_tree( ).

* successful install
    objname = functiongroupname.
    name = objname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_DOCUMENTATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCNODE                        TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_documentation.

    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA lang_node     TYPE REF TO if_ixml_element.
    DATA lang_filter   TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA prog_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    prog_name = docnode->get_attribute( name = 'OBJECT' ).
    obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
    IF prog_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( `language` ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'RE'   "<-- Report/program documentation
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'RE'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Program Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_DYNPRO
* +-------------------------------------------------------------------------------------------------+
* | [--->] DYNP_NODE                      TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_dynpro.
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

    TYPES: BEGIN OF tdyn_head_temp.
             INCLUDE TYPE d020s.
    TYPES:   dtext TYPE d020t-dtxt.
    TYPES: END OF tdyn_head_temp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA: xdyn_text_string TYPE string.
    DATA: xdyn_text        TYPE d020t-dtxt .
    DATA: xdyn_head_temp   TYPE tdyn_head_temp.

    DATA _objname TYPE trobj_name.

    DATA dynpros_node       TYPE REF TO if_ixml_element.
    DATA dynpros_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynpros_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynpro_node        TYPE REF TO if_ixml_element.
    DATA dynpro_filter      TYPE REF TO if_ixml_node_filter.
    DATA dynpro_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA dynfldl_node       TYPE REF TO if_ixml_element.
    DATA dynfldl_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynfldl_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynmcod_node       TYPE REF TO if_ixml_element.
    DATA dynmcod_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynmcod_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynflow_node       TYPE REF TO if_ixml_element.

    DATA xdynpro_flow_source TYPE string.
    DATA idynpro_flow_source TYPE table_of_strings.

    _objname = objname.

    dynpros_node =  dynp_node.
    CHECK dynpros_node IS NOT INITIAL.

    FREE: dynpro_filter, dynpro_iterator, dynpro_node.
    dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
    dynpro_iterator =
          dynpros_node->create_iterator_filtered( dynpro_filter ).
    dynpro_node ?= dynpro_iterator->get_next( ).

    WHILE dynpro_node IS NOT INITIAL.

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = dynpro_node
        CHANGING
          structure = xdyn_head_temp.

      xdyn_head    = xdyn_head_temp.
      xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
      FREE: dynfldl_filter, dynfldl_iterator, dynfldl_node.
      dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynfldl_iterator =
          dynpro_node->create_iterator_filtered( dynfldl_filter ).
      dynfldl_node ?= dynfldl_iterator->get_next( ).
      WHILE dynfldl_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynfldl_node
          CHANGING
            structure = xdyn_fldl.
        APPEND xdyn_fldl TO idyn_fldl.
        dynfldl_node ?= dynfldl_iterator->get_next( ).
      ENDWHILE.

* Retrieve matchcode data.
      FREE: dynmcod_filter, dynmcod_iterator, dynmcod_node.
      dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynmcod_iterator =
           dynpro_node->create_iterator_filtered( dynmcod_filter ).
      dynmcod_node ?= dynmcod_iterator->get_next( ).
      WHILE dynmcod_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynmcod_node
          CHANGING
            structure = xdyn_mcod.
        APPEND xdyn_mcod TO idyn_mcod.
        dynmcod_node ?= dynmcod_iterator->get_next( ).
      ENDWHILE.

* retieve flow logic source.
      CLEAR xdynpro_flow_source.  REFRESH idynpro_flow_source.
      CLEAR xdyn_flow.            REFRESH idyn_flow.
      FREE dynflow_node.
      dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
      xdynpro_flow_source  = dynflow_node->get_value( ).
      idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
      LOOP AT idynpro_flow_source INTO xdyn_flow.
        APPEND xdyn_flow  TO idyn_flow.
      ENDLOOP.

* Build dynpro from data
      CALL FUNCTION 'RPY_DYNPRO_INSERT_NATIVE'
        EXPORTING
*         suppress_corr_checks           = ' '
*         CORRNUM            = ' '
          header             = xdyn_head
          dynprotext         = xdyn_text
*         SUPPRESS_EXIST_CHECKS          = ' '
*         USE_CORRNUM_IMMEDIATEDLY       = ' '
*         SUPPRESS_COMMIT_WORK           = ' '
        TABLES
          fieldlist          = idyn_fldl
          flowlogic          = idyn_flow
          params             = idyn_mcod
        EXCEPTIONS
          cancelled          = 1
          already_exists     = 2
          program_not_exists = 3
          not_executed       = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
      ENDIF.

      dynpro_node ?= dynpro_iterator->get_next( ).

    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_FM_DOCUMENTATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCNODE                        TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_fm_documentation.

    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA lang_node     TYPE REF TO if_ixml_element.
    DATA lang_filter   TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA fm_parm_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    fm_parm_name = docnode->get_attribute( name = 'OBJECT' ).
    obj_name = fm_parm_name.

* If no fm_parm_name, then there was no documenation, just return.
    IF fm_parm_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( `language` ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'FU'   "<-- function module documentation
          langu    = obj_langu
          object   = obj_name
          typ      = 'T'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'FU'
          langu    = obj_langu
          object   = obj_name
          typ      = 'T'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Program Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_FUNCTION_MODULES
* +-------------------------------------------------------------------------------------------------+
* | [--->] FM_NODE                        TYPE REF TO IF_IXML_ELEMENT
* | [--->] FCT_GROUP                      TYPE        TLIBG-AREA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_function_modules.
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

    TYPES: BEGIN OF tfunct_head,
             name   TYPE rs38l-name,
             global TYPE rs38l-global,
             remote TYPE rs38l-remote,
             utask  TYPE rs38l-utask,
             stext  TYPE tftit-stext,
             area   TYPE rs38l-area,
           END OF tfunct_head.

    DATA functionmodules_node TYPE REF TO if_ixml_element.

    DATA source      TYPE string.
    DATA sourcetable TYPE table_of_strings.

    DATA functiongroupname TYPE  tlibg-area.
    DATA mainfgprogname    TYPE trdir-name.

    DATA xfunct_head TYPE tfunct_head.
    DATA iimport     TYPE TABLE OF rsimp.
    DATA ichange     TYPE TABLE OF rscha.
    DATA iexport     TYPE TABLE OF rsexp.
    DATA itables     TYPE TABLE OF rstbl.
    DATA iexcepl     TYPE TABLE OF rsexc.
    DATA idocume     TYPE TABLE OF rsfdo.
    DATA isource     TYPE TABLE OF rssource.
    DATA isource_new TYPE  rsfb_source.

    DATA ximport     TYPE  rsimp.
    DATA xchange     TYPE  rscha.
    DATA xexport     TYPE  rsexp.
    DATA xtables     TYPE  rstbl.
    DATA xexcepl     TYPE  rsexc.
    DATA xdocume     TYPE  rsfdo.
    DATA xsource     TYPE  rssource.
    DATA xsource_new LIKE LINE OF isource_new.

    DATA node          TYPE REF TO if_ixml_element.
    DATA filter        TYPE REF TO if_ixml_node_filter.
    DATA iterator      TYPE REF TO if_ixml_node_iterator.

    DATA im_node       TYPE REF TO if_ixml_element.
    DATA im_filter     TYPE REF TO if_ixml_node_filter.
    DATA im_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA ex_node       TYPE REF TO if_ixml_element.
    DATA ex_filter     TYPE REF TO if_ixml_node_filter.
    DATA ex_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA ch_node       TYPE REF TO if_ixml_element.
    DATA ch_filter     TYPE REF TO if_ixml_node_filter.
    DATA ch_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA ta_node       TYPE REF TO if_ixml_element.
    DATA ta_filter     TYPE REF TO if_ixml_node_filter.
    DATA ta_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA el_node       TYPE REF TO if_ixml_element.
    DATA el_filter     TYPE REF TO if_ixml_node_filter.
    DATA el_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dm_node       TYPE REF TO if_ixml_element.
    DATA dm_filter     TYPE REF TO if_ixml_node_filter.
    DATA dm_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA sc_node       TYPE REF TO if_ixml_element.
    DATA sc_filter     TYPE REF TO if_ixml_node_filter.
    DATA sc_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA scn_node       TYPE REF TO if_ixml_element.
    DATA scn_filter     TYPE REF TO if_ixml_node_filter.
    DATA scn_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA fmdoc_node     TYPE REF TO if_ixml_element.

    functionmodules_node = fm_node.
    functiongroupname    = fct_group.

    IF functionmodules_node  IS NOT INITIAL.

      FREE: filter, iterator, node.
      filter =
           functionmodules_node->create_filter_name( 'functionmodule' ).
      iterator = functionmodules_node->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).
      WHILE node IS NOT INITIAL.

        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node
          CHANGING
            structure = xfunct_head.

        REFRESH: iimport, ichange, iexport,
                 itables, iexcepl, idocume, isource, isource_new.

* Get importing
        FREE: im_filter, im_iterator, im_node.
        im_filter = node->create_filter_name( 'importing' ).
        im_iterator = node->create_iterator_filtered( im_filter ).
        im_node ?= im_iterator->get_next( ).
        WHILE im_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = im_node
            CHANGING
              structure = ximport.
          APPEND ximport TO iimport.
          im_node ?= im_iterator->get_next( ).
        ENDWHILE.

* Get exporting
        FREE: ex_filter, ex_iterator, ex_node.
        ex_filter = node->create_filter_name( 'exporting' ).
        ex_iterator = node->create_iterator_filtered( ex_filter ).
        ex_node ?= ex_iterator->get_next( ).
        WHILE ex_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = ex_node
            CHANGING
              structure = xexport.
          APPEND xexport TO iexport.
          ex_node ?= ex_iterator->get_next( ).
        ENDWHILE.

* Get changing
        FREE: ch_filter, ch_iterator, ch_node.
        ch_filter = node->create_filter_name( 'changing' ).
        ch_iterator = node->create_iterator_filtered( ch_filter ).
        ch_node ?= ch_iterator->get_next( ).
        WHILE ch_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = ch_node
            CHANGING
              structure = xchange.
          APPEND xchange TO ichange.
          ch_node ?= ch_iterator->get_next( ).
        ENDWHILE.

* Get tables
        FREE: ta_filter, ta_iterator, ta_node.
        ta_filter = node->create_filter_name( 'tables' ).
        ta_iterator = node->create_iterator_filtered( ta_filter ).
        ta_node ?= ta_iterator->get_next( ).
        WHILE ta_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = ta_node
            CHANGING
              structure = xtables.
          APPEND xtables TO itables.
          ta_node ?= ta_iterator->get_next( ).
        ENDWHILE.

* Get exception list
        FREE: el_filter, el_iterator, el_node.
        el_filter = node->create_filter_name( 'exceptions' ).
        el_iterator = node->create_iterator_filtered( el_filter ).
        el_node ?= el_iterator->get_next( ).
        WHILE el_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = el_node
            CHANGING
              structure = xexcepl.
          APPEND xexcepl TO iexcepl.
          el_node ?= el_iterator->get_next( ).
        ENDWHILE.

* Get documentation
        FREE: dm_filter, dm_iterator, dm_node.
        dm_filter = node->create_filter_name( 'documentation' ).
        dm_iterator = node->create_iterator_filtered( dm_filter ).
        dm_node ?= dm_iterator->get_next( ).
        WHILE dm_node IS NOT INITIAL.
          CALL METHOD getstructurefromattributes
            EXPORTING
              node      = dm_node
            CHANGING
              structure = xdocume.
          APPEND xdocume TO idocume.
          dm_node ?= dm_iterator->get_next( ).
        ENDWHILE.

* Get fm source

        FREE: sc_filter, sc_iterator, sc_node.
        sc_filter = node->create_filter_name( 'fm_source' ).
        sc_iterator = node->create_iterator_filtered( sc_filter ).
        sc_node ?= sc_iterator->get_next( ).
        WHILE sc_node IS NOT INITIAL.
          source = sc_node->get_value( ).
          sourcetable = buildtablefromstring( source ).
          LOOP AT sourcetable INTO xsource.
            APPEND xsource TO isource.
          ENDLOOP.
          sc_node ?= sc_iterator->get_next( ).
        ENDWHILE.

* Get fm source new
        FREE: scn_filter, scn_iterator, scn_node.
        scn_filter = node->create_filter_name( 'fm_source_new' ).
        scn_iterator = node->create_iterator_filtered( scn_filter ).
        scn_node ?= scn_iterator->get_next( ).
        WHILE scn_node IS NOT INITIAL.
          source = scn_node->get_value( ).
          sourcetable = buildtablefromstring( source ).
          LOOP AT sourcetable INTO xsource_new.
            APPEND xsource_new TO isource_new.
          ENDLOOP.
          scn_node ?= scn_iterator->get_next( ).
        ENDWHILE.

* INsert the function module
        CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
          EXPORTING
            funcname                = xfunct_head-name
            function_pool           = functiongroupname
            interface_global        = xfunct_head-global
            remote_call             = xfunct_head-remote
            update_task             = xfunct_head-utask
            short_text              = xfunct_head-stext
            save_active             = ' ' "<-- Need to set inactive
            new_source              = isource_new
          TABLES
            import_parameter        = iimport
            export_parameter        = iexport
            tables_parameter        = itables
            changing_parameter      = ichange
            exception_list          = iexcepl
            parameter_docu          = idocume
            source                  = isource
          EXCEPTIONS
            double_task             = 1
            error_message           = 2
            function_already_exists = 3
            invalid_function_pool   = 4
            invalid_name            = 5
            too_many_functions      = 6
            no_modify_permission    = 7
            no_show_permission      = 8
            enqueue_system_failure  = 9
            canceled_in_corr        = 10
            OTHERS                  = 11.

* Create function module documentation
        fmdoc_node = node->find_from_name( 'functionModuleDocumentation' ).
        create_fm_documentation( fmdoc_node ).

        node ?= iterator->get_next( ).
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_INCLUDES
* +-------------------------------------------------------------------------------------------------+
* | [--->] INCL_NODE                      TYPE REF TO IF_IXML_ELEMENT
* | [--->] DEVCLASS                       TYPE        DEVCLASS (default ='$TMP')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_includes.
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

    TYPES: BEGIN OF tinclude,
             name(40),
           END OF tinclude.

    DATA iinclude TYPE TABLE OF tinclude.
    DATA xinclude TYPE tinclude.

    DATA inc_node       TYPE REF TO if_ixml_element.
    DATA inc_filter     TYPE REF TO if_ixml_node_filter.
    DATA inc_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA progattribs TYPE trdir.

    DATA includes_node     TYPE REF TO if_ixml_element.
    DATA includesourcenode TYPE REF TO if_ixml_element.

    DATA source      TYPE string.
    DATA sourcetable TYPE table_of_strings.

    includes_node = incl_node.

    CHECK includes_node IS NOT INITIAL.

    FREE: inc_filter, inc_iterator, inc_node.
    inc_filter = includes_node->create_filter_name( 'include' ).
    inc_iterator = includes_node->create_iterator_filtered( inc_filter ).
    inc_node ?= inc_iterator->get_next( ).

    WHILE inc_node IS NOT INITIAL.

      getstructurefromattributes(
            EXPORTING
               node      = inc_node
            CHANGING
               structure = progattribs ).

      includesourcenode = inc_node->find_from_name( 'include_source' ).
      source      = includesourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).

      objname = progattribs-name.   " Include Program Name is the object

      enqueue_abap( ).
      transport_copy( author = progattribs-cnam
                                         devclass = devclass ).
      create_source( source = sourcetable
                                        attribs = progattribs ).
      dequeue_abap( ).

      inc_node  ?=  inc_iterator->get_next( ).

    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_PFSTATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFSTAT_NODE                    TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_pfstatus.
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

    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA xtrkey TYPE trkey.
    DATA xadm   TYPE rsmpe_adm.
    DATA _program TYPE  trdir-name.
    DATA _objname TYPE trobj_name.

    DATA stat_node  TYPE REF TO if_ixml_element.
    DATA node       TYPE REF TO if_ixml_element.
    DATA filter     TYPE REF TO if_ixml_node_filter.
    DATA iterator   TYPE REF TO if_ixml_node_iterator.

    _objname = objname.

    stat_node =  pfstat_node.
    CHECK stat_node IS NOT INITIAL.

* read pfstatus_sta node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_sta' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xsta.
      APPEND xsta TO ista.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_fun node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_fun' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xfun.
      APPEND xfun TO ifun.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_men node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_men' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmen.
      APPEND xmen TO imen.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_mtx node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmtx.
      APPEND xmtx TO imtx.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_act node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_act' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xact.
      APPEND xact TO iact.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_but node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_but' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbut.
      APPEND xbut TO ibut.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_pfk node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xpfk.
      APPEND xpfk TO ipfk.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_set node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_set' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xset.
      APPEND xset TO iset.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_doc node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_doc' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xdoc.
      APPEND xdoc TO idoc.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_tit node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_tit' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xtit.
      APPEND xtit TO itit.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_biv node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_biv' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbiv.
      APPEND xbiv TO ibiv.
      node ?= iterator->get_next( ).
    ENDWHILE.

* Update the gui status
    _program = _objname.

*  xtrkey-obj_type = 'PROG'.
    xtrkey-obj_type = 'FUGR'.
    xtrkey-obj_name = _program.
    xtrkey-sub_type = 'CUAD'.
    xtrkey-sub_name = _program.

    LOOP AT iact ASSIGNING FIELD-SYMBOL(<ls_iact>).
      xadm-actcode = <ls_iact>-code. " Issues #276
    ENDLOOP.
    LOOP AT ipfk ASSIGNING FIELD-SYMBOL(<ls_ipfk>).
      xadm-pfkcode = <ls_ipfk>-code. " Issues #276
    ENDLOOP.
    LOOP AT imen ASSIGNING FIELD-SYMBOL(<ls_imen>).
      xadm-mencode = <ls_imen>-code. " Issues #276
    ENDLOOP.

    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = _program
        language  = sy-langu
        tr_key    = xtrkey
        adm       = xadm
        state     = 'I'
      TABLES
        sta       = ista
        fun       = ifun
        men       = imen
        mtx       = imtx
        act       = iact
        but       = ibut
        pfk       = ipfk
        set       = iset
        doc       = idoc
        tit       = itit
        biv       = ibiv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SOURCE                         TYPE        TABLE_OF_STRINGS
* | [--->] ATTRIBS                        TYPE        TRDIR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_source.
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
    DATA _objname TYPE trobj_name.
    DATA progline TYPE progdir.
    DATA titleinfo TYPE trdirti.
    DATA reportline TYPE string.
    DATA minireport TYPE table_of_strings.

    _objname = objname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'REPS'
        obj_name          = _objname
      EXCEPTIONS
        wrong_object_name = 1.
    INSERT REPORT _objname FROM source STATE 'I'
      PROGRAM TYPE attribs-subc.  "added to handle includes, etc.
    MOVE 'I' TO progline-state.
    MOVE-CORRESPONDING attribs TO progline.
    MODIFY progdir FROM progline.
*  Are you kidding me?!?  No idea why you need to do this!!
    CONCATENATE 'REPORT' _objname '.' INTO reportline SEPARATED BY space.
    APPEND reportline TO minireport.
    INSERT REPORT _objname FROM minireport STATE 'A'
      PROGRAM TYPE attribs-subc. "added to handle includes, etc.
    MOVE 'A' TO progline-state.
    MODIFY progdir FROM progline.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->CREATE_TEXTPOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] TEXTPOOLNODE                   TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_textpool.
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
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA langiterator TYPE REF TO if_ixml_node_iterator.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA textfilter TYPE REF TO if_ixml_node_filter.
    DATA textiterator TYPE REF TO if_ixml_node_iterator.
    DATA langnode TYPE REF TO if_ixml_element.
    DATA atextnode TYPE REF TO if_ixml_element.
    DATA _objname TYPE trobj_name.
    DATA lang TYPE spras.
    DATA langnodeexists TYPE flag.
    DATA logonlanguageexists TYPE flag.
    DATA _state(1) TYPE c.

    _objname = objname.

    filter = textpoolnode->create_filter_name( 'language' ).
    langiterator = textpoolnode->create_iterator_filtered( filter ).
    langnode ?= langiterator->get_next( ).

    WHILE langnode IS NOT INITIAL.
      langnodeexists = 'X'.
      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          object   = 'REPT'
          obj_name = _objname
        EXCEPTIONS
          OTHERS   = 0.

      REFRESH textpooltable.
      textiterator = langnode->create_iterator( ).
      atextnode ?= textiterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      atextnode ?= textiterator->get_next( ).
      WHILE atextnode IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = atextnode
          CHANGING
            structure = textpoolrow.
        APPEND textpoolrow TO textpooltable.
        atextnode ?= textiterator->get_next( ).
      ENDWHILE.
      IF textpooltable IS NOT INITIAL.
        lang = langnode->get_attribute( 'SPRAS' ).
        IF lang = sy-langu.
          logonlanguageexists = 'X'.
          _state = 'I'.
        ELSE.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
          _state = 'A'.
        ENDIF.
      ENDIF.
      INSERT TEXTPOOL _objname FROM textpooltable LANGUAGE lang
      STATE _state.
      langnode ?= langiterator->get_next( ).
    ENDWHILE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_FUNCTIONGROUP->DELETEOBJECT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deleteobject.
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

    DATA area TYPE rs38l-area.

    area = objname.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = area
*       CORRNUM                = ' '
*       TEXT                   = ' '
*       UNAME                  = ' '
        with_korr              = ' '
*       WB_FB_MANAGER          =
        suppress_popups        = 'X'
*       SKIP_PROGRESS_IND      = ' '
* IMPORTING
*       E_CORRNUM              =
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->DEQUEUE_ABAP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dequeue_abap.
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

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        global_lock              = 'X'
        mode                     = 'FREE'
        object                   = objname
        object_class             = 'ABAP'
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 3
        enqueue_system_failure   = 4
        locked_by_author         = 5
        illegal_parameter_values = 6
        no_modify_permission     = 7
        no_show_permission       = 8
        permission_failure       = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 7 OR 8 OR 9.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 5.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->ENQUEUE_ABAP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enqueue_abap.
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


    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
*       authority_check          = authority_check
        global_lock              = 'X'
        mode                     = 'INSERT'
*       master_language          = trdir-rload
        object                   = objname
        object_class             = 'ABAP'
*       importing
*       transport_key            = trkey_global
*       new_master_language      = trdir-rload
*       devclass                 = devclass_local
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 3
        enqueue_system_failure   = 4
        locked_by_author         = 5
        illegal_parameter_values = 6
        no_modify_permission     = 7
        no_show_permission       = 8
        permission_failure       = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 7 OR 8 OR 9.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 5.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZSAPLINK_FUNCTIONGROUP->GETOBJECTTYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJECTTYPE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getobjecttype.
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
    objecttype = 'FUGR'. " Function Group
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_DOCUMENTATION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] DOCNODE                        TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_documentation.

    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'RE'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

    docnode = xmldoc->create_element( 'functionGroupDocumentation' ).

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = 'OBJECT' value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( 'language' ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = 'SPRAS' value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( `textLine` ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_DYNPRO
* +-------------------------------------------------------------------------------------------------+
* | [<-()] DYNP_NODE                      TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_dynpro.
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

    TYPES: BEGIN OF tdynp,
             prog TYPE d020s-prog,
             dnum TYPE d020s-dnum,
           END OF tdynp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA idynp TYPE TABLE OF tdynp.
    DATA xdynp TYPE tdynp.

    DATA xdyn_text TYPE d020t-dtxt.
    DATA xdyn_text_string TYPE string.

    DATA _objname TYPE trobj_name.
    DATA rc TYPE sy-subrc .

    DATA iflowsource TYPE rswsourcet.
    DATA xflowsource LIKE LINE OF iflowsource.
    DATA flowsourcestring TYPE string.

    DATA dynnr_node TYPE REF TO if_ixml_element.
    DATA dynpromatchnode TYPE REF TO if_ixml_element.
    DATA dynprofieldsnode TYPE REF TO if_ixml_element.
    DATA dynproflownode TYPE REF TO if_ixml_element.

    _objname = objname.

* Get all dynpros for program object
    CLEAR xdynp.  REFRESH idynp.
    SELECT prog dnum INTO TABLE idynp
                  FROM d020s
                     WHERE prog = _objname
                       AND type <> 'S'    " No Selection Screens
                       AND type <> 'J'.   " No selection subscreens
    CHECK sy-subrc  = 0 .

    dynp_node = xmldoc->create_element( 'dynpros' ).

    LOOP AT idynp INTO xdynp.

* Retrieve dynpro imformation
      dynnr_node =  xmldoc->create_element( 'dynpro' ).

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname         = xdynp-prog
          dynnr            = xdynp-dnum
*         SUPPRESS_EXIST_CHECKS       = ' '
*         SUPPRESS_CORR_CHECKS        = ' '
        IMPORTING
          header           = xdyn_head
          dynprotext       = xdyn_text
        TABLES
          fieldlist        = idyn_fldl
          flowlogic        = idyn_flow
          params           = idyn_mcod
*         FIELDTEXTS       =
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.

      CHECK sy-subrc = 0.

* Add heading information for screen.
      setattributesfromstructure(
                       node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
      xdyn_text_string =  xdyn_text.
      rc = dynnr_node->set_attribute(
                 name = 'DTEXT'  value = xdyn_text_string ).
      rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
      IF NOT idyn_fldl[] IS INITIAL.
        LOOP AT idyn_fldl INTO xdyn_fldl.
          dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
          setattributesfromstructure(
                   node = dynprofieldsnode structure =  xdyn_fldl ).
          rc = dynnr_node->append_child( dynprofieldsnode ).
        ENDLOOP.
      ENDIF.

* Add flow logic of screen
      IF NOT idyn_flow[] IS INITIAL.
        CLEAR xflowsource. REFRESH  iflowsource.
        LOOP AT idyn_flow INTO xdyn_flow.
          xflowsource  = xdyn_flow.
          APPEND xflowsource TO iflowsource.
        ENDLOOP.

        dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
        flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
        rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
        rc = dynnr_node->append_child( dynproflownode  ).
      ENDIF.

* Add matchcode information for screen.
      IF NOT idyn_mcod[] IS INITIAL.
        LOOP AT idyn_mcod INTO xdyn_mcod.
          CHECK NOT xdyn_mcod-type IS INITIAL
            AND NOT xdyn_mcod-content IS INITIAL.
          dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
          setattributesfromstructure(
                   node = dynpromatchnode structure =  xdyn_mcod ).
          rc = dynnr_node->append_child( dynpromatchnode ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_FM_DOCUMENTATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] FM_NAME                        TYPE        ANY
* | [<-()] DOCNODE                        TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fm_documentation.

    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = fm_name.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'FU'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

    docnode = xmldoc->create_element( 'functionModuleDocumentation' ).

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = 'OBJECT' value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( 'language' ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = 'SPRAS' value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( `textLine` ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_FUNCTION_MODULES
* +-------------------------------------------------------------------------------------------------+
* | [--->] FCT_GROUP                      TYPE        TLIBG-AREA
* | [<-()] FM_NODE                        TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_function_modules.
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

    TYPES: BEGIN OF tfunct_head,
             name   TYPE rs38l-name,
             global TYPE rs38l-global,
             remote TYPE rs38l-remote,
             utask  TYPE rs38l-utask,
             stext  TYPE tftit-stext,
             area   TYPE rs38l-area,
           END OF tfunct_head.

    DATA xfunct_head TYPE tfunct_head.
    DATA iimport     TYPE TABLE OF rsimp.
    DATA ichange     TYPE TABLE OF rscha.
    DATA iexport     TYPE TABLE OF rsexp.
    DATA itables     TYPE TABLE OF rstbl.
    DATA iexcepl     TYPE TABLE OF rsexc.
    DATA idocume     TYPE TABLE OF rsfdo.
    DATA isource     TYPE TABLE OF rssource.
    DATA isource_new TYPE rsfb_source .

    DATA ximport     TYPE  rsimp.
    DATA xchange     TYPE  rscha.
    DATA xexport     TYPE  rsexp.
    DATA xtables     TYPE  rstbl.
    DATA xexcepl     TYPE  rsexc.
    DATA xdocume     TYPE  rsfdo.
    DATA xsource     TYPE  rssource.
    DATA xsource_new LIKE LINE OF isource_new.

    DATA functionmodulesnode TYPE REF TO if_ixml_element.
    DATA functionmodulenode  TYPE REF TO if_ixml_element.
    DATA importsnode TYPE REF TO if_ixml_element.
    DATA changesnode TYPE REF TO if_ixml_element.
    DATA exportsnode TYPE REF TO if_ixml_element.
    DATA tablesnode  TYPE REF TO if_ixml_element.
    DATA exceplnode  TYPE REF TO if_ixml_element.
    DATA documsnode  TYPE REF TO if_ixml_element.
    DATA fmsrcenode  TYPE REF TO if_ixml_element.
    DATA fmsrcenewnode  TYPE REF TO if_ixml_element.
    DATA fmdocumenation TYPE REF TO if_ixml_element.
    DATA fmparmdocumenation TYPE REF TO if_ixml_element.

    DATA functiongroupname TYPE  tlibg-area.

    DATA ifunct TYPE TABLE OF  rs38l_incl.
    DATA xfunct TYPE  rs38l_incl.

    DATA rc           TYPE sysubrc.
    DATA progattribs  TYPE trdir.
    DATA progsource   TYPE rswsourcet.
    DATA _objname(30) TYPE c.
    DATA sourcestring TYPE string.
    DATA function_deleted    TYPE c.
    DATA endfunction_deleted TYPE c.
    DATA lv_len TYPE i.

    functiongroupname = fct_group.

* Now get the function pool contents
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = functiongroupname
      TABLES
        functab                 = ifunct
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.

* Now write out function modules data.
    functionmodulesnode = xmldoc->create_element( 'functionmodules' ).

    LOOP AT ifunct INTO xfunct.

      functionmodulenode = xmldoc->create_element( 'functionmodule' ).
      xfunct_head-name =  xfunct-funcname.

      REFRESH: iimport, ichange, iexport,
               itables, iexcepl, idocume, isource, isource_new.

* Read the function module data
      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname       = xfunct_head-name
        IMPORTING
          global_flag        = xfunct_head-global
          remote_call        = xfunct_head-remote
          update_task        = xfunct_head-utask
          short_text         = xfunct_head-stext
*         FUNCTION_POOL      =
        TABLES
          import_parameter   = iimport
          changing_parameter = ichange
          export_parameter   = iexport
          tables_parameter   = itables
          exception_list     = iexcepl
          documentation      = idocume
          source             = isource
        CHANGING
          new_source         = isource_new
        EXCEPTIONS
          error_message      = 1
          function_not_found = 2
          invalid_name       = 3
          OTHERS             = 4.

* Set the header attributes
      setattributesfromstructure(
                 node = functionmodulenode
                 structure =  xfunct_head  ).

* IMports
      IF NOT iimport[] IS INITIAL.
        LOOP AT iimport INTO ximport.
          importsnode = xmldoc->create_element( 'importing' ).
          setattributesfromstructure(
                   node = importsnode structure =  ximport ).
          rc = functionmodulenode->append_child( importsnode ).
        ENDLOOP.
      ENDIF.

* Exports
      IF NOT iexport[] IS INITIAL.
        LOOP AT iexport INTO xexport.
          exportsnode = xmldoc->create_element( 'exporting' ).
          setattributesfromstructure(
                   node = exportsnode structure =  xexport ).
          rc = functionmodulenode->append_child( exportsnode ).
        ENDLOOP.
      ENDIF.

* Changing
      IF NOT ichange[] IS INITIAL.
        LOOP AT ichange INTO xchange.
          changesnode = xmldoc->create_element( 'changing' ).
          setattributesfromstructure(
                   node = changesnode structure =  xchange ).
          rc = functionmodulenode->append_child( changesnode ).
        ENDLOOP.
      ENDIF.

* Tables
      IF NOT itables[] IS INITIAL.
        LOOP AT itables INTO xtables.
          tablesnode = xmldoc->create_element( 'tables' ).
          setattributesfromstructure(
                   node = tablesnode structure =  xtables ).
          rc = functionmodulenode->append_child( tablesnode ).
        ENDLOOP.
      ENDIF.

* Exception list
      IF NOT iexcepl[] IS INITIAL.
        LOOP AT iexcepl INTO xexcepl.
          exceplnode = xmldoc->create_element( 'exceptions' ).
          setattributesfromstructure(
                   node = exceplnode structure =  xexcepl ).
          rc = functionmodulenode->append_child( exceplnode ).
        ENDLOOP.
      ENDIF.

* Documentation - this is short text
      IF NOT idocume[] IS INITIAL.
        LOOP AT idocume INTO xdocume .
          documsnode = xmldoc->create_element( 'documentation' ).
          setattributesfromstructure(
                   node = documsnode structure =  xdocume  ).

          rc = functionmodulenode->append_child( documsnode ).
        ENDLOOP.
      ENDIF.

* Source code for function module
      IF NOT isource[] IS INITIAL.

* Get rid of the FUNCTION and ENDFUNCTION statements.
* And the signature comments
* All of this will be inserted automatically, when imported.
        CLEAR: function_deleted, endfunction_deleted.
        LOOP AT isource INTO xsource.
          IF xsource+0(2) = '*"'.
            DELETE isource INDEX sy-tabix.
            CONTINUE.
          ENDIF.
          SEARCH xsource FOR 'FUNCTION'.
          "Got it and not a comment?
          IF sy-subrc  = 0 AND xsource+0(1) <> '*' AND
             function_deleted NE 'X'.
            DELETE isource INDEX sy-tabix.
            function_deleted = 'X'.
            CONTINUE.
          ENDIF.
          SEARCH xsource FOR 'ENDFUNCTION'.
          IF sy-subrc  = 0.
            DELETE isource INDEX sy-tabix.
            endfunction_deleted = 'X'.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        fmsrcenode = xmldoc->create_element( 'fm_source' ).
        REFRESH progsource.
        LOOP AT isource INTO xsource.
          APPEND xsource TO progsource.
        ENDLOOP.
        sourcestring = buildsourcestring( sourcetable = progsource ).
        rc = fmsrcenode->if_ixml_node~set_value( sourcestring ).
        rc = functionmodulenode->append_child( fmsrcenode ).

      ENDIF.

* Source code for function module
      IF NOT isource_new[] IS INITIAL.

* Get rid of the FUNCTION and ENDFUNCTION statements.
* And the signature comments
* All of this will be inserted automatically, when imported.
        CLEAR: function_deleted, endfunction_deleted.
        LOOP AT isource_new INTO xsource_new.
          CHECK xsource_new IS NOT INITIAL.
          CLEAR lv_len.
          lv_len = strlen( xsource_new ).
          IF lv_len GE 2.
            IF xsource_new+0(2) = '*"'.
              DELETE isource_new INDEX sy-tabix.
              CONTINUE.
            ENDIF.
          ENDIF.
          SEARCH xsource_new FOR 'FUNCTION'.
          "Got it and not a comment?
          IF sy-subrc  = 0 AND xsource_new+0(1) <> '*' AND
             function_deleted NE 'X'.
            DELETE isource_new INDEX sy-tabix.
            function_deleted = 'X'.
            CONTINUE.
          ENDIF.
          SEARCH xsource_new FOR 'ENDFUNCTION'.
          IF sy-subrc  = 0.
            DELETE isource_new INDEX sy-tabix.
            endfunction_deleted = 'X'.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        fmsrcenewnode = xmldoc->create_element( 'fm_source_new' ).
        REFRESH progsource.
        LOOP AT isource_new INTO xsource_new.
          APPEND xsource_new TO progsource.
        ENDLOOP.
        sourcestring = buildsourcestring( sourcetable = progsource ).
        rc = fmsrcenewnode->if_ixml_node~set_value( sourcestring ).
        rc = functionmodulenode->append_child( fmsrcenewnode ).

      ENDIF.

* Get function module documentation
      fmdocumenation = get_fm_documentation( xfunct-funcname ).
      rc = functionmodulenode->append_child( fmdocumenation ).

* Add to functionmodules node
      rc = functionmodulesnode->append_child( functionmodulenode ).

    ENDLOOP.


    fm_node = functionmodulesnode.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_INCLUDES
* +-------------------------------------------------------------------------------------------------+
* | [--->] MAIN_PROG                      TYPE        SY-REPID
* | [--->] FCT_GROUP                      TYPE        TLIBT-AREA
* | [<-()] INCL_NODE                      TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_includes.
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

    TYPES: BEGIN OF tinclude,
             name(40),
           END OF tinclude.

    DATA iinclude TYPE TABLE OF tinclude.
    DATA xinclude TYPE tinclude.

    DATA ifunct TYPE TABLE OF  rs38l_incl.
    DATA xfunct TYPE  rs38l_incl.

    DATA functiongroupname TYPE  tlibg-area.
    DATA mainfgprogname    TYPE sy-repid.

    DATA includenode  TYPE REF TO if_ixml_element.
    DATA includesnode TYPE REF TO if_ixml_element.
    DATA includesourcenode TYPE REF TO if_ixml_element.

    DATA progattribs  TYPE trdir.
    DATA rc           TYPE sysubrc.
    DATA progsource   TYPE rswsourcet.
    DATA _objname(30) TYPE c.
    DATA sourcestring TYPE string.

    functiongroupname = fct_group.
    mainfgprogname    = main_prog.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = functiongroupname
      TABLES
        functab                 = ifunct
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.

* Get all includes
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = mainfgprogname
      TABLES
        includetab   = iinclude
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.

* Get rid of any includes that are for the function modules
* and any includes that are in SAP namespace
    LOOP AT iinclude INTO xinclude.
      READ TABLE ifunct
               INTO xfunct
                     WITH KEY include = xinclude-name.
      IF sy-subrc  = 0.
        DELETE iinclude WHERE name = xinclude-name.
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM trdir
              INTO progattribs
                     WHERE name = xinclude-name.
      IF progattribs-cnam = 'SAP'.
        DELETE iinclude WHERE name = xinclude-name.
        CONTINUE.
      ENDIF.
      IF xinclude-name(2) <> 'LZ'
         AND xinclude-name(2) <> 'LY'
         AND xinclude-name(1) <> 'Z'
         AND xinclude-name(1) <> 'Y'.
        DELETE iinclude WHERE name = xinclude-name.
        CONTINUE.
      ENDIF.
    ENDLOOP.

* Write out include programs.....
    includesnode = xmldoc->create_element( 'includeprograms' ).

    LOOP AT iinclude INTO xinclude.

      includenode = xmldoc->create_element( 'include' ).
      SELECT SINGLE * FROM trdir
              INTO progattribs
                     WHERE name = xinclude-name.
      setattributesfromstructure(
                 node = includenode
                 structure =  progattribs  ).

      includesourcenode = xmldoc->create_element( 'include_source' ).
      READ REPORT xinclude-name INTO progsource.
      sourcestring = buildsourcestring( sourcetable = progsource ).
      rc = includesourcenode->if_ixml_node~set_value( sourcestring ).
      rc = includenode->append_child( includesourcenode ).
      rc = includesnode->append_child( includenode ).

    ENDLOOP.

    incl_node = includesnode.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_PFSTATUS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PFSTAT_NODE                    TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_pfstatus.
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

    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA sta_node TYPE REF TO if_ixml_element.
    DATA fun_node TYPE REF TO if_ixml_element.
    DATA men_node TYPE REF TO if_ixml_element.
    DATA mtx_node TYPE REF TO if_ixml_element.
    DATA act_node TYPE REF TO if_ixml_element.
    DATA but_node TYPE REF TO if_ixml_element.
    DATA pfk_node TYPE REF TO if_ixml_element.
    DATA set_node TYPE REF TO if_ixml_element.
    DATA doc_node TYPE REF TO if_ixml_element.
    DATA tit_node TYPE REF TO if_ixml_element.
    DATA biv_node TYPE REF TO if_ixml_element.

    DATA _objname TYPE trobj_name.
    DATA _program TYPE  trdir-name.
    DATA rc TYPE sy-subrc.

    _objname = objname.
    _program = objname.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = _program
        language        = sy-langu
      TABLES
        sta             = ista
        fun             = ifun
        men             = imen
        mtx             = imtx
        act             = iact
        but             = ibut
        pfk             = ipfk
        set             = iset
        doc             = idoc
        tit             = itit
        biv             = ibiv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.

    CHECK sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
    IF ista[] IS NOT INITIAL
       OR itit[] IS NOT INITIAL.
      pfstat_node = xmldoc->create_element( 'pfstatus' ).
    ENDIF.


* if ista is filled, assume there are one or more
* gui statuses
    IF ista[] IS NOT INITIAL.

      LOOP AT ista INTO xsta.
        sta_node = xmldoc->create_element( 'pfstatus_sta' ).
        setattributesfromstructure(
                 node = sta_node
                 structure =  xsta ).
        rc = pfstat_node->append_child( sta_node ).
      ENDLOOP.

      LOOP AT ifun INTO xfun.
        fun_node = xmldoc->create_element( 'pfstatus_fun' ).
        setattributesfromstructure(
                 node = fun_node
                 structure =  xfun ).
        rc = pfstat_node->append_child( fun_node ).
      ENDLOOP.

      LOOP AT imen INTO xmen.
        men_node = xmldoc->create_element( 'pfstatus_men' ).
        setattributesfromstructure(
                 node = men_node
                 structure =  xmen ).
        rc = pfstat_node->append_child( men_node ).
      ENDLOOP.

      LOOP AT imtx INTO xmtx.
        mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
        setattributesfromstructure(
                 node = mtx_node
                 structure =  xmtx ).
        rc = pfstat_node->append_child( mtx_node ).
      ENDLOOP.

      LOOP AT iact INTO xact.
        act_node = xmldoc->create_element( 'pfstatus_act' ).
        setattributesfromstructure(
                 node = act_node
                 structure =  xact ).
        rc = pfstat_node->append_child( act_node ).
      ENDLOOP.

      LOOP AT ibut INTO xbut.
        but_node = xmldoc->create_element( 'pfstatus_but' ).
        setattributesfromstructure(
                 node = but_node
                 structure =  xbut ).
        rc = pfstat_node->append_child( but_node ).
      ENDLOOP.

      LOOP AT ipfk INTO xpfk.
        pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
        setattributesfromstructure(
                 node = pfk_node
                 structure =  xpfk ).
        rc = pfstat_node->append_child( pfk_node ).
      ENDLOOP.

      LOOP AT iset INTO xset.
        set_node = xmldoc->create_element( 'pfstatus_set' ).
        setattributesfromstructure(
                 node = set_node
                 structure =  xset ).
        rc = pfstat_node->append_child( set_node ).
      ENDLOOP.

      LOOP AT idoc INTO xdoc.
        doc_node = xmldoc->create_element( 'pfstatus_doc' ).
        setattributesfromstructure(
                 node = doc_node
                 structure =  xdoc ).
        rc = pfstat_node->append_child( doc_node ).
      ENDLOOP.


      LOOP AT ibiv INTO xbiv.
        biv_node = xmldoc->create_element( 'pfstatus_biv' ).
        setattributesfromstructure(
                 node = biv_node
                 structure =  xbiv ).
        rc = pfstat_node->append_child( biv_node ).
      ENDLOOP.

    ENDIF.


* It itit is filled, assume one or more titles
    IF itit[] IS NOT INITIAL.

      LOOP AT itit INTO xtit.
        tit_node = xmldoc->create_element( 'pfstatus_tit' ).
        setattributesfromstructure(
                 node = tit_node
                 structure =  xtit ).
        rc = pfstat_node->append_child( tit_node ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->GET_TEXTPOOL
* +-------------------------------------------------------------------------------------------------+
* | [<-()] TEXTNODE                       TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_textpool.
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

    DATA atext TYPE REF TO if_ixml_element.
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA languagelist TYPE instlang.
    DATA alanguage TYPE spras.
    DATA _objname(30) TYPE c.
    DATA rc TYPE i.
    DATA stemp TYPE string.
    DATA languagenode TYPE REF TO if_ixml_element.

    _objname = objname.


    textnode = xmldoc->create_element( 'textPool' ).

    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
      CHANGING
        installed_languages = languagelist.

    LOOP AT languagelist INTO alanguage.
      READ TEXTPOOL _objname INTO textpooltable LANGUAGE alanguage.
      IF sy-subrc = 0.
        languagenode = xmldoc->create_element( 'language' ).
        stemp = alanguage.
        rc = languagenode->set_attribute( name = 'SPRAS' value = stemp ).
        LOOP AT textpooltable INTO textpoolrow.
          atext = xmldoc->create_element( 'textElement' ).
          setattributesfromstructure( node = atext structure =
          textpoolrow ).
          rc = languagenode->append_child( atext ).
        ENDLOOP.
        rc = textnode->append_child( languagenode ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZSAPLINK_FUNCTIONGROUP->TRANSPORT_COPY
* +-------------------------------------------------------------------------------------------------+
* | [--->] AUTHOR                         TYPE        SYUNAME
* | [--->] DEVCLASS                       TYPE        DEVCLASS
* | [!CX!] ZCX_SAPLINK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD transport_copy.
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


    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author              = author
        global_lock         = 'X'
        object              = objname
        object_class        = 'ABAP'
        devclass            = devclass
*       KORRNUM             = CORRNUMBER_LOCAL
        master_language     = sy-langu
*       PROGRAM             = PROGRAM_LOCAL
        mode                = 'INSERT'
*       IMPORTING
*       AUTHOR              = UNAME
*       KORRNUM             = CORRNUMBER_LOCAL
*       DEVCLASS            = DEVCLASS_LOCAL
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS zsaplink_oo IMPLEMENTATION.
  METHOD create_alias_method.
    DATA: filter   TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node     TYPE REF TO if_ixml_element.

    DATA: ls_alias_method  LIKE LINE OF xt_aliases_method.


    filter = xmldoc->create_filter_name( c_xml_key_alias_method ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR ls_alias_method.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_alias_method.
      INSERT ls_alias_method INTO TABLE xt_aliases_method.
      node ?= iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_clsdeferrd.
    DATA: filter   TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node     TYPE REF TO if_ixml_element.

    DATA: ls_clsdeferrd  LIKE LINE OF xt_clsdeferrds.


    filter   = xmldoc->create_filter_name( c_xml_key_clsdeferrd ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_clsdeferrd.
      APPEND ls_clsdeferrd TO xt_clsdeferrds.
      node ?= iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_intdeferrd.
    DATA: filter   TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node     TYPE REF TO if_ixml_element.

    DATA: ls_intdeferrd  LIKE LINE OF xt_intdeferrds.


    filter   = xmldoc->create_filter_name( c_xml_key_intdeferrd ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_intdeferrd.
      APPEND ls_intdeferrd TO xt_intdeferrds.
      node ?= iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_otr.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.
    DATA sotrpaket TYPE sotr_pack.

* get OTR header info
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = sotrheader.

* get OTR text info
    filter = node->create_filter_name( c_xml_key_sotrtext ).
    iterator = node->create_iterator_filtered( filter ).
    txtnode ?= iterator->get_next( ).

    WHILE txtnode IS NOT INITIAL.
      CLEAR sotrtextline.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = txtnode
        CHANGING
          structure = sotrtextline.
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      APPEND sotrtextline TO sotrtexttable.
      txtnode ?= iterator->get_next( ).
    ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


    DATA objecttable TYPE sotr_objects.
    DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
    CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
      EXPORTING
        object_vector    = sotrheader-objid_vec
      IMPORTING
        objects          = objecttable
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
    sotrpaket-paket = devclass.
    CALL FUNCTION 'SOTR_CREATE_CONCEPT'
      EXPORTING
        paket                         = sotrpaket
        crea_lan                      = sotrheader-crea_lan
        alias_name                    = sotrheader-alias_name
*       CATEGORY                      =
        object                        = objecttype
        entries                       = sotrtexttable
*       FLAG_CORRECTION_ENTRY         =
*       IN_UPDATE_TASK                =
*       CONCEPT_DEFAULT               = sotrHeader-concept "ewH:33
      IMPORTING
        concept                       = concept         "ewH:33
      EXCEPTIONS
        package_missing               = 1
        crea_lan_missing              = 2
        object_missing                = 3
        paket_does_not_exist          = 4
        alias_already_exist           = 5
        object_type_not_found         = 6
        langu_missing                 = 7
        identical_context_not_allowed = 8
        text_too_long                 = 9
        error_in_update               = 10
        no_master_langu               = 11
        error_in_concept_id           = 12
        alias_not_allowed             = 13
        tadir_entry_creation_failed   = 14
        internal_error                = 15
        error_in_correction           = 16
        user_cancelled                = 17
        no_entry_found                = 18
        OTHERS                        = 19.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.
  METHOD create_typepusage.
    DATA: filter   TYPE REF TO if_ixml_node_filter,
          iterator TYPE REF TO if_ixml_node_iterator,
          node     TYPE REF TO if_ixml_element,
          source   TYPE string.


    DATA: ls_typepusage  LIKE LINE OF xt_typepusages.

*rrq comments Forward nodes are created in an old version of the
*create XML from object.  In that node, the only attribute set
*is the "TypeGroup".  All other attributes are hard coded on the
*create Object from XML .  To fix this and make it transparent to
*users, "forwaredDeclaration" nodes will be supported, and a new
*node will be added.
*if it is an old version XML document, forwardDeclarations nodes
*if it is a new version XML document, typeUsages nodes

    filter   = xmldoc->create_filter_name( c_xml_key_typepusage ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = ls_typepusage.
      APPEND ls_typepusage TO xt_typepusages.
      node ?= iterator->get_next( ).
    ENDWHILE.

* only check forwardDeclaration if typeUsages does not exist
* later version this will be removed
    IF xt_typepusages IS INITIAL.
      filter = xmldoc->create_filter_name( c_xml_key_forwarddeclaration ).
      iterator = xmldoc->create_iterator_filtered( filter ).
      node ?= iterator->get_next( ).

      WHILE node IS NOT INITIAL.
        CLEAR ls_typepusage.
        source = node->get_value( ).
        ls_typepusage-clsname = objname.
        ls_typepusage-version = '0'.
        ls_typepusage-tputype = '0'.
        ls_typepusage-explicit =  'X'.
        ls_typepusage-implicit = ''.
        ls_typepusage-typegroup = source.
        APPEND ls_typepusage TO xt_typepusages.
        node ?= iterator->get_next( ).
      ENDWHILE.
    ENDIF.

  ENDMETHOD.
  METHOD get_alias_method.
    DATA lo_alias  TYPE REF TO if_ixml_element.
    DATA ls_alias  TYPE seoaliases.
    DATA: l_rc          TYPE sy-subrc,
          ls_method     LIKE LINE OF it_methods,
          ls_clsmethkey TYPE seocmpkey.

    LOOP AT it_methods INTO ls_method.
      ls_clsmethkey-clsname = objname.
      ls_clsmethkey-cmpname = ls_method-name.
      CLEAR ls_alias.
      CALL FUNCTION 'SEO_ALIAS_GET'
        EXPORTING
          cmpkey       = ls_clsmethkey
*         VERSION      = SEOC_VERSION_INACTIVE
        IMPORTING
          alias        = ls_alias
        EXCEPTIONS
          not_existing = 1
          deleted      = 2
          OTHERS       = 3.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        lo_alias = xmldoc->create_element( c_xml_key_alias_method ).
        setattributesfromstructure( node      = lo_alias
                                    structure = ls_alias ).
        l_rc = xo_rootnode->append_child( lo_alias ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_clsdeferrd.
    DATA: lt_clsdeferrds TYPE seot_clsdeferrds_r,
          lo_clsdeferrds TYPE REF TO if_ixml_element,
          ls_clsdeferrd  TYPE seot_typepusage_r.

    DATA: l_rc        TYPE sy-subrc,
          ls_classkey TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_CLSDEFERRD_READ_ALL'
      EXPORTING
        cifkey            = ls_classkey
        version           = seoc_version_active
      IMPORTING
        classdeferreds    = lt_clsdeferrds
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.

    LOOP AT lt_clsdeferrds INTO ls_clsdeferrd.
      lo_clsdeferrds = xmldoc->create_element( c_xml_key_clsdeferrd ).
      setattributesfromstructure( node      = lo_clsdeferrds
                                  structure = ls_clsdeferrd ).
      l_rc = xo_rootnode->append_child( lo_clsdeferrds ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_intdeferrd.
    DATA: lt_intdeferrds TYPE seot_intdeferrds_r,
          lo_intdeferrds TYPE REF TO if_ixml_element,
          ls_intdeferrd  TYPE seot_intdeferrd_r.

    DATA: l_rc        TYPE sy-subrc,
          ls_classkey TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_INTDEFERRD_READ_ALL'
      EXPORTING
        cifkey             = ls_classkey
        version            = seoc_version_active
      IMPORTING
        interfacedeferreds = lt_intdeferrds
      EXCEPTIONS
        clif_not_existing  = 1
        OTHERS             = 2.

    LOOP AT lt_intdeferrds INTO ls_intdeferrd.
      lo_intdeferrds = xmldoc->create_element( c_xml_key_intdeferrd ).
      setattributesfromstructure( node      = lo_intdeferrds
                                  structure = ls_intdeferrd ).
      l_rc = xo_rootnode->append_child( lo_intdeferrds ).
    ENDLOOP.

  ENDMETHOD.
  METHOD get_otr.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA txtnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.

    DATA sotrheader TYPE sotr_head.
    DATA sotrtextline TYPE sotr_text.
    DATA sotrtexttable TYPE TABLE OF sotr_text.

    DATA _ixml TYPE REF TO if_ixml.
    DATA _xmldoc TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = otrguid
      IMPORTING
        header         = sotrheader
      TABLES
        entries        = sotrtexttable
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
    rootnode = xmldoc->create_element( c_xml_key_sotr ). "OTR object type
    CLEAR sotrheader-concept.                                 "ewH:33
    setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
    LOOP AT sotrtexttable INTO sotrtextline.
      txtnode = xmldoc->create_element( c_xml_key_sotrtext ).
      CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
      setattributesfromstructure(
        node = txtnode structure = sotrtextline ).
      rc = rootnode->append_child( txtnode ).
    ENDLOOP.

    node = rootnode.

  ENDMETHOD.
  METHOD get_typepusage.
    DATA: lt_typepusages TYPE seot_typepusages_r,
          lo_typepusages TYPE REF TO if_ixml_element,
          ls_typepusage  TYPE seot_typepusage_r.

    DATA: l_rc        TYPE sy-subrc,
          l_string    TYPE string,
          ls_classkey TYPE seoclskey.

    ls_classkey-clsname = objname.

    CALL FUNCTION 'SEO_TYPEPUSAGE_READ_ALL'
      EXPORTING
        cifkey            = ls_classkey
        version           = seoc_version_active
      IMPORTING
        typepusages       = lt_typepusages
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.

    LOOP AT lt_typepusages INTO ls_typepusage.
      lo_typepusages = xmldoc->create_element( c_xml_key_typepusage ).
      setattributesfromstructure( node      = lo_typepusages
                                  structure = ls_typepusage ).
      l_rc = xo_rootnode->append_child( lo_typepusages ).
    ENDLOOP.

*ewH: for version 0.1.3, we will continue to generate both nodes
* in order for upgradeability of saplink itself.  For version
* 2.0, forwardDeclaration node generations will be deprecated.
    LOOP AT lt_typepusages INTO ls_typepusage.
      lo_typepusages = xmldoc->create_element( c_xml_key_forwarddeclaration ).
      l_string       = ls_typepusage-typegroup.
      l_rc = lo_typepusages->if_ixml_node~set_value( l_string ).
      l_rc = xo_rootnode->append_child( lo_typepusages ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS zsaplink_class IMPLEMENTATION.
  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA classkey TYPE seoclskey.
    DATA not_active TYPE  seox_boolean.

    classkey-clsname = objname.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey       = classkey
      IMPORTING
        not_active   = not_active
      EXCEPTIONS
*       not_specified = 1
        not_existing = 2.
*      is_interface  = 3
*      no_text       = 4
*      inconsistent  = 5
*      others        = 6.

    IF sy-subrc <> 2.
      exists = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA localimplementation TYPE REF TO if_ixml_element.
    DATA localtypes TYPE REF TO if_ixml_element.
    DATA localmacros TYPE REF TO if_ixml_element.
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA reportlist TYPE STANDARD TABLE OF string.
    DATA includename TYPE program.
    DATA _classname TYPE seoclsname.
    DATA reportstring TYPE string.
    DATA rc TYPE sysubrc.
    DATA classdescr TYPE REF TO cl_abap_classdescr.
    DATA typedescr TYPE REF TO cl_abap_typedescr.
    DATA methoddescr TYPE abap_methdescr.
    DATA methodnode TYPE REF TO if_ixml_element.
    DATA parameternode TYPE REF TO if_ixml_element.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA exceptionnode TYPE REF TO if_ixml_element.
    DATA exceptionlist TYPE seos_exceptions_r.
    DATA anexception TYPE vseoexcep.
    DATA inheritancenode TYPE REF TO if_ixml_element.
    DATA redefnode TYPE REF TO if_ixml_element.

    DATA tempstring TYPE string.
    DATA methodkey TYPE seocpdkey.
    DATA clsmethkey TYPE seocmpkey.
    DATA methodproperties TYPE vseomethod.
    DATA classkey TYPE seoclskey.
    DATA classproperties TYPE vseoclass.
    DATA paramdescr TYPE abap_parmdescr.
    DATA paramkey TYPE seoscokey.
    DATA paramproperties TYPE vseoparam.
    DATA superclass TYPE REF TO cl_abap_typedescr.
    DATA superclassname TYPE string.
    DATA superclasskey TYPE seorelkey.

    DATA attribdescr TYPE abap_attrdescr.
    DATA attribkey TYPE seocmpkey.
    DATA attribproperties TYPE vseoattrib.
    DATA attribnode TYPE REF TO if_ixml_element.
    DATA inheritanceprops TYPE vseoextend.
    DATA redefines TYPE STANDARD TABLE OF seoredef
        WITH KEY clsname refclsname version mtdname.
    DATA inheritance TYPE seor_inheritance_r.
    DATA redefinitions TYPE seor_redefinitions_r.
    DATA redefinition LIKE LINE OF redefinitions.

    DATA otrnode TYPE REF TO if_ixml_element.
    DATA _otrguid TYPE sotr_conc.

    DATA: ls_version_info TYPE gts_version_info.

    _classname = objname.
    classkey-clsname = objname.

*  setObjectType( ).

    DATA _objtype TYPE string.
*  _objType = objType.
    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = classkey
        version      = '1'
      IMPORTING
        class        = classproperties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_found
              object = objname.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'class deleted'.
        WHEN 3.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'interfaces not supported'.
        WHEN 4.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'class is modeled only'.
      ENDCASE.
    ENDIF.

    setattributesfromstructure( node      = rootnode
                                structure = classproperties ).
*--------------------------------------------------------------------*
* Added versioning info
*--------------------------------------------------------------------*
    ls_version_info = get_version_info( ).
    setattributesfromstructure( node      = rootnode
                                structure = ls_version_info ).

    TRY.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = objname
          RECEIVING
            p_descr_ref    = typedescr
          EXCEPTIONS
            type_not_found = 1.
        IF sy-subrc = 0.
          classdescr ?= typedescr.
        ELSE.

        ENDIF.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDTRY.

    CALL METHOD classdescr->get_super_class_type
      RECEIVING
        p_descr_ref           = superclass
      EXCEPTIONS
        super_class_not_found = 1.

    IF sy-subrc = 0.
      superclassname = superclass->get_relative_name( ).
      IF NOT superclassname CS 'OBJECT'.
        superclasskey-clsname = objname.
        superclasskey-refclsname = superclassname.
        CALL FUNCTION 'SEO_INHERITANC_GET'
          EXPORTING
            inhkey        = superclasskey
          IMPORTING
            inheritance   = inheritanceprops
            redefinitions = redefines.
        setattributesfromstructure( node = rootnode structure =
        inheritanceprops ).
      ENDIF.
    ENDIF.

*/***TPJ - Added Logic for TYPES  -------------------*/
    DATA: types      TYPE seoo_types_r,
          wa_type    LIKE LINE OF types,
          types_node TYPE REF TO if_ixml_element.
    CALL FUNCTION 'SEO_TYPE_READ_ALL'
      EXPORTING
        cifkey            = classkey
        version           = 1
      IMPORTING
        types             = types
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT types INTO wa_type.
      types_node = xmldoc->create_element( 'types' ).
      CLEAR wa_type-typesrc_leng. " Will be recalculated on import, differs depending on OS due to linebreaks
      setattributesfromstructure( node = types_node structure =
      wa_type ).
      rc = rootnode->append_child( types_node ).
    ENDLOOP.
*/***TPJ - End of Added Logic for TYPES  -------------------*/

*/***TPJ - Added Logic for Friends  -------------------*/
    DATA: clif_keys    TYPE STANDARD TABLE OF seoclskey,
          friends      TYPE STANDARD TABLE OF seofriends,
          wa_friend    LIKE LINE OF friends,
          friends_node TYPE REF TO if_ixml_element.

    APPEND classkey TO clif_keys.
    CALL FUNCTION 'SEO_FRIENDS_SELECT'
      EXPORTING
        with_external_ref = 'X'
      TABLES
        clif_keys         = clif_keys
        friends_relations = friends.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT friends INTO wa_friend.
      friends_node = xmldoc->create_element( c_xml_key_friends ).
      setattributesfromstructure( node = friends_node structure =
      wa_friend ).
      rc = rootnode->append_child( friends_node ).
    ENDLOOP.
*/***TPJ - End of Added Logic for Friends  -------------------*/

*/***ewH - Added Logic for Interfaces  -------------------*/
*/***uku - discard included interfaces -------------------*/
    DATA: it_implementings      TYPE seor_implementings_r,
          lt_implementings_copy TYPE seor_implementings_r,
          wa_implementings      LIKE LINE OF it_implementings,
          implementingnode      TYPE REF TO if_ixml_element,
          ls_interface          TYPE seoc_interface_r,
          lt_comprisings        TYPE seor_comprisings_r,
          ls_intfkey            TYPE seoclskey.
    FIELD-SYMBOLS <ls_comprisings> TYPE seor_comprising_r.

    CALL FUNCTION 'SEO_IMPLEMENTG_READ_ALL'
      EXPORTING
        clskey             = classkey
      IMPORTING
        implementings      = it_implementings
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.

    lt_implementings_copy = it_implementings.
    LOOP AT it_implementings INTO wa_implementings.
      CLEAR: ls_intfkey.
      ls_intfkey-clsname = wa_implementings-refclsname.
      CALL FUNCTION 'SEO_INTERFACE_TYPEINFO_GET'
        EXPORTING
          intkey      = ls_intfkey
        IMPORTING
          comprisings = lt_comprisings.
      LOOP AT lt_comprisings ASSIGNING <ls_comprisings>.
        DELETE lt_implementings_copy WHERE refclsname = <ls_comprisings>-refclsname.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_implementings_copy INTO wa_implementings.
      implementingnode = xmldoc->create_element( 'implementing' ).
      setattributesfromstructure( node = implementingnode structure =
      wa_implementings ).
      rc = rootnode->append_child( implementingnode ).
    ENDLOOP.
*/***uku - End of discard included interfaces -------------------*/
*/***ewH - End of Added Logic for Interfaces  -------------------*/
*/***rrq - Added Logic for EVENTS  -------------------*/
    DATA: events      TYPE seoo_events_r,
          wa_event    LIKE LINE OF events,
          event_node  TYPE REF TO if_ixml_element,
          eventkey    TYPE seocmpkey,
          eventparams TYPE seos_parameters_r,
          wa_params   TYPE seos_parameter_r.
    CALL FUNCTION 'SEO_EVENT_READ_ALL'
      EXPORTING
        cifkey            = classkey
        version           = 1
      IMPORTING
        events            = events
      EXCEPTIONS
        clif_not_existing = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT events INTO wa_event.
      eventkey-clsname = wa_event-clsname.
      eventkey-cmpname = wa_event-cmpname.
      event_node = xmldoc->create_element( 'events' ).
      setattributesfromstructure( node = event_node structure =
      wa_event ).
      CALL FUNCTION 'SEO_EVENT_SIGNATURE_GET'
        EXPORTING
          evtkey     = eventkey
        IMPORTING
          parameters = eventparams.

*   parameters
      LOOP AT eventparams INTO wa_params.

        parameternode = xmldoc->create_element( 'parameter' ).
        setattributesfromstructure( node = parameternode
        structure = wa_params ).
        rc = event_node->append_child( parameternode ).
      ENDLOOP.
      rc = rootnode->append_child( event_node ).
    ENDLOOP.
*/***rrq - End of Added Logic for EVENTS  -------------------*/
* removed by Rene.
    get_sections( CHANGING rootnode = rootnode ) .
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccimp_name( _classname ).
    READ REPORT includename INTO reportlist.
    localimplementation = xmldoc->create_element( 'localImplementation' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localimplementation->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccdef_name( _classname ).
    READ REPORT includename INTO reportlist.
    localtypes = xmldoc->create_element( 'localTypes' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localtypes->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
    includename = cl_oo_classname_service=>get_ccmac_name( _classname ).
    READ REPORT includename INTO reportlist.
    localmacros = xmldoc->create_element( 'localMacros' ).
    reportstring = buildsourcestring( sourcetable = reportlist ).
    rc = localmacros->if_ixml_node~set_value( reportstring ).
*|--------------------------------------------------------------------|
*/***EVP - Added Logic for Local Test Classes  ----------------------*/
    DATA localtestclasses TYPE REF TO if_ixml_element.
    DATA localtestclassesexist TYPE i.

    includename = cl_oo_classname_service=>get_local_testclasses_include( _classname ).
    READ REPORT includename INTO reportlist.
    " If sy-subrc = 0 the local test classes do exist
    localtestclassesexist = sy-subrc.
    IF localtestclassesexist = 0.
      localtestclasses = xmldoc->create_element( 'localTestClasses' ).
      reportstring = buildsourcestring( sourcetable = reportlist ).
      rc = localtestclasses->if_ixml_node~set_value( reportstring ).
    ENDIF.
*/***EVP - End of Added Logic for Local Test Classes  ---------------*/
*|                                                                    |
*\--------------------------------------------------------------------/
    rc = rootnode->append_child( localimplementation ).
    rc = rootnode->append_child( localtypes ).
    rc = rootnode->append_child( localmacros ).
*/***EVP - Added Logic for Local Test Classes  -------------------*/
    IF localtestclassesexist = 0.
      rc = rootnode->append_child( localtestclasses ).
    ENDIF.
*/***EVP - End of Added Logic for Local Test Classes  ------------*/
**// Rich:  Start
    get_textpool( CHANGING rootnode = rootnode ).
    get_documentation( CHANGING rootnode = rootnode ).
**// Rich:  End
    get_typepusage( CHANGING  xo_rootnode = rootnode ).
    get_clsdeferrd( CHANGING  xo_rootnode = rootnode ).
    get_intdeferrd( CHANGING  xo_rootnode = rootnode ).

*  classDescriptor ?= cl_abap_typedescr=>describe_by_name( className ).
    attribkey-clsname = objname.

    LOOP AT classdescr->attributes INTO attribdescr
    WHERE is_inherited = abap_false
    AND is_interface = abap_false. "rrq:issue 46
      attribnode = xmldoc->create_element( 'attribute' ).
      attribkey-cmpname = attribdescr-name.
      CALL FUNCTION 'SEO_ATTRIBUTE_GET'
        EXPORTING
          attkey    = attribkey
        IMPORTING
          attribute = attribproperties.

*   include OTR if necessary (for exception classes)
      IF attribproperties-type = 'SOTR_CONC' AND attribproperties-attvalue
      IS NOT INITIAL.
        _otrguid = attribproperties-attvalue+1(32).
        otrnode = get_otr( _otrguid ).
        IF otrnode IS BOUND.
          rc = attribnode->append_child( otrnode ).
          " Issue #222 - get_text empty when ZCX_SAPLINK exception is raised
          " Gregor Wolf, 2012-12-20
          " As GUID for OTR Node is created new in every system we import
          " the Slinkee we should empty it
          CLEAR: attribproperties-attvalue.
        ENDIF.
      ENDIF.

*   append attribute node to parent node
      setattributesfromstructure( node      = attribnode
                                  structure = attribproperties ).
      rc = rootnode->append_child( attribnode ).
    ENDLOOP.

*// ewH: begin of logic for interface methods & inheritance redesign-->
* inheritances & redefinitions: old source removed-recover w/subversion
    CALL FUNCTION 'SEO_INHERITANC_READ'
      EXPORTING
        clskey             = classkey
      IMPORTING
        inheritance        = inheritance
        redefinitions      = redefinitions
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.

    IF inheritance IS NOT INITIAL.
      inheritancenode = xmldoc->create_element( c_xml_key_inheritance ).
      setattributesfromstructure( node = inheritancenode structure =
      inheritance ).

      LOOP AT redefinitions INTO redefinition.
        redefnode = xmldoc->create_element( 'redefinition' ).
        setattributesfromstructure( node = redefnode structure =
        redefinition ).
        rc = inheritancenode->append_child( redefnode ).
      ENDLOOP.
      rc = rootnode->append_child( inheritancenode ).
    ENDIF.

* methods with out alias We handle this later
    LOOP AT classdescr->methods INTO methoddescr WHERE alias_for IS INITIAL AND
    NOT ( is_inherited = 'X' AND is_redefined IS INITIAL ).
      methodkey-clsname = _classname.
      methodkey-cpdname = methoddescr-name.
*//nbus: added logic for exception class
      IF    methoddescr-name         =  'CONSTRUCTOR'
        AND classproperties-category =  seoc_category_exception
        AND me->mv_steamroller       <> abap_true.
        " Constructor() will be generated automatically into the
        " target system once the class is saved
        CONTINUE.
      ENDIF.
*//nbus: end of added logic for exception class
*   interface methods
      IF methoddescr-is_interface = 'X'.
        CALL METHOD cl_oo_classname_service=>get_method_include
          EXPORTING
            mtdkey              = methodkey
          RECEIVING
            result              = includename
          EXCEPTIONS
            method_not_existing = 1.
        IF sy-subrc = 0.
          methodnode = xmldoc->create_element( 'interfaceMethod' ).
          setattributesfromstructure( node = methodnode structure =
          methodkey ).
          sourcenode = xmldoc->create_element( 'source' ).
*        tempString = includeName.
*        rc = sourceNode->set_attribute(
*          name = 'includeName' value = tempString ).
          READ REPORT includename INTO reportlist.
          reportstring = buildsourcestring( sourcetable = reportlist ).
          rc = sourcenode->if_ixml_node~set_value( reportstring ).
          rc = methodnode->append_child( sourcenode ).
          rc = rootnode->append_child( methodnode ).
        ENDIF.
*   other methods
      ELSE.
        clsmethkey-clsname = _classname.
        clsmethkey-cmpname = methoddescr-name.
        CLEAR methodproperties.

        IF methoddescr-is_redefined = 'X'.
          methodnode = xmldoc->create_element( 'method' ).
          MOVE-CORRESPONDING clsmethkey TO methodproperties.
*// ewh: begin of forward compatibility hack, can be removed for next
*//      major release-->
          READ TABLE redefinitions INTO redefinition
            WITH KEY mtdname = methoddescr-name.
          IF sy-subrc = 0.
            methodproperties-clsname = redefinition-refclsname.
          ENDIF.
*//<--ewH: end of forward compatibility hack
          setattributesfromstructure( node = methodnode structure =
          methodproperties ).
        ELSE.
          CALL FUNCTION 'SEO_METHOD_GET'
            EXPORTING
              mtdkey       = clsmethkey
            IMPORTING
              method       = methodproperties
            EXCEPTIONS
              not_existing = 1.
          IF sy-subrc = 0.
            methodnode = xmldoc->create_element( 'method' ).
            setattributesfromstructure( node = methodnode structure =
            methodproperties ).

*         parameters
            LOOP AT methoddescr-parameters INTO paramdescr.
              CLEAR paramproperties.
              parameternode = xmldoc->create_element( 'parameter' ).
              paramkey-cmpname = clsmethkey-cmpname.
              paramkey-sconame = paramdescr-name.
              paramkey-clsname = objname.
              CALL FUNCTION 'SEO_PARAMETER_GET'
                EXPORTING
                  parkey    = paramkey
                  version   = '1'
                IMPORTING
                  parameter = paramproperties.
              setattributesfromstructure( node = parameternode
              structure = paramproperties ).
              rc = methodnode->append_child( parameternode ).
            ENDLOOP.

*         exceptions
            CALL FUNCTION 'SEO_METHOD_SIGNATURE_GET'
              EXPORTING
                mtdkey  = clsmethkey
                version = '1'
              IMPORTING
                exceps  = exceptionlist.
            LOOP AT exceptionlist INTO anexception.
              exceptionnode = xmldoc->create_element( 'exception' ).
              setattributesfromstructure( node = exceptionnode
              structure = anexception ).
              rc = methodnode->append_child( exceptionnode ).
            ENDLOOP.
          ENDIF. "method found
        ENDIF. "is_redefined?
*     source
        CALL METHOD cl_oo_classname_service=>get_method_include
          EXPORTING
            mtdkey              = methodkey
          RECEIVING
            result              = includename
          EXCEPTIONS
            method_not_existing = 1.
        IF sy-subrc = 0.
          READ REPORT includename INTO reportlist.
          reportstring = buildsourcestring( sourcetable = reportlist ).
          sourcenode = xmldoc->create_element( 'source' ).
          rc = sourcenode->if_ixml_node~set_value( reportstring ).
          rc = methodnode->append_child( sourcenode ).
        ENDIF.
** StartInsert Rich - Handle method documenation
        get_method_documentation(  EXPORTING method_key = methodkey
                                   CHANGING  rootnode   = methodnode ).
** EndInsert Rich - Handle method documenation
        rc = rootnode->append_child( methodnode ).
      ENDIF. "is_interface?
    ENDLOOP.
* create alias info for load.
    get_alias_method( EXPORTING it_methods     = classdescr->methods
                      CHANGING  xo_rootnode    = rootnode ).
* append root node to xmldoc
    rc = xmldoc->append_child( rootnode ).
    ixmldocument = xmldoc.
*// <--ewH: end of logic for interface methods & inheritance redesign
  ENDMETHOD.
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA classkey TYPE seoclskey.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA iterator TYPE REF TO if_ixml_node_iterator.
    DATA node TYPE REF TO if_ixml_element.
    DATA otrnode TYPE REF TO if_ixml_element.
    DATA filter2 TYPE REF TO if_ixml_node_filter.
    DATA iterator2 TYPE REF TO if_ixml_node_iterator.
    DATA superclass TYPE vseoextend-clsname.
    DATA superclasskey TYPE vseoextend.
    DATA methodsourcenode TYPE REF TO if_ixml_node.
    DATA sourcenode TYPE REF TO if_ixml_node.
    DATA source TYPE string.
    DATA sourcetable TYPE TABLE OF string.
    DATA methodkey TYPE seocpdkey.
    DATA node2 TYPE REF TO if_ixml_element.
    DATA _objtype TYPE string.
    DATA aobjname TYPE e071-obj_name.
    DATA inheritancenode TYPE REF TO if_ixml_element.
    DATA redefnode TYPE REF TO if_ixml_element.
    DATA includename TYPE program.
    DATA mtdkey   TYPE seocpdkey.

*data excClass type ref to ZCX_SAPLINK.

*// --> begin of new data type rrq
    DATA:
*exporting dataTypes
      e_corrnr            TYPE trkorr,
      e_devclass          TYPE devclass,
      e_version           TYPE seoversion,
      e_genflag           TYPE genflag,
      e_authority_check   TYPE seox_boolean,
      e_overwrite         TYPE seox_boolean,
*e_suppress_meth_gen      type SEOX_BOOLEAN,
*e_suppress_refac_gen     type SEOX_BOOLEAN,
      e_method_sources    TYPE seo_method_source_table,
      e_locals_def        TYPE rswsourcet,
      e_locals_imp        TYPE rswsourcet,
      e_locals_mac        TYPE rswsourcet,
*e_suppress_ind_update    type SEOX_BOOLEAN,
*importing dataTypes
      i_korrnr            TYPE trkorr,
*changing dataTypes
      ch_class            TYPE vseoclass,
      ch_inheritance      TYPE vseoextend,
      ch_redefinitions    TYPE seor_redefinitions_r,
      ch_implementings    TYPE seor_implementings_r,
      ch_impl_details     TYPE seo_redefinitions,
      ch_attributes       TYPE seoo_attributes_r,
      ch_methods          TYPE seoo_methods_r,
      ch_events           TYPE seoo_events_r,
      ch_types            TYPE seoo_types_r,
      ch_type_source      TYPE seop_source,
      ch_type_source_temp TYPE seop_source,
      ch_parameters       TYPE seos_parameters_r,
      ch_exceps           TYPE seos_exceptions_r,
      ch_aliases          TYPE seoo_aliases_r,
      ch_typepusages      TYPE seot_typepusages_r,
      ch_clsdeferrds      TYPE seot_clsdeferrds_r,
      ch_intdeferrds      TYPE seot_intdeferrds_r,
      ch_friendships      TYPE seo_friends,
**table dataTypes
*tb_classDescription      type table of seoclasstx,
*tb_component_descr       type table of seocompotx,
*tb_subcomponent_descr    type table of seosubcotx,
* work areas for the tables
      wa_attributes       TYPE seoo_attribute_r,
      wa_types            TYPE seoo_type_r,
      wa_friends          TYPE seofriends,
      wa_implementings    TYPE seor_implementing_r,
      wa_redefinitions    TYPE seoredef,
      wa_methods          TYPE seoo_method_r,
      wa_parameters       TYPE seos_parameter_r,
      wa_exceps           TYPE seos_exception_r,
      wa_method_sources   TYPE seo_method_source,
      wa_events           TYPE seoo_event_r.
    DATA: lines TYPE i,
          l_msg TYPE string.
*//<-- end of new data types rrq

    CALL FUNCTION 'SEO_BUFFER_INIT'.

    e_devclass = devclass.
    _objtype = getobjecttype( ).
    e_overwrite = overwrite.
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).

    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = ch_class.

    objname = classkey-clsname = ch_class-clsname.
    ch_class-version = '0'.
    superclass = rootnode->get_attribute( name = 'REFCLSNAME' ).
    IF superclass IS NOT INITIAL.
* set something for inheritence
      superclasskey-clsname = classkey-clsname.
      superclasskey-refclsname = superclass.
      superclasskey-version = '0'.
      superclasskey-state = '1'.
      MOVE-CORRESPONDING superclasskey TO ch_inheritance.
      ch_inheritance-author = 'BCUSER'.
      ch_inheritance-createdon = sy-datum.
    ENDIF.

*Add attributes to new class
    DATA otrconcept TYPE sotr_text-concept.
    filter = xmldoc->create_filter_name( 'attribute' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
*   create OTR texts if necessary (for exception classes)
      CLEAR otrconcept.
      otrnode = node->find_from_name( c_xml_key_sotr ).
      IF otrnode IS NOT INITIAL.
*     ewH:33-->create new concept with new guid
*      me->createotrfromnode( otrnode ).
        me->create_otr(
          EXPORTING node = otrnode
          IMPORTING concept = otrconcept ).
      ENDIF.
      CLEAR wa_attributes.
*   create attribute
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_attributes.
      wa_attributes-version = '0'.
*   ewH:issue33-->6.40 and above, must create new concept
      IF otrconcept IS NOT INITIAL.
        CONCATENATE `'` otrconcept `'` INTO wa_attributes-attvalue.
      ENDIF.
      APPEND wa_attributes TO ch_attributes.
      node ?= iterator->get_next( ).
    ENDWHILE.

*/***TPJ - Added Logic for TYPES  -------------------*/
*  DATA: types           TYPE seoo_types_r,
*        type_properties LIKE LINE OF types.

    filter = xmldoc->create_filter_name( 'types' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_types.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_types.
      wa_types-version = '0'.
      APPEND wa_types TO ch_types.
      node ?= iterator->get_next( ).
    ENDWHILE.
*/***TPJ - End of Added Logic for TYPES  -------------------*/

*/***TPJ - Added Logic for Friends  -------------------*/
*  DATA: wa_friends type seofriends.

    filter = xmldoc->create_filter_name( c_xml_key_friends ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_friends.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_friends.
      wa_friends-version = '0'.
      APPEND wa_friends TO ch_friendships.
      node ?= iterator->get_next( ).
    ENDWHILE.
*/***TPJ - End of Added Logic for Friends  -------------------*/

*// ewH: Added Logic for Implementings(interfaces)-->
    filter = xmldoc->create_filter_name( 'implementing' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_implementings.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_implementings.
      APPEND wa_implementings TO ch_implementings.
      node ?= iterator->get_next( ).
    ENDWHILE.
*//<--ewH: End of Added Logic for Implementings(interfaces)

*// rrq: Added Logic for events-->
    filter = xmldoc->create_filter_name( 'events' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_events.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_events.
      APPEND wa_events TO ch_events.
      filter2 = node->create_filter_name( 'parameter' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CLEAR wa_parameters.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_parameters.

        "//-> Mar: Added logic for parameter/interface implementation - 08/20/2008
        IF NOT wa_parameters-clsname IS INITIAL.
          APPEND wa_parameters TO ch_parameters.
        ENDIF.
        "//<- Mar: Added logic for parameter/interface implementation - 08/20/2008

        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      node ?= iterator->get_next( ).
    ENDWHILE.
*//<--rrq: End of Added Logic for events

*// ewH: start redesign method/inheritances-->
* inheritance
    inheritancenode = rootnode->find_from_name( c_xml_key_inheritance ).
    IF inheritancenode IS BOUND.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = inheritancenode
        CHANGING
          structure = ch_inheritance.
*   redefs
      filter = inheritancenode->create_filter_name( 'redefinition' ).
      iterator = inheritancenode->create_iterator_filtered( filter ).
      redefnode ?= iterator->get_next( ).
      WHILE redefnode IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = redefnode
          CHANGING
            structure = wa_redefinitions.
        APPEND wa_redefinitions TO ch_redefinitions.
        redefnode ?= iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*Add Methods to new class
    filter = xmldoc->create_filter_name( 'method' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CLEAR wa_methods.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_methods.

*   only create metadata if method is not a redefinition
      READ TABLE ch_redefinitions INTO wa_redefinitions
      WITH KEY mtdname = wa_methods-cmpname.
      IF sy-subrc = 0.
        node ?= iterator->get_next( ).
        CONTINUE.
      ENDIF.
*// ewh: begin of backward compatibility hack, can be removed for next
*//      major release-->
      IF wa_methods-clsname <> ch_class-clsname.
        MOVE-CORRESPONDING wa_methods TO wa_redefinitions.
        wa_redefinitions-clsname = ch_class-clsname.
        wa_redefinitions-refclsname = wa_methods-clsname.
        wa_redefinitions-version = '0'.
        wa_redefinitions-mtdabstrct = ''.
        wa_redefinitions-mtdname = wa_methods-cmpname.
        APPEND wa_redefinitions TO ch_redefinitions.

        node ?= iterator->get_next( ).
        CONTINUE.
      ENDIF.
*// <--ewH: break in backward compatibility hack - 2Bcontinued below

      filter2 = node->create_filter_name( 'parameter' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CLEAR wa_parameters.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_parameters.

        "//-> Mar: Added logic for parameter/interface implementation - 08/20/2008
        IF NOT wa_parameters-clsname IS INITIAL.
          APPEND wa_parameters TO ch_parameters.
        ENDIF.
        "//<- Mar: Added logic for parameter/interface implementation - 08/20/2008

        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      filter2 = node->create_filter_name( 'exception' ).
      iterator2 = node->create_iterator_filtered( filter2 ).
      node2 ?= iterator2->get_next( ).
      WHILE node2 IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = node2
          CHANGING
            structure = wa_exceps.
        APPEND wa_exceps TO ch_exceps.
        node2 ?= iterator2->get_next( ).
      ENDWHILE.
      APPEND wa_methods TO ch_methods.

** StartInsert Rich - Handle method documenation
      create_method_documentation( node = node ).
** EndInsert Rich - Handle method documenation

      node ?= iterator->get_next( ).
    ENDWHILE.
*// <--ewH: end redesign method/inheritances
*// ewh: continuation of backward compatibility hack-->
*  IF ( ch_redefinitions IS NOT INITIAL OR superclass-clsname
*  IS NOT INITIAL ) and ch_inheritance is initial.
*    CALL FUNCTION 'SEO_INHERITANC_CREATE_F_DATA'
*      EXPORTING
*        save          = ' '
*      CHANGING
*        inheritance   = superclasskey
*        redefinitions = ch_redefinitions.
*  ENDIF.
*// <--ewH: end of backward compatibility hack

    create_typepusage( CHANGING xt_typepusages = ch_typepusages ).
    create_clsdeferrd( CHANGING xt_clsdeferrds = ch_clsdeferrds ).
    create_intdeferrd( CHANGING xt_intdeferrds = ch_intdeferrds ).

*Insert source code into the methods
    filter = xmldoc->create_filter_name( 'method' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = wa_methods.
      methodkey-clsname = objname.
      methodkey-cpdname = wa_methods-cmpname.
      aobjname = methodkey.
      methodsourcenode = node->find_from_name( 'source' ).
      IF methodsourcenode IS NOT INITIAL.
        CLEAR wa_method_sources.
        source = methodsourcenode->get_value( ).
        sourcetable = buildtablefromstring( source ).
        READ TABLE ch_redefinitions INTO wa_redefinitions
        WITH KEY mtdname = methodkey-cpdname.
        IF sy-subrc = 0.
          wa_method_sources-redefine = 'X'.
        ENDIF.
        wa_method_sources-cpdname = methodkey-cpdname.
        wa_method_sources-source = sourcetable.
        APPEND wa_method_sources TO e_method_sources.
      ENDIF.
      node ?= iterator->get_next( ).
    ENDWHILE.
*
**// ewH: create interface methods-->
    filter = xmldoc->create_filter_name( 'interfaceMethod' ).
    iterator = xmldoc->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).

    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = methodkey.
      aobjname = methodkey.
      methodsourcenode = node->find_from_name( 'source' ).
      IF methodsourcenode IS NOT INITIAL.
        CLEAR wa_method_sources.
        source = methodsourcenode->get_value( ).
        sourcetable = buildtablefromstring( source ).
        wa_method_sources-cpdname = methodkey-cpdname.
        READ TABLE ch_redefinitions INTO wa_redefinitions
        WITH KEY mtdname = methodkey-cpdname.
        IF sy-subrc = 0.
          wa_method_sources-redefine = 'X'.
        ENDIF.
*      wa_method_sources-redefine = wa_methods-redefin.
        wa_method_sources-source = sourcetable.

        APPEND wa_method_sources TO e_method_sources.
      ENDIF.

      node ?= iterator->get_next( ).
    ENDWHILE.
*// <--ewH: end create interface methods

* local implementation
    DATA _classname TYPE seoclsname.
    _classname = objname.
    sourcenode = xmldoc->find_from_name( 'localImplementation' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_imp = buildtablefromstring( source ).
    ENDIF.

* local types
    sourcenode = xmldoc->find_from_name( 'localTypes' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_def = buildtablefromstring( source ).
    ENDIF.

* local macros
    sourcenode = xmldoc->find_from_name( 'localMacros' ).
    IF sourcenode IS NOT INITIAL.
      source = sourcenode->get_value( ).
      e_locals_mac = buildtablefromstring( source ).
    ENDIF.
* We don't need the sections for now. Code moved by Rene
    create_sections( ).

*Add Alias to new class
    create_alias_method( CHANGING xt_aliases_method = ch_aliases ).

    name = objname.

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        corrnr          = e_corrnr
        devclass        = e_devclass
        version         = e_version
        genflag         = e_genflag
        authority_check = e_authority_check
        overwrite       = e_overwrite
*       SUPPRESS_METHOD_GENERATION         = e_suppress_meth_gen
*       SUPPRESS_REFACTORING_SUPPORT       = e_suppress_refac_gen
*       method_sources  = e_method_sources
        locals_def      = e_locals_def
        locals_imp      = e_locals_imp
        locals_mac      = e_locals_mac
*       SUPPRESS_INDEX_UPDATE              = e_suppress_ind_update
      IMPORTING
        korrnr          = i_korrnr
* TABLES
*       CLASS_DESCRIPTIONS                 = tb_classDescription
*       COMPONENT_DESCRIPTIONS             = tb_component_descr
*       SUBCOMPONENT_DESCRIPTIONS          = tb_subcomponent_descr
      CHANGING
        class           = ch_class
        inheritance     = ch_inheritance
        redefinitions   = ch_redefinitions
        implementings   = ch_implementings
        impl_details    = ch_impl_details
        attributes      = ch_attributes
        methods         = ch_methods
        events          = ch_events
        types           = ch_types
*       TYPE_SOURCE     = ch_type_source "???
        parameters      = ch_parameters
        exceps          = ch_exceps
        aliases         = ch_aliases
        typepusages     = ch_typepusages
        clsdeferrds     = ch_clsdeferrds
        intdeferrds     = ch_intdeferrds
        friendships     = ch_friendships
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    CASE sy-subrc.
      WHEN '0'.
** i guess if we made it this far, we will assume success
** successful install
      WHEN '1'.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
* Now let's add the methods
    LOOP AT e_method_sources INTO wa_method_sources.
      mtdkey-clsname = objname.
      mtdkey-cpdname = wa_method_sources-cpdname.

      CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
        EXPORTING
          mtdkey                         = mtdkey
          version                        = e_version
          force                          = e_overwrite
          redefine                       = wa_method_sources-redefine
*         SUPPRESS_CORR                  = SEOX_FALSE
          implementation_expanded        = wa_method_sources-source
*         IMPLEMENTATION                 =
          suppress_mtdkey_check          = seox_true
*         EDITOR_LOCK                    = SEOX_FALSE
*         GENERATED                      = SEOX_FALSE
          corrnr                         = e_corrnr
          without_method_frame           = seox_true
*         WITH_SUPER_CALL                = SEOX_FALSE
*         SUPPRESS_INDEX_UPDATE          = SEOX_FALSE
*         EXTEND                         = SEOX_FALSE
*         ENHANCEMENT                    = ' '
*         SUPPRESS_MODIFICATION_SUPPORT  = SEOX_FALSE
        EXCEPTIONS
          not_existing                   = 1
          model_only                     = 2
          include_existing               = 3
          method_imp_not_generated       = 4
          method_imp_not_initialised     = 5
          _internal_class_not_existing   = 6
          _internal_method_overflow      = 7
          cancelled                      = 8
          method_is_abstract_implemented = 9
          method_is_final_implemented    = 10
          internal_error_insert_report   = 11
          OTHERS                         = 12.
      CASE sy-subrc.
        WHEN '0'.
** i guess if we made it this far, we will assume success
** successful install
        WHEN '3'.
          l_msg = mtdkey.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>existing
              msg    = l_msg.
        WHEN OTHERS.
          l_msg = mtdkey.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error
              msg    = l_msg.
      ENDCASE.
    ENDLOOP.

*ewH:insert pub, prot, and priv sections manually to keep any direct
* attribute/type definitions
    aobjname = classkey-clsname.
**public
    sourcenode = xmldoc->find_from_name( 'publicSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_pubsec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_public STATE 'I'.
    ENDIF.

**protected
    sourcenode = xmldoc->find_from_name( 'protectedSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_prosec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_protected STATE 'I'.
    ENDIF.

**private
    sourcenode = xmldoc->find_from_name( 'privateSection' ).
    IF sourcenode IS NOT INITIAL.
      includename = cl_oo_classname_service=>get_prisec_name( _classname ).
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).
      INSERT REPORT includename FROM sourcetable EXTENSION TYPE
      srext_ext_class_private STATE 'I'.
    ENDIF.
*/***EVP - Added Logic for Local Test Classes  -------------------*/
**local test classes
    sourcenode = xmldoc->find_from_name( 'localTestClasses' ).
    IF sourcenode IS NOT INITIAL.
      DATA clskey TYPE seoclskey.
      source = sourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).

      clskey-clsname = _classname.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                 = clskey
          force                  = overwrite
          locals_testclasses     = sourcetable
        EXCEPTIONS
          not_existing           = 1
          model_only             = 2
          locals_not_generated   = 3
          locals_not_initialised = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
*/***EVP - End Of Added Logic for Local Test Classes  -------------------*/

**// Rich:  Start
* Create class textpool
    create_textpool( ).

    create_documentation( ).
**// Rich:  End

* insert inactive sections into worklist
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPUB'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPRO'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'CPRI'
        obj_name          = aobjname
      EXCEPTIONS
        wrong_object_name = 1.
    IF sy-subrc <> 0.
    ENDIF.


  ENDMETHOD.
  METHOD create_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA docnode          TYPE REF TO if_ixml_element.

    DATA lang_node        TYPE REF TO if_ixml_element.
    DATA lang_filter      TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA class_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    docnode = xmldoc->find_from_name( c_xml_key_class_documentation ).

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    class_name = docnode->get_attribute( name = c_xml_key_object ).
    obj_name = class_name.

* If no class name, then there was no class documenation, just return.
    IF class_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( c_xml_key_language ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = c_xml_key_spras ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( c_xml_key_textline ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'CL'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'CL'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Class Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_method_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA: methdocnode     TYPE REF TO if_ixml_element.

    DATA lang_node        TYPE REF TO if_ixml_element.
    DATA lang_filter      TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA classmeth_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    methdocnode = node->find_from_name( 'methodDocumentation' ).

    IF methdocnode IS NOT BOUND.
      RETURN.
    ENDIF.

    classmeth_name = methdocnode->get_attribute( name = 'OBJECT' ).
    obj_name = classmeth_name.

* If no class method name, then there was no class method documenation, just return.
    IF classmeth_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = methdocnode->create_filter_name( `language` ).
    lang_iterator = methdocnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'CO'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'CO'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Class Method Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_sections.

*ewH-not sure how this type_source param works. type sources can come
* from private or protected sections, but there is no way to pass
* these separately into the class create FM. After debugging into
* FM->clif_save_all->generate_classpool it treats the source table
* as one, so I am not sure how to get it to differentiate between
* private and protected sections. If only one section has types
* defined, the FM call works, otherwise all hell breaks loose. To
* solve the problem for now, we will just do an insert report for
* the sections after the class creation, since that's all the FM
* does in the end anyway. Wow, this is a really long comment, but
* I dont want to have to try to remember what the hell was going
* on here later...sorry.  :)
*insert code for publicSection
*  sourcenode = xmldoc->find_from_name( 'publicSection' )
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source = buildtablefromstring( source ).
*  ENDIF.
**insert code for pivateSection
*  sourcenode = xmldoc->find_from_name( 'privateSection' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source_temp = buildtablefromstring( source ).
*    append lines of ch_type_source_temp to ch_type_source.
*  ENDIF.
**insert code for ProtectedSection
*  sourcenode = xmldoc->find_from_name( 'protectedSection' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    ch_type_source_temp = buildtablefromstring( source ).
*    append lines of ch_type_source_temp to ch_type_source.
*  ENDIF.

  ENDMETHOD.
  METHOD create_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA langiterator TYPE REF TO if_ixml_node_iterator.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA textfilter TYPE REF TO if_ixml_node_filter.
    DATA textiterator TYPE REF TO if_ixml_node_iterator.
    DATA textpoolnode TYPE REF TO if_ixml_element.
    DATA langnode TYPE REF TO if_ixml_element.
    DATA atextnode TYPE REF TO if_ixml_element.
    DATA _objname TYPE trobj_name.
    DATA obj_name TYPE seoclsname.
    DATA lang TYPE spras.
    DATA langnodeexists TYPE flag.
*  data logonLanguageExists type flag.                  " del #255 - seemingly not used
    DATA _state(1) TYPE c.
    DATA classpoolname TYPE program.
    DATA lv_original_language TYPE sylangu.                " ins #255

    textpoolnode = xmldoc->find_from_name( 'textPool' ).

    IF textpoolnode IS NOT BOUND.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Ticket #255 - Error importing texts when logon language different
*               then original language of class
*--------------------------------------------------------------------*
    textpoolnode = xmldoc->find_from_name( 'CLAS' ).              " ins #255
    lv_original_language = textpoolnode->get_attribute( 'LANGU' )." ins #255
    SET LANGUAGE lv_original_language. " ins #255
    " Gregor Wolf: With this all languages from the Nugget/Slinkee are imported

    obj_name = objname.
    classpoolname = cl_oo_classname_service=>get_classpool_name( obj_name ).
    _objname = classpoolname.

    filter = textpoolnode->create_filter_name( 'language' ).
    langiterator = textpoolnode->create_iterator_filtered( filter ).
    langnode ?= langiterator->get_next( ).

    WHILE langnode IS NOT INITIAL.
      langnodeexists = 'X'.

      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          object   = 'REPT'
          obj_name = _objname
        EXCEPTIONS
          OTHERS   = 0.
      REFRESH textpooltable.
      textiterator = langnode->create_iterator( ).
      atextnode ?= textiterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      atextnode ?= textiterator->get_next( ).
      WHILE atextnode IS NOT INITIAL.
        getstructurefromattributes(
          EXPORTING
            node            = atextnode
          CHANGING
            structure       = textpoolrow
        ).
        APPEND textpoolrow TO textpooltable.
        atextnode ?= textiterator->get_next( ).
      ENDWHILE.
      IF textpooltable IS NOT INITIAL.
        lang = langnode->get_attribute( 'SPRAS' ).
*      if lang = sy-langu.                " del #255 - replaced by original language
        IF lang = lv_original_language.    " ins #255 - replaced former coding
*        logonLanguageExists = 'X'.
          _state = 'I'.
        ELSE.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
*       Stefan Schmöcker:  Looks like this does not trigger on logon- " ins #255
*                          but on class original language             " ins #255
          _state = 'A'.
        ENDIF.
        INSERT TEXTPOOL _objname
          FROM textpooltable
          LANGUAGE lang
          STATE    _state.
      ENDIF.
      langnode ?= langiterator->get_next( ).
    ENDWHILE.
  ENDMETHOD.
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA clskey TYPE seoclskey.

    clskey-clsname = objname.
    CALL FUNCTION 'SEO_CLASS_DELETE_W_DEPS'
      EXPORTING
        clskey       = clskey
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        not_deleted  = 3
        db_error     = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_found.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'interfaces not supported'.
        WHEN 3.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'class not deleted'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD findimplementingclass.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA methodkey TYPE seocmpkey.
    DATA methodproperties TYPE vseomethod.
    DATA classdescr TYPE REF TO cl_abap_classdescr.
    DATA superclass TYPE REF TO cl_abap_typedescr.
    DATA superclassname TYPE string.

    IF startclass IS INITIAL.
      methodkey-clsname = objname.
    ELSE.
      methodkey-clsname = startclass.
    ENDIF.
    methodkey-cmpname = methodname.

    CALL FUNCTION 'SEO_METHOD_GET'
      EXPORTING
        mtdkey       = methodkey
      IMPORTING
        method       = methodproperties
      EXCEPTIONS
        not_existing = 1.
    IF sy-subrc = 0.
      classname = methodproperties-clsname.
    ELSE.
      classdescr ?= cl_abap_classdescr=>describe_by_name(
      methodkey-clsname ).
      CALL METHOD classdescr->get_super_class_type
        RECEIVING
          p_descr_ref           = superclass
        EXCEPTIONS
          super_class_not_found = 1.
      superclassname = superclass->get_relative_name( ).
      classname = findimplementingclass( methodname = methodname
      startclass = superclassname ).
    ENDIF.
  ENDMETHOD.
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

    objecttype = 'CLAS'.  "Class

  ENDMETHOD.
  METHOD get_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA docnode       TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'CL'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( c_xml_key_class_documentation ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = c_xml_key_object value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( c_xml_key_language ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = c_xml_key_spras value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( c_xml_key_textline ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

    rc = rootnode->append_child( docnode ).

  ENDMETHOD.
  METHOD get_method_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA docnode        TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = method_key.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'CO'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( c_xml_key_method_documentation ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = c_xml_key_object value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( c_xml_key_language ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = c_xml_key_spras value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( c_xml_key_textline ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

    rc = rootnode->append_child( docnode ).

  ENDMETHOD.
  METHOD get_sections.
    DATA publicsection TYPE REF TO if_ixml_element.
    DATA protectedsection TYPE REF TO if_ixml_element.
    DATA privatesection TYPE REF TO if_ixml_element.
    DATA includename TYPE program.
    DATA reportstring TYPE string.

**/--------------------------------------------------------------------\
**|                                                                    |
*  includename = cl_oo_classname_service=>get_pubsec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  publicsection = xmldoc->create_element( 'publicSection' ).
*
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = publicsection->if_ixml_node~set_value( reportstring ).
*  CLEAR reportstring.
**|--------------------------------------------------------------------|
*  includename = cl_oo_classname_service=>get_prosec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  protectedsection = xmldoc->create_element( 'protectedSection' ).
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = protectedsection->if_ixml_node~set_value( reportstring ).
*  CLEAR reportstring.
**|--------------------------------------------------------------------|
*  includename = cl_oo_classname_service=>get_prisec_name( _classname ).
*  READ REPORT includename INTO reportlist.
*  privatesection = xmldoc->create_element( 'privateSection' ).
*  reportstring = buildsourcestring( sourcetable = reportlist ).
*  rc = privatesection->if_ixml_node~set_value( reportstring ).

*  rc = rootnode->append_child( publicsection ).
*  rc = rootnode->append_child( protectedsection ).
*  rc = rootnode->append_child( privatesection ).

  ENDMETHOD.
  METHOD get_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA atext TYPE REF TO if_ixml_element.
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA languagelist TYPE instlang.
    DATA alanguage TYPE spras.
    DATA _objname TYPE seoclsname.
    DATA rc TYPE i.
    DATA stemp TYPE string.
    DATA languagenode TYPE REF TO if_ixml_element.
    DATA textnode      TYPE REF TO if_ixml_element.
    DATA classpoolname TYPE program.
    DATA firstloop TYPE flag.

    _objname = objname.

    classpoolname = cl_oo_classname_service=>get_classpool_name( _objname ).

    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
      CHANGING
        installed_languages = languagelist.

    firstloop = abap_true.

    LOOP AT languagelist INTO alanguage.
      READ TEXTPOOL classpoolname INTO textpooltable LANGUAGE alanguage.
      IF sy-subrc = 0.
        IF firstloop = abap_true.
          textnode = xmldoc->create_element( c_xml_key_textpool ).
          firstloop = abap_false.
        ENDIF.
        languagenode = xmldoc->create_element( c_xml_key_language ).
        stemp = alanguage.
        rc = languagenode->set_attribute( name = c_xml_key_spras value = stemp ).
        LOOP AT textpooltable INTO textpoolrow.
          atext = xmldoc->create_element( c_xml_key_textelement ).
          setattributesfromstructure( node = atext structure =
          textpoolrow ).
          rc = languagenode->append_child( atext ).
        ENDLOOP.
        rc = textnode->append_child( languagenode ).
      ENDIF.
    ENDLOOP.

    rc = rootnode->append_child( textnode ).

  ENDMETHOD.
  METHOD get_version_info.

    rs_version_info-zsaplink_plugin_major_version = 0.  " We will still import anything written by older version, versioning doesn't change in- or ouptut
    rs_version_info-zsaplink_plugin_minor_version = 1.  " Since we add versioning info this has to increase
    rs_version_info-zsaplink_plugin_build_version = 0.  " minor version increased --> reset to 0

    rs_version_info-zsaplink_plugin_info1         = 'ZSAPLINK_CLASS is part of the main ZSAPLINK project --> This plugin found there instead of ZSAPLINK_PLUGINS projects'.
    rs_version_info-zsaplink_plugin_info2         = 'SAPLINK homepage: https://www.assembla.com/spaces/saplink/wiki'.
    rs_version_info-zsaplink_plugin_info3         = 'Download from https://www.assembla.com/code/saplink/subversion/nodes'.
    rs_version_info-zsaplink_plugin_info4         = 'and navigate to:  trunk -> core -> ZSAPLINK -> CLAS -> ZSAPLINK_CLASS.slnk'.
    rs_version_info-zsaplink_plugin_info5         = ''.

  ENDMETHOD.
ENDCLASS.
CLASS zsaplink_program IMPLEMENTATION.
  METHOD checkexists.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

    SELECT SINGLE name FROM trdir INTO objname WHERE name = objname.
    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.

  ENDMETHOD.
  METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA textpoolnode TYPE REF TO if_ixml_element.
    DATA docnode TYPE REF TO if_ixml_element.
    DATA dynpronode TYPE REF TO if_ixml_element.
    DATA statusnode TYPE REF TO if_ixml_element.
    DATA rc TYPE sysubrc.
    DATA progattribs TYPE trdir.
    DATA progsource TYPE rswsourcet.
    DATA sourcestring TYPE string.
    DATA _objtype TYPE string.

    _objtype = getobjecttype( ).
    rootnode = xmldoc->create_element( _objtype ).
    sourcenode = xmldoc->create_element( 'source' ).
    SELECT SINGLE * FROM trdir INTO progattribs WHERE name = objname.
    IF sy-subrc = 0.
      setattributesfromstructure( node = rootnode structure =  progattribs ).
      progsource = me->get_source( ).
      sourcestring = buildsourcestring( sourcetable = progsource ).
      rc = sourcenode->if_ixml_node~set_value( sourcestring ).
      textpoolnode = get_textpool( ).
      rc = rootnode->append_child( textpoolnode ).
      docnode = get_documentation( ).
      rc = rootnode->append_child( docnode ).
      dynpronode = get_dynpro( ).
      rc = rootnode->append_child( dynpronode ).
      statusnode = get_pfstatus( ).
      rc = rootnode->append_child( statusnode ).
      rc = rootnode->append_child( sourcenode ).
      rc = xmldoc->append_child( rootnode ).
      ixmldocument = xmldoc.
    ELSE.
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found
          object = objname.
    ENDIF.
  ENDMETHOD.
  METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA progattribs TYPE trdir.
    DATA sourcenode TYPE REF TO if_ixml_element.
    DATA textnode TYPE REF TO if_ixml_element.
    DATA docnode TYPE REF TO if_ixml_element.
    DATA dynpnode TYPE REF TO if_ixml_element.
    DATA statnode TYPE REF TO if_ixml_element.
    DATA source TYPE string.
    DATA sourcetable TYPE table_of_strings.
    DATA _objname(30) TYPE c.
    DATA aobjname TYPE trobj_name.
    DATA _objtype TYPE string.
    DATA checkexists TYPE flag.

*if sy-uname <> 'USDWM01'.
*    _objType = getObjectType( ).
*    xmlDoc = ixmlDocument.
*    rootNode = xmlDoc->find_from_name( _objType ).
*    call method GETSTRUCTUREFROMATTRIBUTES
*          exporting
*            node = rootNode
*          changing
*            structure = progAttribs.
*    objName = progAttribs-NAME.
*
**   check existing
*    select single name from trdir into objName where NAME = objName.
*    if sy-subrc = 0.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>existing.
*    endif.
*
*    sourceNode = rootNode->find_from_name( 'source' ).
*    source = sourceNode->get_value( ).
*    sourceTable = BUILDTABLEFROMSTRING( source ).
*    insert report progAttribs-NAME from sourceTable.
*
*    commit work.
*
*    call function 'RS_INSERT_INTO_WORKING_AREA'
*      EXPORTING
*        object            = 'REPS'
*        obj_name          = aobjName
*      EXCEPTIONS
*        wrong_object_name = 1.
*    if sy-subrc <> 0.
*
*    endif.
*
*else.

    _objtype = getobjecttype( ).
    xmldoc = ixmldocument.
    rootnode = xmldoc->find_from_name( _objtype ).
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rootnode
      CHANGING
        structure = progattribs.
    objname = progattribs-name.

*  check if object exists
*  select single name from trdir into objName where NAME = objName.
*  if sy-subrc = 0 and overwrite <> 'X'.
*    raise exception type zcx_saplink
*      exporting textid = zcx_saplink=>existing.
*  endif.

    checkexists = checkexists( ).
    IF checkexists IS NOT INITIAL.
      IF overwrite IS INITIAL.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>existing.
      ELSE.
*     delete object for new install
        deleteobject( ).
      ENDIF.
    ENDIF.


    enqueue_abap( ).
    transport_copy( author = progattribs-cnam devclass = devclass ).
    sourcenode = rootnode->find_from_name( 'source' ).
    source = sourcenode->get_value( ).
    sourcetable = buildtablefromstring( source ).
    create_source( source = sourcetable attribs = progattribs ).
    textnode = rootnode->find_from_name( 'textPool' ).
    create_textpool( textnode ).
    docnode = rootnode->find_from_name( 'programDocumentation' ).
    create_documentation( docnode ).
    dynpnode = rootnode->find_from_name( 'dynpros' ).
    create_dynpro( dynpnode ).
    statnode = rootnode->find_from_name( 'pfstatus' ).
    create_pfstatus( statnode ).

    dequeue_abap( ).
    update_wb_tree( ).
*endif.

* successful install
    name = objname.

  ENDMETHOD.
  METHOD createstringfromobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA progsource TYPE rswsourcet.
    progsource = me->get_source( ).
    string = buildsourcestring( sourcetable = progsource ).
  ENDMETHOD.
  METHOD create_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA txtline_node     TYPE REF TO if_ixml_element.
    DATA txtline_filter   TYPE REF TO if_ixml_node_filter.
    DATA txtline_iterator TYPE REF TO if_ixml_node_iterator.

    DATA lang_node     TYPE REF TO if_ixml_element.
    DATA lang_filter   TYPE REF TO if_ixml_node_filter.
    DATA lang_iterator TYPE REF TO if_ixml_node_iterator.

    DATA obj_name TYPE dokhl-object.
    DATA prog_name TYPE string.
    DATA language  TYPE string.
    DATA obj_langu TYPE dokhl-langu.
    DATA lv_str TYPE string.
    DATA rc TYPE sy-subrc.

    DATA lt_lines  TYPE TABLE OF tline.
    FIELD-SYMBOLS: <ls_lines> LIKE LINE OF lt_lines.

    IF docnode IS NOT BOUND.
      RETURN.
    ENDIF.

    prog_name = docnode->get_attribute( name = 'OBJECT' ).
    obj_name = prog_name.

* If no prog name, then there was no program documenation, just return.
    IF prog_name IS INITIAL.
      RETURN.
    ENDIF.

* Get languages from XML
    FREE: lang_filter, lang_iterator, lang_node.
    lang_filter = docnode->create_filter_name( `language` ).
    lang_iterator = docnode->create_iterator_filtered( lang_filter ).
    lang_node ?= lang_iterator->get_next( ).
    WHILE lang_node IS NOT INITIAL.

      REFRESH lt_lines.
      language = lang_node->get_attribute( name = 'SPRAS' ).
      obj_langu = language.

* Get TextLines from XML
      FREE: txtline_filter, txtline_iterator, txtline_node.
      txtline_filter = lang_node->create_filter_name( `textLine` ).
      txtline_iterator = lang_node->create_iterator_filtered( txtline_filter ).
      txtline_node ?= txtline_iterator->get_next( ).
      WHILE txtline_node IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_lines>.
        me->getstructurefromattributes(
                EXPORTING   node      = txtline_node
                CHANGING    structure = <ls_lines> ).
        txtline_node ?= txtline_iterator->get_next( ).
      ENDWHILE.

* Delete any documentation that may currently exist.
      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = 'RE'   "<-- Report/program documentation
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

* Now update with new documentation text
      CALL FUNCTION 'DOCU_UPD'
        EXPORTING
          id       = 'RE'
          langu    = obj_langu
          object   = obj_name
          typ      = 'E'
        TABLES
          line     = lt_lines
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = `Program Documentation object import failed`.
      ENDIF.

      lang_node ?= lang_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD create_dynpro.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    TYPES: BEGIN OF tdyn_head_temp.
             INCLUDE TYPE d020s.
    TYPES:   dtext TYPE d020t-dtxt.
    TYPES: END OF tdyn_head_temp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA: xdyn_text_string TYPE string.
    DATA: xdyn_text        TYPE d020t-dtxt .
    DATA: xdyn_head_temp   TYPE tdyn_head_temp.

    DATA _objname TYPE trobj_name.

    DATA dynpros_node       TYPE REF TO if_ixml_element.
    DATA dynpros_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynpros_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynpro_node        TYPE REF TO if_ixml_element.
    DATA dynpro_filter      TYPE REF TO if_ixml_node_filter.
    DATA dynpro_iterator    TYPE REF TO if_ixml_node_iterator.

    DATA dynfldl_node       TYPE REF TO if_ixml_element.
    DATA dynfldl_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynfldl_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynmcod_node       TYPE REF TO if_ixml_element.
    DATA dynmcod_filter     TYPE REF TO if_ixml_node_filter.
    DATA dynmcod_iterator   TYPE REF TO if_ixml_node_iterator.

    DATA dynflow_node       TYPE REF TO if_ixml_element.

    DATA xdynpro_flow_source TYPE string.
    DATA idynpro_flow_source TYPE table_of_strings.

    _objname = objname.

    dynpros_node =  dynp_node.
    CHECK dynpros_node IS NOT INITIAL.

    FREE: dynpro_filter, dynpro_iterator, dynpro_node.
    dynpro_filter = dynpros_node->create_filter_name( 'dynpro' ).
    dynpro_iterator =
          dynpros_node->create_iterator_filtered( dynpro_filter ).
    dynpro_node ?= dynpro_iterator->get_next( ).

    WHILE dynpro_node IS NOT INITIAL.

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

* Get the header data for the screen.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = dynpro_node
        CHANGING
          structure = xdyn_head_temp.

      xdyn_head    = xdyn_head_temp.
      xdyn_text    = xdyn_head_temp-dtext.

* Retrieve field list
      FREE: dynfldl_filter, dynfldl_iterator, dynfldl_node.
      dynfldl_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynfldl_iterator =
          dynpro_node->create_iterator_filtered( dynfldl_filter ).
      dynfldl_node ?= dynfldl_iterator->get_next( ).
      WHILE dynfldl_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynfldl_node
          CHANGING
            structure = xdyn_fldl.
        APPEND xdyn_fldl TO idyn_fldl.
        dynfldl_node ?= dynfldl_iterator->get_next( ).
      ENDWHILE.

* Retrieve matchcode data.
      FREE: dynmcod_filter, dynmcod_iterator, dynmcod_node.
      dynmcod_filter = dynpro_node->create_filter_name( 'dynprofield' ).
      dynmcod_iterator =
           dynpro_node->create_iterator_filtered( dynmcod_filter ).
      dynmcod_node ?= dynmcod_iterator->get_next( ).
      WHILE dynmcod_node IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = dynmcod_node
          CHANGING
            structure = xdyn_mcod.
        APPEND xdyn_mcod TO idyn_mcod.
        dynmcod_node ?= dynmcod_iterator->get_next( ).
      ENDWHILE.

* retieve flow logic source.
      CLEAR xdynpro_flow_source.  REFRESH idynpro_flow_source.
      CLEAR xdyn_flow.            REFRESH idyn_flow.
      FREE dynflow_node.
      dynflow_node = dynpro_node->find_from_name( 'dynproflowsource' ).
      xdynpro_flow_source  = dynflow_node->get_value( ).
      idynpro_flow_source = buildtablefromstring( xdynpro_flow_source ).
      LOOP AT idynpro_flow_source INTO xdyn_flow.
        APPEND xdyn_flow  TO idyn_flow.
      ENDLOOP.

* Build dynpro from data
      CALL FUNCTION 'RPY_DYNPRO_INSERT_NATIVE'
        EXPORTING
*         suppress_corr_checks           = ' '
*         CORRNUM            = ' '
          header             = xdyn_head
          dynprotext         = xdyn_text
*         SUPPRESS_EXIST_CHECKS          = ' '
*         USE_CORRNUM_IMMEDIATEDLY       = ' '
*         SUPPRESS_COMMIT_WORK           = ' '
        TABLES
          fieldlist          = idyn_fldl
          flowlogic          = idyn_flow
          params             = idyn_mcod
        EXCEPTIONS
          cancelled          = 1
          already_exists     = 2
          program_not_exists = 3
          not_executed       = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
      ENDIF.

      dynpro_node ?= dynpro_iterator->get_next( ).

    ENDWHILE.

  ENDMETHOD.
  METHOD create_pfstatus.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA xtrkey TYPE trkey.
    DATA xadm   TYPE rsmpe_adm.
    DATA _program TYPE  trdir-name.
    DATA _objname TYPE trobj_name.

    DATA stat_node  TYPE REF TO if_ixml_element.
    DATA node       TYPE REF TO if_ixml_element.
    DATA filter     TYPE REF TO if_ixml_node_filter.
    DATA iterator   TYPE REF TO if_ixml_node_iterator.

    DATA: ls_iact TYPE rsmpe_act,
          ls_ipfk TYPE rsmpe_pfk,
          ls_imen TYPE rsmpe_men.

    _objname = objname.

    stat_node =  pfstat_node.
    CHECK stat_node IS NOT INITIAL.

* read pfstatus_sta node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_sta' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xsta.
      APPEND xsta TO ista.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_fun node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_fun' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xfun.
      APPEND xfun TO ifun.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_men node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_men' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmen.
      APPEND xmen TO imen.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_mtx node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_mtx' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xmtx.
      APPEND xmtx TO imtx.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_act node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_act' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xact.
      APPEND xact TO iact.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_but node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_but' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbut.
      APPEND xbut TO ibut.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_pfk node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_pfk' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xpfk.
      APPEND xpfk TO ipfk.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_set node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_set' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xset.
      APPEND xset TO iset.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_doc node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_doc' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xdoc.
      APPEND xdoc TO idoc.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_tit node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_tit' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xtit.
      APPEND xtit TO itit.
      node ?= iterator->get_next( ).
    ENDWHILE.

* read pfstatus_biv node
    FREE: filter, iterator, node.
    filter = stat_node->create_filter_name( 'pfstatus_biv' ).
    iterator = stat_node->create_iterator_filtered( filter ).
    node ?= iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node
        CHANGING
          structure = xbiv.
      APPEND xbiv TO ibiv.
      node ?= iterator->get_next( ).
    ENDWHILE.

* Update the gui status
    _program = _objname.

    xtrkey-obj_type = 'PROG'.
    xtrkey-obj_name = _program.
    xtrkey-sub_type = 'CUAD'.
    xtrkey-sub_name = _program.

    LOOP AT iact INTO ls_iact.
      xadm-actcode = ls_iact-code.
    ENDLOOP.
    LOOP AT ipfk INTO ls_ipfk.
      xadm-pfkcode = ls_ipfk-code.
    ENDLOOP.
    LOOP AT imen INTO ls_imen.
      xadm-mencode = ls_imen-code.
    ENDLOOP.

    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = _program
        language  = sy-langu
        tr_key    = xtrkey
        adm       = xadm
        state     = 'I'
      TABLES
        sta       = ista
        fun       = ifun
        men       = imen
        mtx       = imtx
        act       = iact
        but       = ibut
        pfk       = ipfk
        set       = iset
        doc       = idoc
        tit       = itit
        biv       = ibiv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.

  ENDMETHOD.
  METHOD create_source.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

    DATA _objname TYPE trobj_name.
    DATA progline TYPE progdir.
    DATA titleinfo TYPE trdirti.
    DATA reportline TYPE string.
    DATA minireport TYPE table_of_strings.

    _objname = objname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = 'REPS'
        obj_name          = _objname
      EXCEPTIONS
        wrong_object_name = 1.
    INSERT REPORT _objname FROM source STATE 'I'
      PROGRAM TYPE attribs-subc.  "added to handle includes, etc.
    MOVE 'I' TO progline-state.
    MOVE-CORRESPONDING attribs TO progline.
    progline-idate = sy-datum.
    progline-itime = sy-uzeit.
    progline-cdat  = sy-datum.
    progline-udat  = sy-datum.
    progline-sdate = sy-datum.
    MODIFY progdir FROM progline.
*  Are you kidding me?!?  No idea why you need to do this!!
    CONCATENATE 'REPORT' _objname '.' INTO reportline SEPARATED BY space.
    APPEND reportline TO minireport.
    INSERT REPORT _objname FROM minireport STATE 'A'
      PROGRAM TYPE attribs-subc. "added to handle includes, etc.
    MOVE 'A' TO progline-state.
    MODIFY progdir FROM progline.

  ENDMETHOD.
  METHOD create_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA langiterator TYPE REF TO if_ixml_node_iterator.
    DATA filter TYPE REF TO if_ixml_node_filter.
    DATA textfilter TYPE REF TO if_ixml_node_filter.
    DATA textiterator TYPE REF TO if_ixml_node_iterator.
    DATA langnode TYPE REF TO if_ixml_element.
    DATA atextnode TYPE REF TO if_ixml_element.
    DATA _objname TYPE trobj_name.
    DATA lang TYPE spras.
    DATA langnodeexists TYPE flag.
    DATA logonlanguageexists TYPE flag.
    DATA _state(1) TYPE c.

    _objname = objname.
    CHECK textpoolnode IS NOT INITIAL.

    filter = textpoolnode->create_filter_name( 'language' ).
    langiterator = textpoolnode->create_iterator_filtered( filter ).
    langnode ?= langiterator->get_next( ).

    WHILE langnode IS NOT INITIAL.
      langnodeexists = 'X'.
      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          object   = 'REPT'
          obj_name = _objname
        EXCEPTIONS
          OTHERS   = 0.

      REFRESH textpooltable.
      textiterator = langnode->create_iterator( ).
      atextnode ?= textiterator->get_next( ).
*For some reason the 1st one is blank... not sure why.
      atextnode ?= textiterator->get_next( ).
      WHILE atextnode IS NOT INITIAL.
        CALL METHOD getstructurefromattributes
          EXPORTING
            node      = atextnode
          CHANGING
            structure = textpoolrow.
        APPEND textpoolrow TO textpooltable.
        atextnode ?= textiterator->get_next( ).
      ENDWHILE.
      IF textpooltable IS NOT INITIAL.
        lang = langnode->get_attribute( 'SPRAS' ).
        IF lang = sy-langu.
          logonlanguageexists = 'X'.
          _state = 'I'.
        ELSE.
*       seems that if a textpool is inserted as inactive for language
*       other than the logon language, it is lost upon activation
*       not sure inserting as active is best solution,but seems to work
          _state = 'A'.
        ENDIF.
        INSERT TEXTPOOL _objname
          FROM textpooltable
          LANGUAGE lang
          STATE    _state.
      ENDIF.
      langnode ?= langiterator->get_next( ).
    ENDWHILE.
  ENDMETHOD.
  METHOD deleteobject.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA program TYPE sy-repid.

    program = objname.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
*       CORRNUMBER     =
        program        = program
*       SUPPRESS_CHECKS                  = ' '
*       SUPPRESS_COMMIT                  = ' '
        suppress_popup = 'X'
*       MASS_DELETE_CALL                 = ' '
*       WITH_CUA       = 'X'
*       WITH_DOCUMENTATION               = 'X'
*       WITH_DYNPRO    = 'X'
*       WITH_INCLUDES  = ' '
*       WITH_TEXTPOOL  = 'X'
*       WITH_VARIANTS  = 'X'
*       TADIR_DEVCLASS =
*       SKIP_PROGRESS_IND                = ' '
*       FORCE_DELETE_USED_INCLUDES       = ' '
* IMPORTING
*       CORRNUMBER     =
*       PROGRAM        =
* EXCEPTIONS
*       ENQUEUE_LOCK   = 1
*       OBJECT_NOT_FOUND                 = 2
*       PERMISSION_FAILURE               = 3
*       REJECT_DELETION                  = 4
*       OTHERS         = 5
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.
  METHOD dequeue_abap.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        global_lock              = 'X'
        mode                     = 'FREE'
        object                   = objname
        object_class             = 'ABAP'
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 3
        enqueue_system_failure   = 4
        locked_by_author         = 5
        illegal_parameter_values = 6
        no_modify_permission     = 7
        no_show_permission       = 8
        permission_failure       = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 7 OR 8 OR 9.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 5.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD enqueue_abap.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
*       authority_check          = authority_check
        global_lock              = 'X'
        mode                     = 'INSERT'
*       master_language          = trdir-rload
        object                   = objname
        object_class             = 'ABAP'
*       importing
*       transport_key            = trkey_global
*       new_master_language      = trdir-rload
*       devclass                 = devclass_local
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 3
        enqueue_system_failure   = 4
        locked_by_author         = 5
        illegal_parameter_values = 6
        no_modify_permission     = 7
        no_show_permission       = 8
        permission_failure       = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 7 OR 8 OR 9.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN 5.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = 'object locked'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    objecttype = 'PROG'. "ABAP Program
  ENDMETHOD.
  METHOD get_documentation.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA languagenode   TYPE REF TO if_ixml_element.
    DATA txtlines_node TYPE REF TO if_ixml_element.
    DATA rc            TYPE sysubrc.
    DATA _objtype      TYPE string.

    TYPES: BEGIN OF t_dokhl,
             id         TYPE dokhl-id,
             object     TYPE dokhl-object,
             langu      TYPE dokhl-langu,
             typ        TYPE dokhl-typ,
             dokversion TYPE dokhl-dokversion,
           END OF t_dokhl.

    DATA lt_dokhl TYPE TABLE OF t_dokhl.
    DATA ls_dokhl LIKE LINE OF lt_dokhl.

    DATA lt_lines TYPE TABLE OF tline.
    DATA ls_lines LIKE LINE OF lt_lines.

    DATA lv_str TYPE string.
    DATA _objname TYPE e071-obj_name.

    _objname = objname.

* Check against database
    SELECT  id object langu typ dokversion
          INTO CORRESPONDING FIELDS OF TABLE lt_dokhl
             FROM dokhl
               WHERE id = 'RE'
                  AND object = _objname.

* Use only most recent version.
    SORT lt_dokhl BY id object langu typ ASCENDING dokversion DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_dokhl COMPARING id object typ langu.

* Make sure there is at least one record here.
    CLEAR ls_dokhl.
    READ TABLE lt_dokhl INTO ls_dokhl INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    docnode = xmldoc->create_element( 'programDocumentation' ).

* Set docNode object attribute
    lv_str = ls_dokhl-object.
    rc = docnode->set_attribute( name = 'OBJECT' value = lv_str ).

    LOOP AT lt_dokhl INTO ls_dokhl.

* Create language node, and set attribute
      languagenode = xmldoc->create_element( 'language' ).
      lv_str = ls_dokhl-langu.
      rc = languagenode->set_attribute( name = 'SPRAS' value = lv_str ).

* Read the documentation text
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = ls_dokhl-id
          langu   = ls_dokhl-langu
          object  = ls_dokhl-object
          typ     = ls_dokhl-typ
          version = ls_dokhl-dokversion
        TABLES
          line    = lt_lines.

* Write records to XML node
      LOOP AT lt_lines INTO ls_lines.
        txtlines_node = xmldoc->create_element( `textLine` ).
        me->setattributesfromstructure( node = txtlines_node structure = ls_lines ).
        rc = languagenode->append_child( txtlines_node ).
      ENDLOOP.
      rc = docnode->append_child( languagenode ) .
    ENDLOOP.

  ENDMETHOD.
  METHOD get_dynpro.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    TYPES: BEGIN OF tdynp,
             prog TYPE d020s-prog,
             dnum TYPE d020s-dnum,
           END OF tdynp.

    DATA: idyn_fldl TYPE TABLE OF d021s,
          idyn_flow TYPE TABLE OF d022s,
          idyn_mcod TYPE TABLE OF d023s.

    DATA: xdyn_head TYPE  d020s,
          xdyn_fldl TYPE  d021s,
          xdyn_flow TYPE  d022s,
          xdyn_mcod TYPE  d023s.

    DATA idynp TYPE TABLE OF tdynp.
    DATA xdynp TYPE tdynp.

    DATA xdyn_text TYPE d020t-dtxt.
    DATA xdyn_text_string TYPE string.

    DATA _objname TYPE trobj_name.
    DATA rc TYPE sy-subrc .

    DATA iflowsource TYPE rswsourcet.
    DATA xflowsource LIKE LINE OF iflowsource.
    DATA flowsourcestring TYPE string.

    DATA dynnr_node TYPE REF TO if_ixml_element.
    DATA dynpromatchnode TYPE REF TO if_ixml_element.
    DATA dynprofieldsnode TYPE REF TO if_ixml_element.
    DATA dynproflownode TYPE REF TO if_ixml_element.

    _objname = objname.

* Get all dynpros for program object
    CLEAR xdynp.  REFRESH idynp.
    SELECT prog dnum INTO TABLE idynp
                  FROM d020s
                     WHERE prog = _objname
                       AND type <> 'S'    " No Selection Screens
                       AND type <> 'J'.   " No selection subscreens
    CHECK sy-subrc  = 0 .

    dynp_node = xmldoc->create_element( 'dynpros' ).

    LOOP AT idynp INTO xdynp.

* Retrieve dynpro imformation
      dynnr_node =  xmldoc->create_element( 'dynpro' ).

      CLEAR:    xdyn_head, xdyn_fldl, xdyn_flow, xdyn_mcod.
      REFRESH:  idyn_fldl, idyn_flow, idyn_mcod.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname         = xdynp-prog
          dynnr            = xdynp-dnum
*         SUPPRESS_EXIST_CHECKS       = ' '
*         SUPPRESS_CORR_CHECKS        = ' '
        IMPORTING
          header           = xdyn_head
          dynprotext       = xdyn_text
        TABLES
          fieldlist        = idyn_fldl
          flowlogic        = idyn_flow
          params           = idyn_mcod
*         FIELDTEXTS       =
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.

      CHECK sy-subrc = 0.

* Add heading information for screen.
      setattributesfromstructure(
                       node = dynnr_node structure =  xdyn_head  ).
* Add the dynpro text also.
      xdyn_text_string =  xdyn_text.
      rc = dynnr_node->set_attribute(
                 name = 'DTEXT'  value = xdyn_text_string ).
      rc = dynp_node->append_child( dynnr_node ).

* Add fields information for screen.
      IF NOT idyn_fldl[] IS INITIAL.
        LOOP AT idyn_fldl INTO xdyn_fldl.
          dynprofieldsnode = xmldoc->create_element( 'dynprofield' ).
          setattributesfromstructure(
                   node = dynprofieldsnode structure =  xdyn_fldl ).
          rc = dynnr_node->append_child( dynprofieldsnode ).
        ENDLOOP.
      ENDIF.

* Add flow logic of screen
      IF NOT idyn_flow[] IS INITIAL.
        CLEAR xflowsource. REFRESH  iflowsource.
        LOOP AT idyn_flow INTO xdyn_flow.
          xflowsource  = xdyn_flow.
          APPEND xflowsource TO iflowsource.
        ENDLOOP.

        dynproflownode = xmldoc->create_element( 'dynproflowsource' ).
        flowsourcestring = buildsourcestring( sourcetable = iflowsource ).
        rc = dynproflownode->if_ixml_node~set_value( flowsourcestring ).
        rc = dynnr_node->append_child( dynproflownode  ).
      ENDIF.

* Add matchcode information for screen.
      IF NOT idyn_mcod[] IS INITIAL.
        LOOP AT idyn_mcod INTO xdyn_mcod.
          CHECK NOT xdyn_mcod-type IS INITIAL
            AND NOT xdyn_mcod-content IS INITIAL.
          dynpromatchnode = xmldoc->create_element( 'dynpromatchcode' ).
          setattributesfromstructure(
                   node = dynpromatchnode structure =  xdyn_mcod ).
          rc = dynnr_node->append_child( dynpromatchnode ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_pfstatus.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.

    DATA: xsta TYPE rsmpe_stat,
          xfun TYPE rsmpe_funt,
          xmen TYPE rsmpe_men,
          xmtx TYPE rsmpe_mnlt,
          xact TYPE rsmpe_act,
          xbut TYPE rsmpe_but,
          xpfk TYPE rsmpe_pfk,
          xset TYPE rsmpe_staf,
          xdoc TYPE rsmpe_atrt,
          xtit TYPE rsmpe_titt,
          xbiv TYPE rsmpe_buts.

    DATA sta_node TYPE REF TO if_ixml_element.
    DATA fun_node TYPE REF TO if_ixml_element.
    DATA men_node TYPE REF TO if_ixml_element.
    DATA mtx_node TYPE REF TO if_ixml_element.
    DATA act_node TYPE REF TO if_ixml_element.
    DATA but_node TYPE REF TO if_ixml_element.
    DATA pfk_node TYPE REF TO if_ixml_element.
    DATA set_node TYPE REF TO if_ixml_element.
    DATA doc_node TYPE REF TO if_ixml_element.
    DATA tit_node TYPE REF TO if_ixml_element.
    DATA biv_node TYPE REF TO if_ixml_element.

    DATA _objname TYPE trobj_name.
    DATA _program TYPE  trdir-name.
    DATA rc TYPE sy-subrc.

    _objname = objname.
    _program = objname.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = _program
        language        = sy-langu
      TABLES
        sta             = ista
        fun             = ifun
        men             = imen
        mtx             = imtx
        act             = iact
        but             = ibut
        pfk             = ipfk
        set             = iset
        doc             = idoc
        tit             = itit
        biv             = ibiv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.

    CHECK sy-subrc = 0.

* if there is a gui status or gui title present, then
* create pfstatus node.
    IF ista[] IS NOT INITIAL
       OR itit[] IS NOT INITIAL.
      pfstat_node = xmldoc->create_element( 'pfstatus' ).
    ENDIF.


* if ista is filled, assume there are one or more
* gui statuses
    IF ista[] IS NOT INITIAL.

      LOOP AT ista INTO xsta.
        sta_node = xmldoc->create_element( 'pfstatus_sta' ).
        setattributesfromstructure(
                 node = sta_node
                 structure =  xsta ).
        rc = pfstat_node->append_child( sta_node ).
      ENDLOOP.

      LOOP AT ifun INTO xfun.
        fun_node = xmldoc->create_element( 'pfstatus_fun' ).
        setattributesfromstructure(
                 node = fun_node
                 structure =  xfun ).
        rc = pfstat_node->append_child( fun_node ).
      ENDLOOP.

      LOOP AT imen INTO xmen.
        men_node = xmldoc->create_element( 'pfstatus_men' ).
        setattributesfromstructure(
                 node = men_node
                 structure =  xmen ).
        rc = pfstat_node->append_child( men_node ).
      ENDLOOP.

      LOOP AT imtx INTO xmtx.
        mtx_node = xmldoc->create_element( 'pfstatus_mtx' ).
        setattributesfromstructure(
                 node = mtx_node
                 structure =  xmtx ).
        rc = pfstat_node->append_child( mtx_node ).
      ENDLOOP.

      LOOP AT iact INTO xact.
        act_node = xmldoc->create_element( 'pfstatus_act' ).
        setattributesfromstructure(
                 node = act_node
                 structure =  xact ).
        rc = pfstat_node->append_child( act_node ).
      ENDLOOP.

      LOOP AT ibut INTO xbut.
        but_node = xmldoc->create_element( 'pfstatus_but' ).
        setattributesfromstructure(
                 node = but_node
                 structure =  xbut ).
        rc = pfstat_node->append_child( but_node ).
      ENDLOOP.

      LOOP AT ipfk INTO xpfk.
        pfk_node = xmldoc->create_element( 'pfstatus_pfk' ).
        setattributesfromstructure(
                 node = pfk_node
                 structure =  xpfk ).
        rc = pfstat_node->append_child( pfk_node ).
      ENDLOOP.

      LOOP AT iset INTO xset.
        set_node = xmldoc->create_element( 'pfstatus_set' ).
        setattributesfromstructure(
                 node = set_node
                 structure =  xset ).
        rc = pfstat_node->append_child( set_node ).
      ENDLOOP.

      LOOP AT idoc INTO xdoc.
        doc_node = xmldoc->create_element( 'pfstatus_doc' ).
        setattributesfromstructure(
                 node = doc_node
                 structure =  xdoc ).
        rc = pfstat_node->append_child( doc_node ).
      ENDLOOP.


      LOOP AT ibiv INTO xbiv.
        biv_node = xmldoc->create_element( 'pfstatus_biv' ).
        setattributesfromstructure(
                 node = biv_node
                 structure =  xbiv ).
        rc = pfstat_node->append_child( biv_node ).
      ENDLOOP.

    ENDIF.


* It itit is filled, assume one or more titles
    IF itit[] IS NOT INITIAL.

      LOOP AT itit INTO xtit.
        tit_node = xmldoc->create_element( 'pfstatus_tit' ).
        setattributesfromstructure(
                 node = tit_node
                 structure =  xtit ).
        rc = pfstat_node->append_child( tit_node ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD get_source.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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

    DATA _objname(30) TYPE c.

    _objname = me->objname.
    READ REPORT _objname INTO progsource.

  ENDMETHOD.
  METHOD get_textpool.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    DATA atext TYPE REF TO if_ixml_element.
    DATA textpooltable TYPE STANDARD TABLE OF textpool.
    DATA textpoolrow TYPE textpool.
    DATA languagelist TYPE instlang.
    DATA alanguage TYPE spras.
    DATA _objname(30) TYPE c.
    DATA rc TYPE i.
    DATA stemp TYPE string.
    DATA languagenode TYPE REF TO if_ixml_element.
    DATA firstloop TYPE flag.

    _objname = objname.


    CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
      CHANGING
        installed_languages = languagelist.

    firstloop = abap_true.

    LOOP AT languagelist INTO alanguage.
      READ TEXTPOOL _objname INTO textpooltable LANGUAGE alanguage.
      IF sy-subrc = 0.
        IF firstloop = abap_true.
          textnode = xmldoc->create_element( 'textPool' ).
          firstloop = abap_false.
        ENDIF.
        languagenode = xmldoc->create_element( 'language' ).
        stemp = alanguage.
        rc = languagenode->set_attribute( name = 'SPRAS' value = stemp ).
        LOOP AT textpooltable INTO textpoolrow.
          atext = xmldoc->create_element( 'textElement' ).
          setattributesfromstructure( node = atext structure =
          textpoolrow ).
          rc = languagenode->append_child( atext ).
        ENDLOOP.
        rc = textnode->append_child( languagenode ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD transport_copy.
*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
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
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author              = author
        global_lock         = 'X'
        object              = objname
        object_class        = 'ABAP'
        devclass            = devclass
*       KORRNUM             = CORRNUMBER_LOCAL
        master_language     = sy-langu
*       PROGRAM             = PROGRAM_LOCAL
        mode                = 'INSERT'
*       IMPORTING
*       AUTHOR              = UNAME
*       KORRNUM             = CORRNUMBER_LOCAL
*       DEVCLASS            = DEVCLASS_LOCAL
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 2.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>not_authorized.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>system_error.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD update_wb_tree.

    DATA: BEGIN OF pname,
            root(3)     VALUE 'PG_',
            program(27),
          END OF pname.

    DATA: trdir TYPE trdir.

    pname-program = me->objname.

    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = pname.

    trdir-name    = me->objname.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = trdir-name
        program   = trdir-name
        operation = 'INSERT'
        type      = 'CP'.

  ENDMETHOD.
ENDCLASS.

TYPE-POOLS: seor, abap, icon.
DATA retfiletable TYPE filetable.
DATA retrc TYPE sysubrc.
DATA retuseraction TYPE i.

DATA tempxmlstring TYPE string.
DATA ixmlnugget TYPE REF TO if_ixml_document.

DATA pluginexists TYPE flag.
DATA objectexists TYPE flag.
DATA flag TYPE flag.
DATA statusmsg TYPE string.
DATA  y2all TYPE flag.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(20) filecom FOR FIELD nuggfil.
  PARAMETERS nuggfil(300) TYPE c MODIF ID did OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(20) checkcom FOR FIELD nuggfil.
  PARAMETERS overwrt TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.



START-OF-SELECTION. "farid
  CLEAR tempxmlstring.
  PERFORM uploadxmlfromlm USING nuggfil tempxmlstring.
  PERFORM convertstringtoixmldoc USING tempxmlstring CHANGING ixmlnugget.
  PERFORM installnugget USING ixmlnugget overwrt.


*/--------------------------------------------------------------------\
*| Selection screen events                                            |
INITIALIZATION.
  filecom = 'Installation Nugget'.
  checkcom = 'Overwrite Originals'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR nuggfil.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      multiselection    = abap_false
      file_filter       = '*.nugg'
      default_extension = 'nugg'
    CHANGING
      file_table        = retfiletable
      rc                = retrc
      user_action       = retuseraction.
  READ TABLE retfiletable INTO nuggfil INDEX 1.
  REFRESH retfiletable.

*\--------------------------------------------------------------------/


*/--------------------------------------------------------------------\
*| Forms from the SAPLink Installer                                   |
*|                                                                     |
FORM uploadxmlfromlm USING p_filename xmlstring TYPE string .
  DATA retfiletable TYPE filetable.
  DATA retrc TYPE sysubrc.
  DATA retuseraction TYPE i.
  DATA temptable TYPE table_of_strings.
  DATA temptable_bin TYPE TABLE OF x255.
  DATA filelength TYPE i.
  DATA l_filename TYPE string.

  l_filename = p_filename.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'       " File Type Binary
    IMPORTING
      filelength              = filelength
    CHANGING
      data_tab                = temptable_bin
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        PERFORM writemessage USING 'E' 'File Open Error'.
      WHEN OTHERS.
        PERFORM writemessage USING 'E' 'Unknown Error occured'.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = filelength
    IMPORTING
      text_buffer  = xmlstring
    TABLES
      binary_tab   = temptable_bin.
  IF sy-subrc <> 0.
    " Just catch the sy-subrc when there was nothing replaced
    sy-subrc = 0.
  ENDIF.
*  call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
*        exporting
*          FILENAME = l_fileName
*        changing
*          data_tab = tempTable.
*  PERFORM createstring USING temptable CHANGING xmlstring.
ENDFORM.
*\--------------------------------------------------------------------/
FORM createstring
      USING
        temptable TYPE table_of_strings
      CHANGING
        bigstring TYPE string.

  DATA stemp TYPE string.
  LOOP AT temptable INTO stemp.
    CONCATENATE bigstring stemp cl_abap_char_utilities=>newline INTO
    bigstring.
  ENDLOOP.

ENDFORM.
*/----------------------------------------------------------------------



*/--------------------------------------------------------------------\
*| Forms from the SAPLink Root Class                                  |
FORM convertstringtoixmldoc
      USING
        i_xmlstring TYPE string
      CHANGING
        ixmldocument TYPE REF TO if_ixml_document.

  DATA xmlstring TYPE string.
  DATA ixml TYPE REF TO if_ixml.
  DATA streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA istream TYPE REF TO if_ixml_istream.
  DATA ixmlparser TYPE REF TO if_ixml_parser.
  DATA xmldoc TYPE REF TO if_ixml_document.

  xmlstring = i_xmlstring.
  " Make sure to convert Windows Line Break to Unix as
  " this linebreak is used to get a correct import
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN xmlstring WITH cl_abap_char_utilities=>newline.

  ixml = cl_ixml=>create( ).
  xmldoc = ixml->create_document( ).
  streamfactory = ixml->create_stream_factory( ).
  istream = streamfactory->create_istream_string( xmlstring ).
  ixmlparser = ixml->create_parser(  stream_factory = streamfactory
                                     istream        = istream
                                     document       = xmldoc ).
  ixmlparser->parse( ).
  ixmldocument = xmldoc.

ENDFORM.

*|                                                                     |
*|                                                                     |

FORM getobjectinfofromixmldoc
      USING ixmldocument TYPE REF TO if_ixml_document
      CHANGING objtypename TYPE string objname TYPE string.
  DATA rootnode TYPE REF TO if_ixml_node.
  DATA rootattr TYPE REF TO if_ixml_named_node_map.
  DATA attrnode TYPE REF TO if_ixml_node.
  DATA nodename TYPE string.

  rootnode ?= ixmldocument->get_root_element( ).

* get object type
  objtypename = rootnode->get_name( ).
  TRANSLATE objtypename TO UPPER CASE.

* get object name
  rootattr = rootnode->get_attributes( ).
  attrnode = rootattr->get_item( 0 ).
  objname = attrnode->get_value( ).

ENDFORM.

*/--------------------------------------------------------------------\
*|  Nugget Class                                                      |
FORM     installnugget
      USING xmldoc TYPE REF TO if_ixml_document overwrite TYPE c.
  TYPES: BEGIN OF t_objecttable,
           classname TYPE string,
           object    TYPE ko100-object,
           text      TYPE ko100-text,
         END OF t_objecttable.


  DATA iterator TYPE REF TO if_ixml_node_iterator.
  DATA ixml TYPE REF TO if_ixml.
  DATA namefilter TYPE REF TO if_ixml_node_filter.
  DATA parentfilter TYPE REF TO if_ixml_node_filter.
  DATA currentnode TYPE REF TO if_ixml_node.
  DATA newnode TYPE REF TO if_ixml_node.
  DATA rval TYPE i.
  DATA ixmldocument TYPE REF TO if_ixml_document.
  DATA _objname TYPE string.
  DATA objtype TYPE string.
  DATA objecttable TYPE TABLE OF t_objecttable.
  DATA objectline TYPE t_objecttable.
  DATA exists TYPE flag.
  DATA stemp TYPE string.
  DATA namecollision TYPE flag.
  DATA l_targetobject TYPE REF TO zsaplink.
  DATA l_installobject TYPE string.
  DATA l_excclass TYPE REF TO zcx_saplink.
  DATA tempcname TYPE string.

  ixml = cl_ixml=>create( ).
  namefilter = xmldoc->create_filter_name( name = 'nugget' ).
  parentfilter = xmldoc->create_filter_parent( namefilter ).
  iterator = xmldoc->create_iterator_filtered( parentfilter ).

  currentnode ?= iterator->get_next( ).
  WHILE currentnode IS NOT INITIAL.
    CLEAR exists.
    ixmldocument = ixml->create_document( ).
    newnode = currentnode->clone( ).
    rval = ixmldocument->append_child( newnode ).

    CALL METHOD zsaplink=>getobjectinfofromixmldoc
      EXPORTING
        ixmldocument = ixmldocument
      IMPORTING
        objtypename  = objtype
        objname      = _objname.

*  call method zsaplink=>getplugins( changing objectTable = objectTable )
*.
*
*  read table objectTable into objectLine with key object = objType.
*
*  if sy-subrc = 0.

    TRANSLATE objtype TO UPPER CASE.
    CASE objtype.
      WHEN 'CLAS'.
        tempcname = 'ZSAPLINK_CLASS'.
      WHEN 'PROG'.
        tempcname = 'ZSAPLINK_PROGRAM'.
      WHEN 'DOMA'.
        tempcname = 'ZSAPLINK_DOMAINS'.
      WHEN 'DTEL'.
        tempcname = 'ZSAPLINK_DATA_ELEMENTS'.
      WHEN 'FUGR'.
        tempcname = 'ZSAPLINK_FUNCTIONGROUP'.
      WHEN 'TABL'.
        tempcname = 'ZSAPLINK_TABLES'.
      WHEN 'TRAN'.
        tempcname = 'ZSAPLINK_TRANSACTIONS'.
      WHEN 'MSAG'.
        tempcname = 'ZSAPLINK_MESSAGE_CLASS'.
      WHEN OTHERS.
    ENDCASE.

    CREATE OBJECT l_targetobject TYPE (tempcname)
      EXPORTING name = _objname.

    objectexists = l_targetobject->checkexists( ).

    IF objectexists = 'X' AND overwrt = ''.
      WRITE :/  objtype, _objname,
      ' exists on this system , if you wish to install this Nugget '
      & 'please set the Overwrite Originals checkbox.'
          .
    ELSEIF objectexists = 'X' AND overwrt = 'X'.

      IF l_targetobject IS NOT INITIAL.

        IF y2all <> 'X'.
          CONCATENATE objtype _objname INTO stemp SEPARATED BY space.
          PERFORM confirmoverwrite USING stemp
                                CHANGING flag.
          IF flag = '1'. "yes
          ELSEIF flag = '2'. "yes to all
            y2all = 'X'.
          ELSEIF flag = 'A'. "cancel
            WRITE / 'Import cancelled by user'.
*          Flag = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        TRY.
            l_installobject = l_targetobject->createobjectfromixmldoc(
                                            ixmldocument = ixmldocument
                                            overwrite = overwrt ).

          CATCH zcx_saplink INTO l_excclass.
            statusmsg = l_excclass->get_text( ).
            flag = 'X'.
        ENDTRY.
        IF l_installobject IS NOT INITIAL.
          CONCATENATE 'Installed: ' objtype l_installobject
           INTO statusmsg SEPARATED BY space.
        ENDIF.
      ELSE.
        statusmsg = 'an undetermined error occured'.
        flag = 'X'.
      ENDIF.

    ELSE.
      TRY.
          l_installobject = l_targetobject->createobjectfromixmldoc(
                                          ixmldocument = ixmldocument
                                          overwrite = overwrt ).

        CATCH zcx_saplink INTO l_excclass.
          statusmsg = l_excclass->get_text( ).
          flag = 'X'.
      ENDTRY.
      IF l_installobject IS NOT INITIAL.
        CONCATENATE 'Installed: ' objtype l_installobject
         INTO statusmsg SEPARATED BY space.
      ENDIF.
    ENDIF.
    currentnode ?= iterator->get_next( ).
    WRITE: / statusmsg.
  ENDWHILE.
ENDFORM.

*/----------------------confirmOverwrite------------------------------\
FORM confirmoverwrite USING l_objinfo TYPE string
                   CHANGING l_answer TYPE flag.

  DATA l_message TYPE string.
  DATA l_title TYPE string.

  CLEAR l_answer.
  l_title = 'Overwrite confirm. Proceed with CAUTION!'.

  CONCATENATE 'You have selected to overwrite originals.'
    l_objinfo 'will be overwritten. Are you sure?'
    INTO l_message SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = l_title
      text_question         = l_message
      text_button_1         = 'Yes'
      text_button_2         = 'Yes to all'
      default_button        = '1'
      display_cancel_button = 'X'
    IMPORTING
      answer                = l_answer.
ENDFORM.
*\--------------------------------------------------------------------/
*/---------------------writeMessage-----------------------\
FORM writemessage USING VALUE(p_type) TYPE sy-msgty
                        VALUE(p_msg).
  CASE p_type.
    WHEN 'E' OR 'A' OR 'X'.
      WRITE / icon_led_red AS ICON.
    WHEN 'W'.
      WRITE / icon_led_yellow AS ICON.
    WHEN OTHERS.
      WRITE / icon_led_green AS ICON.
  ENDCASE.

  WRITE p_msg.
ENDFORM.                    "WriteMessage
