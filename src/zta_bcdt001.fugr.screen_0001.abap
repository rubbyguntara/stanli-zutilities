PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zta_bcdt001 CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zta_bcdt001-if_group .
      FIELD zta_bcdt001-if_code .
      FIELD zta_bcdt001-description .
      FIELD zta_bcdt001-direction .
      FIELD zta_bcdt001-if_type .
      FIELD zta_bcdt001-source_sys .
      FIELD zta_bcdt001-target_sys .
      FIELD zta_bcdt001-balobj .
      FIELD zta_bcdt001-balsubobj .
      FIELD zta_bcdt001-is_active .
      FIELD zta_bcdt001-ernam .
      FIELD zta_bcdt001-erdat .
      FIELD zta_bcdt001-erzet .
      FIELD zta_bcdt001-aenam .
      FIELD zta_bcdt001-aedat .
      FIELD zta_bcdt001-aezet .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zta_bcdt001-if_group .
      FIELD zta_bcdt001-if_code .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE modify_field.
  MODULE liste_after_loop.
