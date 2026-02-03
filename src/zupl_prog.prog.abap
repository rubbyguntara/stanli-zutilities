*&---------------------------------------------------------------------*
*& Report ZUPL_PROG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zupl_prog
     NO STANDARD PAGE HEADING
     LINE-SIZE 132
     LINE-COUNT 55.


TABLES:trdir.
DATA:
     yesno,
     wk-title(128),
     d_file(128) TYPE c.

DATA: it_tab  TYPE filetable,
      d_subrc TYPE i.

DATA:
  BEGIN OF textrec,
    id(1),
    key(8),
    entry(70),
  END         OF textrec,

  BEGIN OF texttab OCCURS 0,
    id(1),
    key(8),
    entry(70),
  END         OF texttab,

  BEGIN OF abaprec,
    line(150),
  END         OF abaprec,

  BEGIN OF abaptab OCCURS 500,
    line(255),
  END         OF abaptab,

  BEGIN OF trtab OCCURS 0,
    name  LIKE trdir-name,
    entry LIKE texttab-entry,
    cdat  LIKE trdir-cdat,
    udat  LIKE trdir-udat,
  END         OF trtab.

SELECTION-SCREEN BEGIN OF BLOCK 01 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) comm1.
PARAMETERS: p_name TYPE programm.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) comm2.
PARAMETERS: p_file TYPE filename.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) comm3.
PARAMETERS: p_upload AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK 01.


AT SELECTION-SCREEN OUTPUT.
  comm1 = 'Program Name'.
  comm2 = 'File Name'.
  comm3 = 'Upload ?'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select File'
      default_filename = '*.txt'
    CHANGING
      file_table       = it_tab
      rc               = d_subrc.

  READ TABLE it_tab INDEX 1 INTO p_file.


START-OF-SELECTION.
  d_file = p_file.
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      filename            = d_file
      filetype            = 'ASC'
    TABLES
      data_tab            = abaptab
    EXCEPTIONS
      file_open_error     = 1
      file_write_error    = 2
      invalid_filesize    = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      OTHERS              = 8.

  IF p_upload EQ 'X'.
    SELECT SINGLE * FROM trdir WHERE name = p_name.
    IF sy-subrc = 0.
      wk-title(8) = p_name.
      wk-title+9(20) = 'Already exists'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        EXPORTING
          diagnosetext1 = wk-title
          textline1     = 'Do you want to overwrite?'
          titel         = 'Upload'
        IMPORTING
          answer        = yesno
        EXCEPTIONS
          OTHERS        = 1.

      IF yesno = 'J'.
        INSERT REPORT p_name FROM abaptab.
      ENDIF.
    ELSE.
      INSERT REPORT p_name FROM abaptab.
    ENDIF.
  ELSE.
    LOOP AT abaptab.
      WRITE:/ abaptab.
    ENDLOOP.
  ENDIF.
