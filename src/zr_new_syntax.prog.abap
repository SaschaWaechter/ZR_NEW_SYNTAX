********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2021
*
* Programm für einfache und nachvollziehbare Testfälle zur neuen ABAP-Syntax ab 7.40
********************************************************************************
REPORT zr_new_syntax.
DATA lt_seltab TYPE TABLE OF rsparams.
DATA lt_method_Calls TYPE stringtab.

SELECTION-SCREEN BEGIN OF BLOCK rel_740_5 WITH FRAME TITLE TEXT-745.
  "MOVE-CORRESPONDING Internal Table
  PARAMETERS p_corrit AS CHECKBOX.
  "CORRESPONDING Operator
  PARAMETERS p_corr AS CHECKBOX.
  "Meshes
  PARAMETERS p_mesh AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK rel_740_5.

SELECTION-SCREEN BEGIN OF BLOCK rel_750 WITH FRAME TITLE TEXT-750.
  "SELECT UNION
  PARAMETERS p_sel_un AS CHECKBOX.
  "Dynamische RFC-Destination
  PARAMETERS p_dynrfc AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK rel_750.

CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
  EXPORTING
    curr_report     = sy-repid
  TABLES
    selection_table = lt_seltab
  EXCEPTIONS
    not_found       = 1
    no_report       = 2
    OTHERS          = 3.

LOOP AT lt_seltab ASSIGNING FIELD-SYMBOL(<ls_seltab>) WHERE low = abap_true.
  lt_method_calls = VALUE stringtab( BASE lt_method_calls ( |{ <ls_seltab>-selname+2 }| ) ).
ENDLOOP.
NEW zcl_new_syntax( lt_method_Calls ).
