*-------------------------------------------------------------------
***INCLUDE LSUSLE01 .
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&   Event AT LINE-SELECTION
*&---------------------------------------------------------------------*
TYPES: BEGIN OF INTUSOBT_NEW_TEMP,
       OBJECT TYPE USOBT_C-OBJECT,
       FIELD  TYPE  USOBT_C-FIELD,
       LOW    TYPE  USOBT_C-LOW,
       HIGH   TYPE  USOBT_C-HIGH,
       END OF INTUSOBT_NEW_TEMP.


DATA: GS_INTUSOBT_NEW1 TYPE INTUSOBT_NEW_TEMP.
DATA: GT_INTUSOBT TYPE STANDARD TABLE OF INTUSOBT_NEW_TEMP.


****AT LINE-SELECTION.

****  CASE SY-PFKEY.
****    WHEN 'TCOD'.
****  CALL FUNCTION 'SUSR_TCODE_DISPLAY'
****           EXPORTING   NAME    = H_TCOD_TCODE
****                       TYPE    = TYPE_TCODE
****           EXCEPTIONS  OTHERS  = 1.
****
****
****   ENDCASE.


TOP-OF-PAGE.
  NEW-PAGE NO-TITLE NO-HEADING.
  PERFORM LISTTITEL.
*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM LISTTITEL.

AT USER-COMMAND.
  CASE SY-UCOMM.
  WHEN 'SORT'.
    IF SY-CUCOL > 43.
      SORT TEMPTSTCT1 BY SELTYPE.
      PERFORM LIST_TCODES_B.
    ENDIF.
    IF SY-CUCOL < 43.
      SORT TEMPTSTCT1 BY TCODE.
      PERFORM LIST_TCODES_B.
    ENDIF.
  ENDCASE.




*  class lcl_handle_events definition deferred.
*
*  data: gr_table   type ref to cl_salv_table.
*
* for handling the events of cl_salv_table
*   data: gr_events type ref to lcl_handle_events.
*
* reference to a functions object
*data: gr_functions type ref to cl_salv_functions_list.
