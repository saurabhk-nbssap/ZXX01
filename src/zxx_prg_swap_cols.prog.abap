*&---------------------------------------------------------------------*
*& Report  ZXX_PRG_SWAP_COLS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zxx_prg_swap_cols.

* ---- Data declaration ---- *
TABLES: sscrfields.

DATA: ref_tab TYPE REF TO data.
DATA: tmp     TYPE REF TO data. " temporary store during swap
DATA: tmp_fld TYPE dd03d-fieldname.
FIELD-SYMBOLS: <table> TYPE ANY TABLE,
               <wa>    TYPE any,
               <f1>    TYPE any,
               <f2>    TYPE any,
               <tmp>   TYPE any.  " temporary store during swap

DATA: dd02l TYPE dd02l, " tables
      dd03l TYPE dd03l. " tables - fieldnames

* ---- Selection Screen ---- *
SELECTION-SCREEN BEGIN OF BLOCK bwrn WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 25(79) text-wrn MODIF ID wrn.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bwrn.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-001.
PARAMETERS: tabname TYPE dd02d-dbtabname OBLIGATORY,
            field1  TYPE dd03d-fieldname OBLIGATORY,
            field2  TYPE dd03d-fieldname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk.

* ---- Selection screen events ---- *
* ---- PBO ---- *
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'WRN'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* ---- PAI ---- *
AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'ONLI'.
    CLEAR dd02l.
    SELECT SINGLE * FROM dd02l INTO dd02l WHERE tabname = tabname.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid table' TYPE 'E'.
    ENDIF.
    CLEAR dd03l.
    SELECT SINGLE * FROM dd03l INTO dd03l WHERE tabname = tabname AND fieldname = field1.
    IF sy-subrc <> 0.
      MESSAGE 'Field' && ` ` && field1 && ` ` && 'does not belong to table' && ` ` && tabname TYPE 'E'.
    ENDIF.
    SELECT SINGLE * FROM dd03l INTO dd03l WHERE tabname = tabname AND fieldname = field2.
    IF sy-subrc <> 0.
      MESSAGE 'Field' && ` ` && field2 && ` ` && 'does not belong to table' && ` ` && tabname TYPE 'E'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  TRY.
      CREATE DATA ref_tab TYPE TABLE OF (tabname).
      ASSIGN ref_tab->* TO <table>.
      CHECK <table> IS ASSIGNED.
      SELECT *
        FROM (tabname)
        INTO TABLE <table>.

      CHECK <table> IS NOT INITIAL.
      tmp_fld = tabname && '-' && field1.
      CREATE DATA tmp TYPE (tmp_fld).
      ASSIGN tmp->* TO <tmp>.
      UNASSIGN <wa>.

      LOOP AT <table> ASSIGNING <wa>.
        UNASSIGN: <f1>, <f2>.
        CLEAR <tmp>.
        ASSIGN COMPONENT field1 OF STRUCTURE <wa> TO <f1>.
        <tmp> = <f1>.
        ASSIGN COMPONENT field2 OF STRUCTURE <wa> TO <f2>.

        MOVE <f2> TO <f1>.
        MOVE <tmp> TO <f2>.
      ENDLOOP.

      DATA: table TYPE rstable-tabname.
      CLEAR table.
      table = tabname.
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          tabname        = table
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        UPDATE (tabname) FROM TABLE <table>.
        IF sy-dbcnt > 0.
          COMMIT WORK AND WAIT.
          IF sy-subrc = 0.
            WRITE: / 'Done'.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_E_TABLE'
          EXPORTING
            tabname        = table
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
      ENDIF.

    CATCH cx_root.
      WRITE: / 'Error during processing. Please check your input.'.
  ENDTRY.

END-OF-SELECTION.
  LEAVE TO LIST-PROCESSING.
