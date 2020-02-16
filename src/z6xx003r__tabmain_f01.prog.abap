*&---------------------------------------------------------------------*
*&  Include           Z6XX003R__TABMAIN_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZPROG_SD_TABMAIN_F01                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  modify_selection
*&---------------------------------------------------------------------*
FORM modify_selection.
  LOOP AT SCREEN.
    IF screen-name(4) EQ 'FELD'
    OR screen-name(6) EQ '%_FELD'.
      screen-input    = off.
      screen-output   = off.
      screen-display_3d  = off.
*     screen-invisible = on.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  SELECT * FROM dd03l WHERE tabname EQ tabname
                        AND as4local EQ 'A'
                        AND keyflag EQ 'X'
                        AND rollname NE 'MANDT'.
    CONCATENATE 'FELD' dd03l-position INTO feldname.
    CONCATENATE '%_' feldname '_%_APP_%-TEXT'
                              INTO labname.
    LOOP AT SCREEN.
      IF screen-name EQ feldname.
        screen-input    = on.
        screen-output   = on.
*       screen-invisible = off.
        screen-length = dd03l-leng.
        screen-display_3d  = on.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ  labname.
        ASSIGN (labname) TO <label>.
        <label> = dd03l-fieldname.
        screen-input    = on.
        screen-output   = on.
*       screen-invisible = off.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDSELECT.
ENDFORM.                    " modify_selection

*&---------------------------------------------------------------------*
*&      Form  get_record
*&---------------------------------------------------------------------*
FORM get_record.
  REFRESH wtab.
  SELECT * FROM dd03l WHERE tabname EQ tabname
                        AND as4local EQ 'A'
                        AND keyflag EQ 'X'
                        AND rollname NE 'MANDT'.
    CLEAR wtab.
    MOVE dd03l-fieldname TO wtab+1.
    CONCATENATE 'FELD' dd03l-position INTO feldname.
    ASSIGN (feldname) TO <wert>.
    CONCATENATE and wtab ' EQ ''' <wert> '''' INTO wtab.
    APPEND wtab.
    and = 'AND'.
  ENDSELECT.

*  SELECT * UP TO count ROWS
*           FROM (tabname) INTO TABLE buffer WHERE (wtab).
  CLEAR not_found .
  SELECT SINGLE *
           FROM (tabname) INTO CORRESPONDING FIELDS OF <l_line>
           WHERE (wtab).
  IF sy-subrc NE 0 .
    not_found = 'X' .
    MESSAGE s002(sy) WITH 'No table entries found for specified key' .
  ENDIF .
*  LOOP AT buffer.
*    WRITE: / buffer.
*  ENDLOOP .
ENDFORM.                    " get_record

*&---------------------------------------------------------------------*
*&      Form  get_table_fields
*&---------------------------------------------------------------------*
FORM get_table_fields.
  DATA : counter(3) TYPE n .

  FIELD-SYMBOLS : <val> .

  SELECT SINGLE * FROM tadir INTO itadir
                  WHERE pgmid     EQ  'R3TR'
                  AND   object    EQ  'TABL'
                  AND   obj_name  EQ  tabname .

  IF sy-subrc EQ 0 .
    SELECT * FROM dd03l WHERE tabname EQ itadir-obj_name .
      IF dd03l-fieldname(1) NE '.' .
        MOVE dd03l-fieldname  TO  itab-field   .
        MOVE dd03l-keyflag    TO  itab-keyflag .
        MOVE dd03l-leng       TO  itab-leng    .
        MOVE dd03l-decimals   TO  itab-deci    .
        APPEND itab .
        CLEAR  itab .
      ENDIF .
    ENDSELECT .
  ENDIF .

*    ASSIGN COMPONENT 'SUBRC' OF STRUCTURE <l_line> TO <l_field>.
  LOOP AT itab .

    counter = sy-tabix .
    CONCATENATE 'LBL' counter INTO field .
    ASSIGN (field) TO <field> .
    <field> = itab-field .

    ASSIGN COMPONENT itab-field OF STRUCTURE <l_line> TO <l_field> .
    CONCATENATE 'VAL' counter INTO field .
    ASSIGN (field) TO <field> .
    <field> = <l_field> .
    CONDENSE <field> .

  ENDLOOP .
ENDFORM.                    " get_table_fields

*&---------------------------------------------------------------------*
*&      Form  generate_table
*&---------------------------------------------------------------------*
FORM generate_table.
**-- Build fieldcat
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = tabname
       CHANGING
            ct_fieldcat      = it_fcat[].
  LOOP AT it_fcat INTO is_fcat . "WHERE NOT reptext_ddic IS initial.
    MOVE-CORRESPONDING is_fcat TO is_fieldcat.
    is_fieldcat-fieldname = is_fcat-fieldname.
    is_fieldcat-ref_field = is_fcat-fieldname.
    is_fieldcat-ref_table = is_fcat-ref_tabname.
    APPEND is_fieldcat TO it_fieldcat.
  ENDLOOP.

**-- Create a new Table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
         EXPORTING
          it_fieldcatalog = it_fieldcat
         IMPORTING
          ep_table        = new_table.

**-- Create a new Line with the same structure of the table.
  ASSIGN new_table->* TO <l_table>.
  CREATE DATA new_line LIKE LINE OF <l_table>.
  ASSIGN new_line->* TO <l_line>.

**-- test it...
*  DO 30 TIMES.
*    ASSIGN COMPONENT 'SUBRC' OF STRUCTURE <l_line> TO <l_field>.
*    <l_field> = sy-index.
*    INSERT <l_line> INTO TABLE <l_table>.
*  ENDDO.
*
*  LOOP AT <l_table> ASSIGNING <l_line>.
*    ASSIGN COMPONENT 'SUBRC' OF STRUCTURE <l_line> TO <l_field>.
*    WRITE <l_field>.
*  ENDLOOP.
ENDFORM.                    " generate_table

*&---------------------------------------------------------------------*
*&      Form  save_record
*&---------------------------------------------------------------------*
FORM save_record.
  DATA : counter(3) TYPE n .
*  SELECT SINGLE *
*           FROM (tabname) INTO CORRESPONDING FIELDS OF <l_line>
*           WHERE (wtab).
  LOOP AT itab .
    counter = sy-tabix .
    IF itab-keyflag EQ space .
      ASSIGN COMPONENT itab-field OF STRUCTURE <l_line> TO <l_field> .
      CONCATENATE 'VAL' counter INTO field .
      ASSIGN (field) TO <field> .
      IF sy-subrc EQ 0 .
        CONDENSE <field> .
        <l_field> = <field> .
      ENDIF .
    ENDIF .
  ENDLOOP .
  UPDATE (tabname) FROM <l_line> .
  IF sy-subrc EQ 0 .
    MESSAGE s002(sy) WITH 'Record saved successfully' .
  ELSE .
    MESSAGE s002(sy) WITH 'Record not saved' .
  ENDIF .
ENDFORM.                    " save_record
