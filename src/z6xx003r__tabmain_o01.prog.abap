*&---------------------------------------------------------------------*
*&  Include           Z6XX003R__TABMAIN_O01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZPROG_SD_TABMAIN_O01                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100' .
  SET TITLEBAR  '100' .
ENDMODULE.                 " status_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  DATA : v_field(10) .

  LOOP AT SCREEN.
    IF screen-name CS 'VAL' .
      CONCATENATE 'LBL' screen-name+3(3) INTO v_field .
      ASSIGN (v_field) TO <field> .
      IF <field> NE space .
        READ TABLE itab WITH KEY field = <field> .
        IF sy-subrc EQ 0 .

*          IF itab-type EQ 'DATS' .
*            IF screen-name NS '_D' .
*              wrong_field = 'X' .
*            ENDIF .
*          ELSE .
*            CLEAR wrong_field .
*          ENDIF .
*          IF wrong_field = 'X' .
*            screen-input = off .
*            screen-invisible = on .
*            MODIFY SCREEN .
*            CONTINUE .
*          ENDIF .
          IF itab-keyflag = 'X' .
            screen-input    = off.
          ELSE .
            screen-input    = on.
          ENDIF .
          screen-output   = on.
          screen-length = itab-leng.
          screen-display_3d  = on.
          MODIFY SCREEN.
        ENDIF .
      ELSE .
        screen-input = off .
        screen-invisible = on .
        MODIFY SCREEN.
      ENDIF .
    ENDIF .
  ENDLOOP .
ENDMODULE.                 " modify_screen  OUTPUT
