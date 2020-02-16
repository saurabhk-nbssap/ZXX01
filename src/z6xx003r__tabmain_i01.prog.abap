*&---------------------------------------------------------------------*
*&  Include           Z6XX003R__TABMAIN_I01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZPROG_SD_TABMAIN_I01                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code .

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' .
      CLEAR : ok_code .
      SET SCREEN 0 . LEAVE SCREEN .

    WHEN 'SAVE' .
      CLEAR : ok_code .
      PERFORM save_record .
      SET SCREEN 0 . LEAVE SCREEN .

  ENDCASE .

ENDMODULE.                 " user_command_0100  INPUT
