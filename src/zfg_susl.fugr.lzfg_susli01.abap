*-------------------------------------------------------------------
***INCLUDE LSUSLI01 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0254  INPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0254 INPUT.
  IF FERTIG >= 1.
    SET SCREEN 000.
  ELSE.
    LEAVE TO LIST-PROCESSING.
    NEW-PAGE NO-TITLE NO-HEADING.
    PERFORM LIST_TCODES.
    FERTIG = 1.
      ENDIF.
  CLEAR FCODE.
ENDMODULE.                 " USER_COMMAND_0254  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0254_b  INPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0254_B INPUT.
  IF FERTIG >= 1.
    SET SCREEN 000.
  ELSE.
    LEAVE TO LIST-PROCESSING.
    NEW-PAGE NO-TITLE NO-HEADING.
    PERFORM LIST_TCODES_B.    " wie List_tcodes Ausdruck der Option
                              " S_TCODE oder TSTCA
    FERTIG = 1.
  ENDIF.
  CLEAR FCODE.
ENDMODULE.                 " USER_COMMAND_0254  INPUT
