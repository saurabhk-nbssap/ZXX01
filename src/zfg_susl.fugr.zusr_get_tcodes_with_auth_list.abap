FUNCTION ZUSR_GET_TCODES_WITH_AUTH_LIST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SELTYPE) LIKE  USSEL1-SELTYPE
*"     VALUE(SELTYPETEXT)
*"     VALUE(NAME)
*"     VALUE(NAME2) OPTIONAL
*"     VALUE(AUTHS) LIKE  USREF STRUCTURE  USREF
*"  TABLES
*"      I_TEMPTSTCT STRUCTURE  TSTCT OPTIONAL
*"----------------------------------------------------------------------

  IF SELTYPE = ' '. SELTYPE = 'T'. ENDIF.
  CASE SELTYPE.
    WHEN 'T'.                          " Selektion TSTCA
      REFRESH TCODES.
      CALL FUNCTION 'SUSR_GET_TCODES_WITH_AUTH'
           TABLES
                AUTHS  = AUTS
                TCODES = TCODES
           EXCEPTIONS
                OTHERS = 1.

      CALL FUNCTION 'SUSR_TCODES_LIST'
           EXPORTING
                SELTYPE = SELTYPETEXT
                NAME    = NAME
                NAME2   = NAME2
           TABLES
                TCODES  = TCODES
           EXCEPTIONS
                OTHERS  = 1.

    WHEN 'S'.                          " S_Tcode
      REFRESH TCODES.
      CALL FUNCTION 'SUSR_GET_TCODES_AUTH_S_TCODE'
           TABLES
                AUTHS  = AUTS
                TCODES = TCODES
           EXCEPTIONS
                OTHERS = 1.
      CALL FUNCTION 'SUSR_TCODES_LIST'
           EXPORTING
                SELTYPE = SELTYPETEXT
                NAME    = NAME
           TABLES
                TCODES  = TCODES
           EXCEPTIONS
                OTHERS  = 1.


    WHEN 'B' OR 'C'.                          " both , compare
      REFRESH TCODES.
      REFRESH TCODES1.
      CALL FUNCTION 'SUSR_GET_TCODES_WITH_AUTH'
           TABLES
                AUTHS  = AUTS
                TCODES = TCODES
           EXCEPTIONS
                OTHERS = 1.

      TCODES1-SELTYPE = 'T'.
      LOOP AT TCODES.
        MOVE-CORRESPONDING TCODES TO TCODES1.
        APPEND TCODES1.
      ENDLOOP.

      REFRESH TCODES.
      CALL FUNCTION 'SUSR_GET_TCODES_AUTH_S_TCODE'
           TABLES
                AUTHS  = AUTS
                TCODES = TCODES
           EXCEPTIONS
                OTHERS = 1.

      TCODES1-SELTYPE = 'S'.
      LOOP AT TCODES.
        MOVE-CORRESPONDING TCODES TO TCODES1.
        APPEND TCODES1.
      ENDLOOP.

      CALL FUNCTION 'ZSUSR_TCODES_LIST_BOTH'
           EXPORTING
                SELTYPE = SELTYPETEXT
                NAME    = NAME
                NAME2   = NAME2
                COMPARE = SELTYPE
           TABLES
                TCODES  = TCODES1
                E_TEMPTSTCT = I_TEMPTSTCT
           EXCEPTIONS
                OTHERS  = 1.
  ENDCASE.

ENDFUNCTION.
