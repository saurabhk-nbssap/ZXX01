FUNCTION CLEAR_EIS_INITIAL_VALUE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_STRUKTUR_NAME_I1)
*"       CHANGING
*"             VALUE(I_STRUKTUR)
*"       EXCEPTIONS
*"              ERROR_FOUND
*"----------------------------------------------------------------------
  DATA:
        CHAR.

  DATA: BEGIN OF UB_DFIES OCCURS 2000.
          INCLUDE STRUCTURE DFIES.
  DATA: END   OF UB_DFIES.

* DDIC Info besorgen
  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
       EXPORTING
            I_STRUCTURE_TABNAME = I_STRUKTUR_NAME_I1
       TABLES
            IT_DFIES            = UB_DFIES
       EXCEPTIONS
            OTHERS              = C_RC4.
  IF SY-SUBRC <> C_RC0.
    RAISE ERROR_FOUND.
  ENDIF.

  SORT UB_DFIES BY TABNAME POSITION ASCENDING.
  CLEAR SY-SUBRC.
  LOOP AT UB_DFIES WHERE TABNAME = I_STRUKTUR_NAME_I1.
    ASSIGN COMPONENT ub_dfies-position of structure i_struktur
            TO <F1> TYPE UB_DFIES-INTTYPE.

    CHECK: SY-SUBRC = C_RC0.
*          <f1> co c_latte.
    CHAR = <F1>.                                            "XER
    CHECK CHAR = C_LATTE.                                   "XER
    CLEAR <F1>.
  ENDLOOP.

ENDFUNCTION.
