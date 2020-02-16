FUNCTION TRFDUE_ERROR_HANDLER.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_AKTYP) LIKE  TP105-AKTYP
*"             VALUE(I_AG) LIKE  SPROT_X-AG OPTIONAL
*"             VALUE(I_SEVERITY) LIKE  SPROT_X-SEVERITY OPTIONAL
*"             VALUE(I_MSGNR) LIKE  SPROT_X-MSGNR OPTIONAL
*"             VALUE(I_INDEX) LIKE  SPROT_X-INDEX OPTIONAL
*"             VALUE(I_VAR1) OPTIONAL
*"             VALUE(I_VAR2) OPTIONAL
*"             VALUE(I_VAR3) OPTIONAL
*"             VALUE(I_VAR4) OPTIONAL
*"             VALUE(I_COMPLETE) LIKE  SY-BATCH DEFAULT ' '
*"       TABLES
*"              IT_ERROR_TAB STRUCTURE  SPROT_X
*"       EXCEPTIONS
*"              ERROR_FOUND
*"              NO_ENTRY
*"----------------------------------------------------------------------

CONSTANTS:
  C_BUILD_ERROR_ITAB LIKE TP105-AKTYP VALUE '10',
  C_CHANCE_ERROR_ITAB LIKE TP105-AKTYP VALUE '20',
  C_FILL_ERROR_ITAB LIKE TP105-AKTYP VALUE '30',
  C_CLEAN_UP_ERROR_ITAB LIKE TP105-AKTYP VALUE '40'.

DATA: RC LIKE SY-SUBRC.

  CASE I_AKTYP.
    WHEN C_BUILD_ERROR_ITAB.
      PERFORM BUILD_ERROR_HEADER   TABLES   IT_ERROR_TAB
                                   USING    I_SEVERITY
                                            I_VAR1 I_VAR2
                                   CHANGING RC.
    WHEN C_CHANCE_ERROR_ITAB.
      PERFORM CHANGE_ERROR_HEADER TABLES   IT_ERROR_TAB
                                  USING    I_VAR1 I_VAR2
                                  CHANGING RC.
      IF RC <> C_RC0.
        RAISE NO_ENTRY.
      ENDIF.
    WHEN C_FILL_ERROR_ITAB.
      PERFORM FILL_ERROR_ITAB TABLES   IT_ERROR_TAB
                              USING    I_AG I_SEVERITY I_MSGNR I_INDEX
                                       I_VAR1 I_VAR2 I_VAR3 I_VAR4
                              CHANGING RC.
    WHEN C_CLEAN_UP_ERROR_ITAB.
      PERFORM CLEAN_UP_ERROR_TABLE TABLES   IT_ERROR_TAB
                                   USING    I_COMPLETE
                                   CHANGING RC.
    WHEN OTHERS.
      RC = C_RC4.
  ENDCASE.
  CHECK RC <> C_RC0.
      RAISE ERROR_FOUND.
ENDFUNCTION.
