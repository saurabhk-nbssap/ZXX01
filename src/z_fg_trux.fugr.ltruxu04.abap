FUNCTION SAP_CONVERT_TO_CSV_FORMAT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 DEFAULT ';'
*"             VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"             VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"             VALUE(I_APPL_KEEP) TYPE  CHAR01 DEFAULT SPACE
*"       TABLES
*"              I_TAB_SAP_DATA TYPE  STANDARD TABLE
*"       CHANGING
*"             VALUE(I_TAB_CONVERTED_DATA) TYPE  TRUXS_T_TEXT_DATA
*"                             OPTIONAL
*"       EXCEPTIONS
*"              CONVERSION_FAILED
*"----------------------------------------------------------------------
  CONSTANTS: C_FIELD_SEPARATOR VALUE ';'.

  DATA:
        L_START_STRING LIKE SY-FDPOS,
        L_END_STRING LIKE SY-FDPOS,
        L_LEN_STRING(6) TYPE N,
        L_EOL_STRING LIKE SY-FDPOS,
        L_START_TARGET_STRING LIKE SY-FDPOS,
        L_END_TARGET_STRING LIKE SY-FDPOS,
        L_LEN_TARGET_STRING(6) TYPE N,
        L_EOL_TARGET_STRING LIKE SY-FDPOS,
        L_CONV_DATA TYPE LINE OF TRUXS_T_TEXT_DATA,
        L_CSV_DATA TYPE LINE OF TRUXS_T_TEXT_DATA.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      I_FIELD_SEPERATOR    = C_FIELD_SEPARATOR
      I_LINE_HEADER        = I_LINE_HEADER
      I_FILENAME           = I_FILENAME
    TABLES
      I_TAB_SAP_DATA       = I_TAB_SAP_DATA
    CHANGING
      I_TAB_CONVERTED_DATA = I_TAB_CONVERTED_DATA
    EXCEPTIONS
      CONVERSION_FAILED    = C_RC4.

  CHECK SY-SUBRC <> C_RC0.
*  if sy-subrc <> c_rc0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
          RAISING CONVERSION_FAILED.
*  endif.

  DESCRIBE FIELD L_CSV_DATA LENGTH L_LEN_TARGET_STRING
                                     in character mode.
  DESCRIBE FIELD L_CONV_DATA LENGTH L_EOL_STRING
                                     in character mode.
  LOOP AT I_TAB_CONVERTED_DATA INTO L_CONV_DATA.
    L_START_STRING = 1.
    CLEAR: L_CSV_DATA,
           L_START_TARGET_STRING,
           L_END_TARGET_STRING.
    DO.
      SEARCH L_CONV_DATA FOR C_FIELD_SEPARATOR STARTING AT
                                                  L_START_STRING
                                                  ENDING AT
                                                  L_EOL_STRING.
      IF SY-SUBRC <> C_RC0.
        EXIT.
      ENDIF.
      IF SY-SUBRC = C_RC0.
        L_END_STRING = L_START_STRING + SY-FDPOS.
        L_LEN_STRING = L_END_STRING - L_START_STRING + 1.
        L_START_STRING = L_START_STRING - 1.
        L_END_TARGET_STRING = L_END_TARGET_STRING + L_LEN_STRING + 4.
        IF L_END_TARGET_STRING < L_LEN_TARGET_STRING.
          L_CSV_DATA+L_START_TARGET_STRING(3) = '"""'.
          L_START_TARGET_STRING = L_START_TARGET_STRING + 3.
          L_CSV_DATA+L_START_TARGET_STRING(L_LEN_STRING) =
                                      L_CONV_DATA+L_START_STRING.
          L_START_TARGET_STRING = L_START_TARGET_STRING + L_LEN_STRING
                                                        - 1.
          L_CSV_DATA+L_START_TARGET_STRING(1) = '"'.
          L_START_TARGET_STRING = L_START_TARGET_STRING + 1.
          L_CSV_DATA+L_START_TARGET_STRING(1) = ';'.
          L_END_TARGET_STRING = STRLEN( L_CSV_DATA ).
          L_START_TARGET_STRING = L_END_TARGET_STRING.
          L_START_STRING = L_END_STRING + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
    MODIFY I_TAB_CONVERTED_DATA FROM L_CSV_DATA.
  ENDLOOP.
ENDFUNCTION.
