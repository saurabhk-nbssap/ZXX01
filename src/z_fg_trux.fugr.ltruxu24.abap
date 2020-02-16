FUNCTION TEXT_CONVERT_CSV_TO_SAP.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 DEFAULT ';'
*"             VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"             VALUE(I_TAB_RAW_DATA) TYPE  TRUXS_T_TEXT_DATA
*"             VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"       TABLES
*"              I_TAB_CONVERTED_DATA TYPE  STANDARD TABLE
*"       EXCEPTIONS
*"              CONVERSION_FAILED
*"----------------------------------------------------------------------

  CONSTANTS: C_FIELD_SEPARATOR VALUE ';'.

  CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
       EXPORTING
            I_FIELD_SEPERATOR    = C_FIELD_SEPARATOR
            I_LINE_HEADER        = I_LINE_HEADER
            I_TAB_RAW_DATA       = I_TAB_RAW_DATA
            I_FILENAME           = I_FILENAME
       TABLES
            I_TAB_CONVERTED_DATA = I_TAB_CONVERTED_DATA
       EXCEPTIONS
            OTHERS               = C_RC4.

  CHECK SY-SUBRC <> C_RC0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
          RAISING CONVERSION_FAILED.

ENDFUNCTION.
