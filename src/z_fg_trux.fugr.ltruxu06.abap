FUNCTION SAP_CONVERT_TO_TEX_FORMAT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(I_APPL_KEEP) TYPE  CHAR01 DEFAULT SPACE
*"     VALUE(I_STEP) TYPE  I DEFAULT 5
*"  TABLES
*"      I_TAB_SAP_DATA TYPE  STANDARD TABLE
*"  CHANGING
*"     VALUE(I_TAB_CONVERTED_DATA) TYPE  TRUXS_T_TEXT_DATA OPTIONAL
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
* N2418726: Do not call 'SAPGUI_PROGRESS_INDICATOR' too often

  FIELD-SYMBOLS: <F_SOURCE>.

  DATA:
        L_PERCENTAGE(4)  TYPE C,
        L_TEXT80(80),
        L_TEXT6(6),
        L_ITAB_ENTRIES LIKE SY-TABIX,
        L_TABIX LIKE SY-TABIX,
        L_HELP_ID LIKE TLINE-TDLINE,
        L_STRUC_RAW_DATA LIKE LINE OF I_TAB_CONVERTED_DATA,
        L_STRUC_COL_NAMES LIKE LINE OF I_TAB_CONVERTED_DATA,
        L_MAX_FIELD LIKE L_STRUC_RAW_DATA,
        L_DATE_EXTERN(30) TYPE C,
        L_TYPE,
        L_EDIT_MASK(10) TYPE C,
        L_LEN_STRING  TYPE I,
        L_LEN_COL_STRING  TYPE I,
        L_LEN_FIELD_SEP TYPE I,
        L_START_STRING TYPE I,
        L_START_COL_STRING TYPE I,
        L_END_STRING TYPE I,
        L_END_COL_STRING TYPE I,
        L_EOL_STRING TYPE I,
        L_STRUC_INDEX LIKE SY-INDEX,
        L_PERCENTAGE_MARK  TYPE I.

  CLEAR I_LINE_HEADER.                 " is not supported yet.

  CLEAR: I_TAB_CONVERTED_DATA.
  REFRESH: I_TAB_CONVERTED_DATA.

  DESCRIBE FIELD L_STRUC_RAW_DATA LENGTH L_EOL_STRING
                                    in character mode.
  L_LEN_FIELD_SEP = STRLEN( I_FIELD_SEPERATOR ).

  DESCRIBE TABLE I_TAB_SAP_DATA LINES L_ITAB_ENTRIES.
  LOOP AT I_TAB_SAP_DATA.
    L_TABIX = SY-TABIX.
    CLEAR: L_STRUC_INDEX, L_STRUC_RAW_DATA, L_START_STRING,
           L_START_COL_STRING.
    IF SY-BATCH IS INITIAL.
      L_PERCENTAGE = L_TABIX / L_ITAB_ENTRIES * 100.
      L_TEXT6 = L_TABIX.
      CONDENSE L_TEXT6 NO-GAPS.
      L_TEXT80 = TEXT-KON.
      REPLACE '&&&&&&' WITH L_TEXT6 INTO L_TEXT80.
      L_TEXT6 = L_ITAB_ENTRIES.
      REPLACE '&&&&&&' WITH L_TEXT6 INTO L_TEXT80.
      CONDENSE L_TEXT80.

      IF L_PERCENTAGE > L_PERCENTAGE_MARK.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
           EXPORTING
                PERCENTAGE = L_PERCENTAGE
                TEXT       = L_TEXT80.
        L_PERCENTAGE_MARK = L_PERCENTAGE_MARK + I_STEP.
     ENDIF.
    ENDIF.
    DO.
      L_STRUC_INDEX = L_STRUC_INDEX + 1.
      CLEAR L_LEN_STRING.
      ASSIGN COMPONENT L_STRUC_INDEX OF
               STRUCTURE I_TAB_SAP_DATA TO <F_SOURCE>.
      IF SY-SUBRC <> C_RC0.
        EXIT.
      ELSE.
*       header line versorgen
        IF L_TABIX = 1 AND NOT I_LINE_HEADER IS INITIAL.
          ASSIGN COMPONENT L_STRUC_INDEX OF
                   STRUCTURE I_TAB_SAP_DATA TO <F_SOURCE>.
          DESCRIBE FIELD <F_SOURCE>  HELP-ID L_HELP_ID.
          CONDENSE L_HELP_ID.
          L_LEN_COL_STRING = STRLEN( L_HELP_ID ).
          IF L_LEN_COL_STRING <> C_RC0.
            IF L_STRUC_INDEX <> 1.
              L_STRUC_COL_NAMES+L_START_COL_STRING(L_LEN_COL_STRING) =
                                                   I_FIELD_SEPERATOR.
              L_LEN_COL_STRING = L_LEN_FIELD_SEP.
            ENDIF.
            L_END_COL_STRING = L_START_COL_STRING + L_LEN_COL_STRING.
            IF L_END_COL_STRING > L_EOL_STRING.
              MESSAGE ID 'UX' TYPE 'E' NUMBER 898
                         WITH L_TABIX L_STRUC_INDEX
                              RAISING CONVERSION_FAILED.
            ENDIF.
            L_STRUC_COL_NAMES+L_START_COL_STRING(L_LEN_COL_STRING) =
                                           L_HELP_ID.
            L_START_COL_STRING = L_START_COL_STRING + L_LEN_COL_STRING.
          ENDIF.
        ENDIF.
*       Special processing when field is type DATA
        DESCRIBE FIELD <F_SOURCE> TYPE L_TYPE.
        CASE L_TYPE.
          WHEN 'P'.
            WRITE <F_SOURCE> TO L_DATE_EXTERN.
            CATCH SYSTEM-EXCEPTIONS CONVERSION_ERRORS  = C_RC4.
              IF SY-SUBRC <> C_RC0.
                MESSAGE E899(UX) WITH L_TYPE <F_SOURCE> L_HELP_ID
                                      L_TABIX RAISING CONVERSION_FAILED.
              ENDIF.
            ENDCATCH.
            ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
          WHEN 'I'.       "N1855878
            WRITE <F_SOURCE> TO L_DATE_EXTERN.
            CATCH SYSTEM-EXCEPTIONS CONVERSION_ERRORS  = C_RC4.
              IF SY-SUBRC <> C_RC0.
                MESSAGE E899(UX) WITH L_TYPE <F_SOURCE> L_HELP_ID
                                      L_TABIX RAISING CONVERSION_FAILED.
              ENDIF.
            ENDCATCH.
            ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
          WHEN 'X'.
            ASSIGN COMPONENT L_STRUC_INDEX OF
                    STRUCTURE I_TAB_SAP_DATA TO <F_SOURCE> TYPE 'C'.
            WRITE <F_SOURCE> TO L_DATE_EXTERN.
            ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
          WHEN 'T'.
            CLEAR L_DATE_EXTERN.
            L_DATE_EXTERN(2) = <F_SOURCE>(2).
            L_DATE_EXTERN+2(1) = ':'.
            L_DATE_EXTERN+3(2) = <F_SOURCE>+2(2).
            L_DATE_EXTERN+5(1) = ':'.
            L_DATE_EXTERN+6(2) = <F_SOURCE>+4(2).
            ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
          WHEN 'D'.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                 EXPORTING
                      DATE_INTERNAL = <F_SOURCE>
                 IMPORTING
                      DATE_EXTERNAL = L_DATE_EXTERN
                 EXCEPTIONS
                      OTHERS        = C_RC4.
            IF SY-SUBRC <> C_RC0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                    RAISING CONVERSION_FAILED.
            ELSE.
              ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
            ENDIF.
          WHEN 'F'.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION'
                 EXPORTING
                      INPUT  = <F_SOURCE>
                 IMPORTING
                      FLSTR  = L_DATE_EXTERN
                 EXCEPTIONS
                      OTHERS = C_RC4.
            IF SY-SUBRC <> C_RC0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                    RAISING CONVERSION_FAILED.
            ELSE.
              ASSIGN L_DATE_EXTERN TO <F_SOURCE>.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
*       remember der seperation character!
        IF L_STRUC_INDEX <> 1.
          L_MAX_FIELD+L_LEN_STRING(L_LEN_FIELD_SEP) = I_FIELD_SEPERATOR.
          L_LEN_STRING = L_LEN_FIELD_SEP.
        ENDIF.
        IF <F_SOURCE> CO C_DARL_NUMBER.
          CONDENSE <F_SOURCE> NO-GAPS.
        ELSE.
          CONDENSE <F_SOURCE>.
        ENDIF.
        L_MAX_FIELD+L_LEN_STRING = <F_SOURCE>.
*       check the remaining space of target structure
        L_LEN_STRING = STRLEN( L_MAX_FIELD ).
        CHECK L_LEN_STRING <> C_RC0.
        L_END_STRING = L_START_STRING + L_LEN_STRING.
        IF L_END_STRING > L_EOL_STRING.
          MESSAGE ID 'UX' TYPE 'E' NUMBER 898
                     WITH L_TABIX L_STRUC_INDEX
                          RAISING CONVERSION_FAILED.
        ENDIF.
        L_STRUC_RAW_DATA+L_START_STRING(L_LEN_STRING) = L_MAX_FIELD.
        L_START_STRING = L_START_STRING + L_LEN_STRING.
      ENDIF.
    ENDDO.
    IF L_TABIX = 1 AND NOT L_STRUC_COL_NAMES IS INITIAL.
      APPEND L_STRUC_COL_NAMES TO I_TAB_CONVERTED_DATA.
      CLEAR L_STRUC_COL_NAMES.
    ENDIF.
    IF NOT L_STRUC_RAW_DATA IS INITIAL.
      APPEND L_STRUC_RAW_DATA TO I_TAB_CONVERTED_DATA.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
