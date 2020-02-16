FUNCTION LOAN_CHECK_STRUCTURE_COMPLETE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_STRUCTURE_TABNAME) LIKE  DD02L-TABNAME
*"             VALUE(I_STRUCTURE)
*"             VALUE(I_TABIX) LIKE  SY-TABIX DEFAULT 1
*"       TABLES
*"              T_ERROR_TABLE STRUCTURE  SPROT_X OPTIONAL
*"              T_ERROR_FIELDS TYPE  TRUXS_ERROR_FIELDS OPTIONAL
*"              T_IGNORE_FIELDS TYPE  TRUXS_ERROR_FIELDS OPTIONAL
*"              T_REQUIRED_FIELDS STRUCTURE  DFIES OPTIONAL
*"       EXCEPTIONS
*"              ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_I_DFIES LIKE I_DFIES OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF IT_BUFF_VALUES OCCURS 100.
  INCLUDE TYPE TRUXS_STRUC_BUFFERED_VALUES.
  DATA: END OF IT_BUFF_VALUES,
        TMP_BUFF_VALUES TYPE TRUXS_BUFFERED_VALUES.

  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
       EXPORTING
            I_STRUCTURE_TABNAME = I_STRUCTURE_TABNAME
            I_KEYS              = C_TRUE
       IMPORTING
            E_BUFFERED_VALUES   = TMP_BUFF_VALUES
       TABLES
            IT_DFIES            = TMP_I_DFIES
            T_IGNORE_FIELDS     = T_IGNORE_FIELDS
       EXCEPTIONS
            OTHERS              = C_RC4.
  IF SY-SUBRC <> C_RC0.
    RAISE ERROR_FOUND.
  ENDIF.
  IT_BUFF_VALUES[] = TMP_BUFF_VALUES[].
* Start with check of structure

  SORT IT_BUFF_VALUES BY TABNAME FIELDNAME FIELDVALUES DOMVALUE_L.
  LOOP AT TMP_I_DFIES WHERE TABNAME = I_STRUCTURE_TABNAME.
    IF T_IGNORE_FIELDS IS REQUESTED.
      READ TABLE T_IGNORE_FIELDS WITH KEY
                                      TABNAME   = TMP_I_DFIES-TABNAME
                                      FIELDNAME = TMP_I_DFIES-FIELDNAME
                                      BINARY SEARCH.
      IF SY-SUBRC = C_RC0.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF T_REQUIRED_FIELDS IS REQUESTED.
      READ TABLE T_REQUIRED_FIELDS WITH KEY
                                   FIELDNAME = TMP_I_DFIES-FIELDNAME.
      IF SY-SUBRC = C_RC0.
        ASSIGN COMPONENT TMP_I_DFIES-FIELDNAME OF
               STRUCTURE I_STRUCTURE TO <F1>.
        IF SY-SUBRC = C_RC0.
          IF <F1> IS INITIAL.
            PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                    USING    C_UX C_ERROR C_275 I_TABIX
                                             TMP_I_DFIES-FIELDNAME
                                             C_SPACE C_SPACE C_SPACE
                                    CHANGING SY-SUBRC.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    READ TABLE IT_BUFF_VALUES WITH KEY
                        TABNAME     = TMP_I_DFIES-TABNAME
                        FIELDNAME   = TMP_I_DFIES-FIELDNAME
                        BINARY SEARCH.
    CHECK SY-SUBRC = C_RC0.
    ASSIGN COMPONENT it_buff_values-position of structure i_structure
              TO <F1> TYPE IT_BUFF_VALUES-INTTYPE.
    IF NOT <F1> IS INITIAL.
      IF NOT IT_BUFF_VALUES-FIELDVALUES IS INITIAL.
*       check foreign key value
        READ TABLE IT_BUFF_VALUES WITH KEY
                            TABNAME     = TMP_I_DFIES-TABNAME
                            FIELDNAME   = TMP_I_DFIES-FIELDNAME
                            FIELDVALUES = <F1> BINARY SEARCH.
        IF SY-SUBRC <> C_RC0 AND T_ERROR_FIELDS IS REQUESTED.
          T_ERROR_FIELDS-TABNAME    = TMP_I_DFIES-TABNAME.
          T_ERROR_FIELDS-FIELDNAME  = TMP_I_DFIES-FIELDNAME.
          T_ERROR_FIELDS-FIELDVALUE = <F1>.
          CONDENSE T_ERROR_FIELDS-FIELDVALUE.
          APPEND T_ERROR_FIELDS.
        ENDIF.
        IF SY-SUBRC <> C_RC0 AND T_ERROR_TABLE IS REQUESTED.
          PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                  USING    C_UX C_ERROR C_276 I_TABIX
                                           TMP_I_DFIES-FIELDNAME
                                           TMP_I_DFIES-TABNAME
                                           <F1>
                                           C_SPACE
                                  CHANGING SY-SUBRC.
          READ TABLE IT_BUFF_VALUES WITH KEY
                              TABNAME     = TMP_I_DFIES-TABNAME
                              FIELDNAME   = TMP_I_DFIES-FIELDNAME
                              BINARY SEARCH.
          IF SY-SUBRC = C_RC0.
            PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                    USING    C_67 C_ERROR C_102 I_TABIX
                                             IT_BUFF_VALUES-CHECKTABLE
                                             C_SPACE C_SPACE C_SPACE
                                    CHANGING SY-SUBRC.
          ELSE.
            PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                    USING    C_67 C_ERROR C_102 I_TABIX
                                             TMP_I_DFIES-TABNAME
                                             C_SPACE C_SPACE C_SPACE
                                    CHANGING SY-SUBRC.
          ENDIF.
          PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                  USING    C_67 C_ERROR C_506 I_TABIX
                                           <F1> C_SPACE
                                           C_SPACE C_SPACE
                                  CHANGING SY-SUBRC.
        ENDIF.
      ENDIF.
*     check domain values
      IF NOT ( IT_BUFF_VALUES-DOMVALUE_L IS INITIAL AND
               IT_BUFF_VALUES-DOMVALUE_H IS INITIAL ).
        CALL FUNCTION 'LOAN_DOMAEN_MANAGER'
             EXPORTING
                  I_DOMNAME    = IT_BUFF_VALUES-DOMNAME
                  I_DOMVALUE_L = <F1>
             EXCEPTIONS
                  OTHERS       = C_RC4.
        IF SY-SUBRC <> C_RC0 AND T_ERROR_TABLE IS REQUESTED.
          PERFORM FILL_ERROR_ITAB TABLES   T_ERROR_TABLE
                                  USING    C_UX C_ERROR C_383 I_TABIX
                                           TMP_I_DFIES-FIELDNAME <F1>
                                           TMP_I_DFIES-DOMNAME C_SPACE
                                  CHANGING SY-SUBRC.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
